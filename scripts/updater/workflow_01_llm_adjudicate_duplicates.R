#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_path <- arg("--input")
output_path <- arg("--output")
human_path <- arg("--human-review-output")
model <- arg("--model",Sys.getenv("OPENAI_DUPLICATE_MODEL","gpt-5.6-luna"))
threshold <- as.numeric(arg("--auto-threshold","0.95"))
start_row <- as.integer(arg("--start-row","1"))
end_arg <- arg("--end-row",NULL)
if (any(vapply(list(input_path,output_path,human_path),is.null,logical(1)))) {
  stop("Required: --input --output --human-review-output",call.=FALSE)
}
if (!is.finite(threshold) || threshold < 0 || threshold > 1) stop("Invalid --auto-threshold",call.=FALSE)
token <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(token)) stop("OPENAI_API_KEY is required",call.=FALSE)

lines <- readLines(input_path,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
full_n <- length(lines)
end_row <- if (is.null(end_arg)) full_n else as.integer(end_arg)
if (is.na(start_row)||is.na(end_row)||start_row<1L||end_row<start_row||end_row>full_n) {
  stop("Invalid adjudication row range",call.=FALSE)
}
lines <- lines[start_row:end_row]

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
dir.create(dirname(human_path),recursive=TRUE,showWarnings=FALSE)
out <- file(output_path,"wt",encoding="UTF-8")
human <- file(human_path,"wt",encoding="UTF-8")
on.exit({try(close(out),silent=TRUE);try(close(human),silent=TRUE)},add=TRUE)

schema <- list(
  type="object",
  additionalProperties=FALSE,
  properties=list(
    decision=list(type="string",enum=list("duplicate","not_duplicate","uncertain")),
    confidence=list(type="number",minimum=0,maximum=1),
    rationale=list(type="string"),
    abstract_consistent_with_record_i=list(type="boolean"),
    abstract_consistent_with_record_j=list(type="boolean")
  ),
  required=c("decision","confidence","rationale","abstract_consistent_with_record_i","abstract_consistent_with_record_j")
)

system_prompt <- paste(
  "You adjudicate whether two bibliographic records are manifestations of the same scholarly work.",
  "Use publication identity evidence, not topical similarity.",
  "A DOI is supporting evidence but may be wrong and is never decisive by itself.",
  "Preprint-to-journal, early-online-to-final, conference-abstract-to-full-paper, and database-source duplicates may legitimately differ in DOI, title wording, pagination or journal metadata.",
  "Different experiments, parts, reports, years, volumes, issues, numbered supplements, or genuinely different publications are not duplicates merely because abstracts or topics are similar.",
  "CRITICAL DISTINCTION: the same underlying study, experiment, dataset, trial, cohort, farm, sampling campaign or research project can produce multiple distinct publications. Shared study identity is not sufficient for a duplicate decision. Classify as duplicate only when the two records are manifestations of the same publication/work, not merely outputs from the same underlying study.",
  "Companion papers, secondary analyses, follow-up papers, methods papers, protocol papers, conference outputs and full papers should be treated as distinct publications unless the evidence shows they are versions/manifestations of the same work.",
  "Use title, abstract, keywords, authors, journal/source, year, volume, issue, pages and identifiers together.",
  "CRITICAL ABSTRACT-INTEGRITY CHECK: when abstracts are identical or near-identical, independently ask whether that abstract is semantically consistent with record i's title and whether it is semantically consistent with record j's title. A shared abstract that fits only one title is evidence of metadata contamination, not evidence that the records are duplicate publications.",
  "For distinct publications from the same study, one shared or copied abstract must not override different publication identities.",
  "Set abstract_consistent_with_record_i and abstract_consistent_with_record_j explicitly. If either is false, do not return a high-confidence duplicate solely from the shared abstract.",
  "Return uncertain whenever identity is not clear enough to resolve safely.",
  "Return JSON only under the supplied schema.",
  sep="\n"
)

call_model <- function(case) {
  body <- list(
    model=model,
    store=FALSE,
    reasoning=list(effort="low"),
    input=list(
      list(role="system",content=list(list(type="input_text",text=system_prompt))),
      list(role="user",content=list(list(type="input_text",text=toJSON(case,auto_unbox=TRUE,null="null",na="null"))))
    ),
    text=list(
      verbosity="low",
      format=list(type="json_schema",name="duplicate_adjudication",strict=TRUE,schema=schema)
    )
  )
  resp <- request("https://api.openai.com/v1/responses") |>
    req_auth_bearer_token(token) |>
    req_body_json(body,auto_unbox=TRUE) |>
    req_timeout(180) |>
    req_retry(max_tries=4,backoff=~2^.x) |>
    req_perform() |>
    resp_body_json(simplifyVector=FALSE)

  messages <- Filter(function(x)is.list(x)&&identical(x$type,"message"),resp$output)
  text_items <- unlist(lapply(messages,function(x)Filter(
    function(y)is.list(y)&&identical(y$type,"output_text")&&!is.null(y$text),x$content
  )),recursive=FALSE)
  if (!length(text_items)) stop("No output_text returned",call.=FALSE)
  parsed <- fromJSON(text_items[[1L]]$text,simplifyVector=FALSE)
  if (is.null(parsed$decision)||is.null(parsed$confidence)||is.null(parsed$rationale)||
      is.null(parsed$abstract_consistent_with_record_i)||is.null(parsed$abstract_consistent_with_record_j)) stop("Incomplete model result",call.=FALSE)
  if (!(parsed$decision %in% c("duplicate","not_duplicate","uncertain"))) stop("Invalid model decision",call.=FALSE)
  conf <- as.numeric(parsed$confidence)
  if (!is.finite(conf)||conf<0||conf>1) stop("Invalid model confidence",call.=FALSE)
  rat <- as.character(parsed$rationale)
  if (!length(rat)||!nzchar(trimws(rat))) stop("Empty model rationale",call.=FALSE)
  list(
    decision=parsed$decision,
    confidence=conf,
    rationale=rat,
    abstract_consistent_with_record_i=isTRUE(parsed$abstract_consistent_with_record_i),
    abstract_consistent_with_record_j=isTRUE(parsed$abstract_consistent_with_record_j),
    response_id=if(is.null(resp$id)) NULL else as.character(resp$id),
    resolved_model=if(is.null(resp$model)) model else as.character(resp$model),
    usage=resp$usage
  )
}

human_n <- 0L
auto_n <- 0L
technical_n <- 0L
for (n in seq_along(lines)) {
  case <- fromJSON(lines[[n]],simplifyVector=FALSE)
  started <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
  result <- tryCatch(call_model(case),error=function(e) {
    technical_n <<- technical_n + 1L
    list(decision="uncertain",confidence=0,rationale="Model adjudication failed; human review required.",
         response_id=NULL,resolved_model=model,usage=NULL,technical_error=conditionMessage(e))
  })
  technical_error <- if(is.null(result$technical_error)) NULL else result$technical_error
  exact_abstract_rule <- identical(case$deterministic_evidence$classifier_rule,"exact_abstract_insufficient_metadata")
  abstract_title_inconsistency_guard <- exact_abstract_rule &&
    (!isTRUE(result$abstract_consistent_with_record_i) || !isTRUE(result$abstract_consistent_with_record_j))
  title_i_missing <- is.null(case$record_i$title) || !nzchar(trimws(as.character(case$record_i$title)))
  title_j_missing <- is.null(case$record_j$title) || !nzchar(trimws(as.character(case$record_j$title)))
  exact_abstract_missing_title_nd_guard <- exact_abstract_rule &&
    identical(result$decision,"not_duplicate") &&
    (title_i_missing || title_j_missing)

  promotion <- if (!is.null(technical_error)) "human_review" else if (
    identical(result$decision,"uncertain") || result$confidence < threshold ||
      abstract_title_inconsistency_guard || exact_abstract_missing_title_nd_guard
  ) "human_review" else result$decision
  promotion_reason <- if (!is.null(technical_error)) "technical_failure" else if (
    identical(result$decision,"uncertain")
  ) "model_uncertain" else if (result$confidence < threshold) "below_auto_threshold" else if (
    abstract_title_inconsistency_guard
  ) "abstract_title_inconsistency_guard" else if (
    exact_abstract_missing_title_nd_guard
  ) "exact_abstract_missing_title_not_duplicate_guard" else "high_confidence_model_decision"

  rec <- c(case,list(
    workflow="01_duplicate_llm_adjudication",
    requested_model=model,
    resolved_model=result$resolved_model,
    response_id=result$response_id,
    api_usage=result$usage,
    model_decision=result$decision,
    model_confidence=result$confidence,
    model_rationale=result$rationale,
    abstract_consistent_with_record_i=result$abstract_consistent_with_record_i,
    abstract_consistent_with_record_j=result$abstract_consistent_with_record_j,
    technical_error=technical_error,
    auto_threshold=threshold,
    promotion=promotion,
    promotion_reason=promotion_reason,
    adjudicated_at_utc=started
  ))
  line <- toJSON(rec,auto_unbox=TRUE,null="null",na="null")
  writeLines(line,out,useBytes=TRUE)
  if (identical(promotion,"human_review")) {
    human_n <- human_n + 1L
    writeLines(line,human,useBytes=TRUE)
  } else auto_n <- auto_n + 1L

  if (n==1L || n%%25L==0L || n==length(lines)) {
    cat(sprintf("LLM adjudication %d/%d in chunk; automatic=%d human=%d technical_failures=%d\n",
                n,length(lines),auto_n,human_n,technical_n))
    flush.console()
  }
}
close(out); close(human)
on.exit(NULL,add=FALSE)

usage_lines <- readLines(output_path,warn=FALSE,encoding="UTF-8")
usage_records <- lapply(usage_lines[nzchar(trimws(usage_lines))],fromJSON,simplifyVector=FALSE)
usage_num <- function(z,name) {
  if (is.null(z$api_usage) || is.null(z$api_usage[[name]])) return(0)
  v <- suppressWarnings(as.numeric(z$api_usage[[name]]))
  if (!length(v) || is.na(v)) 0 else v
}
detail_num <- function(z,group,name) {
  if (is.null(z$api_usage) || is.null(z$api_usage[[group]]) || is.null(z$api_usage[[group]][[name]])) return(0)
  v <- suppressWarnings(as.numeric(z$api_usage[[group]][[name]]))
  if (!length(v) || is.na(v)) 0 else v
}
usage_summary <- list(
  input_tokens=sum(vapply(usage_records,usage_num,numeric(1),name="input_tokens")),
  output_tokens=sum(vapply(usage_records,usage_num,numeric(1),name="output_tokens")),
  total_tokens=sum(vapply(usage_records,usage_num,numeric(1),name="total_tokens")),
  cached_input_tokens=sum(vapply(usage_records,detail_num,numeric(1),group="input_tokens_details",name="cached_tokens")),
  reasoning_output_tokens=sum(vapply(usage_records,detail_num,numeric(1),group="output_tokens_details",name="reasoning_tokens"))
)

manifest <- list(
  schema="living-evidence-map-workflow01-llm-adjudication-manifest-v1",
  requested_model=model,
  auto_threshold=threshold,
  source_case_count=full_n,
  start_row=start_row,
  end_row=end_row,
  processed_cases=length(lines),
  automatic_cases=auto_n,
  human_review_cases=human_n,
  technical_failures=technical_n,
  api_usage=usage_summary,
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  output_sha256=digest(file=output_path,algo="sha256",serialize=FALSE),
  human_review_sha256=digest(file=human_path,algo="sha256",serialize=FALSE)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           paste0(output_path,".manifest.json"))
cat(sprintf("PASS: LLM adjudicated %d cases: %d automatic, %d require human review\n",
            length(lines),auto_n,human_n))
