#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call.=FALSE)
  args[[i+1L]]
}
`%||%` <- function(x,y) if (is.null(x)) y else x

queue_path <- arg("--queue")
decisions_path <- arg("--decisions")
current_patch_path <- arg("--current-patch")
output_patch_path <- arg("--output-patch")
manifest_path <- arg("--manifest")
expected_sha <- arg("--expected-queue-sha256")

req <- list(queue_path, decisions_path, current_patch_path, output_patch_path, manifest_path, expected_sha)
if (any(vapply(req, is.null, logical(1)))) {
  stop("Required: --queue --decisions --current-patch --output-patch --manifest --expected-queue-sha256", call.=FALSE)
}

readjl <- function(p) {
  if (!file.exists(p)) stop("Missing file: ", p, call.=FALSE)
  x <- readLines(p, warn=FALSE, encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(seq_along(x), function(i) {
    tryCatch(fromJSON(x[[i]], simplifyVector=FALSE),
             error=function(e) stop(sprintf("Invalid JSONL %s line %d: %s", p, i, conditionMessage(e)), call.=FALSE))
  })
}
writejl <- function(xs,p) {
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  con <- file(p,"wt",encoding="UTF-8"); on.exit(close(con),add=TRUE)
  if(length(xs)) for(x in xs) writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
}
clean <- function(x) {
  if (is.null(x) || !length(x)) return("")
  s <- trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])))
  if (is.na(s)) "" else s
}
value_missing <- function(field, x) {
  if (identical(field,"author_keywords")) {
    if (is.null(x) || !length(x)) return(TRUE)
    vals <- trimws(as.character(unlist(x,use.names=FALSE)))
    return(!length(vals[!is.na(vals)&nzchar(vals)]))
  }
  !nzchar(clean(x))
}

actual_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
if (!identical(actual_sha, expected_sha)) {
  stop(sprintf("Queue SHA mismatch: expected=%s actual=%s", expected_sha, actual_sha), call.=FALSE)
}

queue <- readjl(queue_path)
decisions <- readjl(decisions_path)
patch <- readjl(current_patch_path)

if (!length(queue)) stop("W02 queue is empty", call.=FALSE)

qids <- vapply(queue,function(x) clean(x$review_case_id),character(1))
if(any(!nzchar(qids)) || anyDuplicated(qids)) stop("W02 queue has missing/duplicate review_case_id",call.=FALSE)
schemas <- vapply(queue,function(x) clean(x$schema),character(1))
if(any(schemas!="living-evidence-map-workflow02-shiny-case-v1")) stop("Unsupported W02 queue schema",call.=FALSE)

dids <- vapply(decisions,function(x) clean(x$review_case_id),character(1))
if(any(!nzchar(dids)) || anyDuplicated(dids)) stop("W02 decisions have missing/duplicate review_case_id",call.=FALSE)
if(any(!dids %in% qids)) stop("W02 decisions reference case(s) outside frozen queue",call.=FALSE)

dmap <- setNames(decisions,dids)
missing_decisions <- setdiff(qids,dids)
if(length(missing_decisions)) stop(sprintf("W02 adjudication incomplete: %d case(s) have no decision",length(missing_decisions)),call.=FALSE)

allowed_field <- c("accept_provider_field","reject_provider_field","uncertain")
allowed_doi <- c("reject_provider_match","uncertain")

for (q in queue) {
  id <- clean(q$review_case_id)
  d <- dmap[[id]]
  if (!identical(clean(d$queue_sha256), expected_sha)) stop("Decision queue SHA mismatch for ",id,call.=FALSE)
  if (!identical(clean(d$record_id), clean(q$record_id))) stop("Decision record_id mismatch for ",id,call.=FALSE)
  if (!identical(clean(d$provider), clean(q$provider))) stop("Decision provider mismatch for ",id,call.=FALSE)
  if (!identical(clean(d$field), clean(q$field))) stop("Decision field mismatch for ",id,call.=FALSE)
  if (!identical(clean(d$reason), clean(q$reason))) stop("Decision reason mismatch for ",id,call.=FALSE)

  reason <- clean(q$reason)
  decision <- clean(d$decision)
  if (identical(reason,"returned_doi_mismatch")) {
    if(!decision %in% allowed_doi) stop("Invalid returned DOI mismatch decision for ",id,call.=FALSE)
  } else {
    if(!decision %in% allowed_field) stop("Invalid field-level W02 decision for ",id,call.=FALSE)
  }
  if (identical(decision,"uncertain")) {
    stop("W02 adjudication incomplete: uncertain decision remains for ",id,call.=FALSE)
  }
}

patch_ids <- vapply(patch,function(x) clean(x$record_id),character(1))
if(any(!nzchar(patch_ids)) || anyDuplicated(patch_ids)) stop("Current W02 patch has missing/duplicate record_id",call.=FALSE)
store <- setNames(patch,patch_ids)

accepted <- 0L
rejected <- 0L
for (q in queue) {
  id <- clean(q$review_case_id)
  d <- dmap[[id]]
  decision <- clean(d$decision)
  rid <- clean(q$record_id)

  if (decision %in% c("reject_provider_field","reject_provider_match")) {
    rejected <- rejected + 1L
    next
  }
  if (!identical(decision,"accept_provider_field")) stop("Unhandled W02 decision: ",decision,call.=FALSE)

  field <- clean(q$field)
  if(!field %in% c("title","abstract","author_keywords")) stop("Unsupported accepted W02 field: ",field,call.=FALSE)
  pr <- q$provider_response %||% list()
  value <- pr[[field]]
  if (value_missing(field,value)) stop(sprintf("Accepted provider field is empty: %s %s",id,field),call.=FALSE)

  p <- store[[rid]]
  if (is.null(p)) {
    p <- list(
      record_id=rid,
      input_doi=clean(q$doi),
      title=NULL,
      abstract=NULL,
      author_keywords=NULL,
      metadata_enrichment=list(),
      audit=list()
    )
  }

  existing <- p[[field]]
  if (!is.null(existing)) {
    existing_value <- existing$value
    if (!identical(existing_value,value)) {
      stop(sprintf("Human-approved field conflicts with existing W02 patch for %s %s",rid,field),call.=FALSE)
    }
  } else {
    p[[field]] <- list(
      value=value,
      provider=clean(q$provider),
      human_adjudicated=TRUE,
      review_case_id=id
    )
  }

  prior_meta <- p$metadata_enrichment %||% list()
  prior_meta$human_adjudication <- c(
    prior_meta$human_adjudication %||% list(),
    list(list(
      review_case_id=id,
      provider=clean(q$provider),
      field=field,
      reason=clean(q$reason),
      decision=decision,
      reviewer=clean(d$reviewer),
      resolved_at_utc=clean(d$resolved_at_utc),
      queue_sha256=expected_sha
    ))
  )
  p$metadata_enrichment <- prior_meta

  prior_audit <- p$audit %||% list()
  prior_audit$human_adjudication <- c(
    prior_audit$human_adjudication %||% list(),
    list(list(
      review_case_id=id,
      conflict=q$conflict %||% list(),
      decision=decision
    ))
  )
  p$audit <- prior_audit

  store[[rid]] <- p
  accepted <- accepted + 1L
}

ids <- sort(names(store))
out <- unname(store[ids])
writejl(out,output_patch_path)

manifest <- list(
  schema="living-evidence-map-workflow02-human-adjudication-application-v1",
  status="PASS",
  queue_sha256=expected_sha,
  queue_cases=length(queue),
  decisions=length(decisions),
  accepted_provider_fields=accepted,
  rejected_conflicts=rejected,
  input_current_patch_sha256=digest(file=current_patch_path,algo="sha256",serialize=FALSE),
  output_patch_sha256=digest(file=output_patch_path,algo="sha256",serialize=FALSE),
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
dir.create(dirname(manifest_path),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),manifest_path,useBytes=TRUE)

cat(sprintf(
  "PASS: applied W02 human decisions: %d accepted provider fields; %d rejected conflicts; %d patch records\n",
  accepted,rejected,length(out)
))
