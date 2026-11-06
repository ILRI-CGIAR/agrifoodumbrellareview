#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

queue_path <- arg("--queue")
decisions_path <- arg("--decisions")
output_path <- arg("--output")
require_complete <- tolower(arg("--require-complete","true")) %in% c("true","1","yes")
if (any(vapply(list(queue_path,decisions_path,output_path),is.null,logical(1)))) {
  stop("Required: --queue --decisions --output",call.=FALSE)
}

read_jsonl <- function(path) {
  if (!file.exists(path)) stop(sprintf("File not found: %s",path),call.=FALSE)
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
queue <- read_jsonl(queue_path)
decisions <- read_jsonl(decisions_path)
queue_ids <- vapply(queue,function(x)as.character(x$review_case_id),character(1))
decision_ids <- vapply(decisions,function(x)as.character(x$review_case_id),character(1))
if (anyDuplicated(queue_ids)) stop("Queue contains duplicate review_case_id",call.=FALSE)
if (anyDuplicated(decision_ids)) stop("Human decisions contain duplicate review_case_id",call.=FALSE)
unknown <- setdiff(decision_ids,queue_ids)
if (length(unknown)) stop(sprintf("Human decisions contain %d unknown case IDs",length(unknown)),call.=FALSE)

valid <- c("duplicate","not_duplicate","uncertain")
for (d in decisions) {
  if (is.null(d$decision) || !(d$decision %in% valid)) stop("Invalid human decision",call.=FALSE)
  if (is.null(d$rationale) || !nzchar(trimws(as.character(d$rationale)))) stop("Human rationale is required",call.=FALSE)
  if (is.null(d$reviewer) || !nzchar(trimws(as.character(d$reviewer)))) stop("Reviewer is required",call.=FALSE)
  if (is.null(d$resolved_at_utc) || !nzchar(trimws(as.character(d$resolved_at_utc)))) stop("resolved_at_utc is required",call.=FALSE)
}

missing <- setdiff(queue_ids,decision_ids)
uncertain <- decision_ids[vapply(decisions,function(x)identical(x$decision,"uncertain"),logical(1))]
if (require_complete && length(missing)) stop(sprintf("%d human-review cases remain unresolved",length(missing)),call.=FALSE)
if (require_complete && length(uncertain)) stop(sprintf("%d human-review cases remain uncertain",length(uncertain)),call.=FALSE)

by_id <- setNames(decisions,decision_ids)
resolved <- vector("list",length(queue))
for (i in seq_along(queue)) {
  q <- queue[[i]]
  d <- by_id[[q$review_case_id]]
  if (is.null(d)) {
    q$human_status <- "pending"
  } else {
    q$human_status <- if (identical(d$decision,"uncertain")) "uncertain" else "resolved"
    q$human_decision <- d$decision
    q$human_rationale <- d$rationale
    q$human_reviewer <- d$reviewer
    q$human_resolved_at_utc <- d$resolved_at_utc
  }
  resolved[[i]] <- q
}

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
con <- file(output_path,"wt",encoding="UTF-8")
for (z in resolved) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
close(con)

manifest <- list(
  schema="living-evidence-map-workflow01-human-decision-validation-v1",
  queue_cases=length(queue),
  supplied_decisions=length(decisions),
  unresolved_cases=length(missing),
  uncertain_cases=length(uncertain),
  complete=(length(missing)==0L && length(uncertain)==0L),
  queue_sha256=digest(file=queue_path,algo="sha256",serialize=FALSE),
  decisions_sha256=digest(file=decisions_path,algo="sha256",serialize=FALSE),
  resolved_sha256=digest(file=output_path,algo="sha256",serialize=FALSE)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           paste0(output_path,".manifest.json"))
cat(sprintf("PASS: validated %d/%d human decisions; unresolved=%d uncertain=%d\n",
            length(decisions),length(queue),length(missing),length(uncertain)))
