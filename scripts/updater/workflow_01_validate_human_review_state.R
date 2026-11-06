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
repairs_path <- arg("--repairs")
output_path <- arg("--output")
expected_queue_sha <- arg("--expected-queue-sha256",NULL)
require_complete <- tolower(arg("--require-complete","true")) %in% c("true","1","yes")
if(any(vapply(list(queue_path,decisions_path,repairs_path,output_path),is.null,logical(1)))) stop("Required: --queue --decisions --repairs --output",call.=FALSE)

read_jsonl <- function(path) {
  if(!file.exists(path)) stop(sprintf("File not found: %s",path),call.=FALSE)
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
queue <- read_jsonl(queue_path)
decisions <- read_jsonl(decisions_path)
repairs <- read_jsonl(repairs_path)
queue_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
if(!is.null(expected_queue_sha)&&!identical(expected_queue_sha,queue_sha)) stop("Queue SHA does not match locked expected SHA",call.=FALSE)

ids <- vapply(queue,function(z)as.character(z$review_case_id),character(1))
if(anyDuplicated(ids)) stop("Queue contains duplicate review_case_id",call.=FALSE)
qmap <- setNames(queue,ids)
dids <- vapply(decisions,function(z)as.character(z$review_case_id),character(1))
if(anyDuplicated(dids)) stop("Active decisions contain duplicate review_case_id",call.=FALSE)
unknown <- setdiff(dids,ids)
if(length(unknown)) stop(sprintf("Active decisions contain %d unknown case IDs",length(unknown)),call.=FALSE)
valid <- c("duplicate","not_duplicate","uncertain")
for(d in decisions) {
  if(is.null(d$queue_sha256)||!identical(as.character(d$queue_sha256),queue_sha)) stop(sprintf("Decision %s missing/wrong queue_sha256",d$review_case_id),call.=FALSE)
  if(is.null(d$decision)||!(d$decision %in% valid)) stop(sprintf("Decision %s invalid",d$review_case_id),call.=FALSE)
  for(field in c("rationale","reviewer","resolved_at_utc")) if(is.null(d[[field]])||!nzchar(trimws(as.character(d[[field]])))) stop(sprintf("Decision %s missing %s",d$review_case_id,field),call.=FALSE)
}
missing <- setdiff(ids,dids)
uncertain <- dids[vapply(decisions,function(z)identical(z$decision,"uncertain"),logical(1))]
if(require_complete&&length(missing)) stop(sprintf("%d cases remain unresolved",length(missing)),call.=FALSE)
if(require_complete&&length(uncertain)) stop(sprintf("%d cases remain uncertain",length(uncertain)),call.=FALSE)

allowed_actions <- c("strip_abstract","replace_abstract","set_doi","set_title","set_canonical_preference")
repair_keys <- character()
for(r in repairs) {
  id <- as.character(r$review_case_id)
  q <- qmap[[id]]
  if(is.null(q)) stop(sprintf("Repair references unknown case %s",id),call.=FALSE)
  if(is.null(r$queue_sha256)||!identical(as.character(r$queue_sha256),queue_sha)) stop(sprintf("Repair %s missing/wrong queue_sha256",id),call.=FALSE)
  if(is.null(r$source_record_id)||!nzchar(trimws(as.character(r$source_record_id)))) stop(sprintf("Repair %s missing source_record_id",id),call.=FALSE)
  if(is.null(r$source)||!nzchar(trimws(as.character(r$source)))) stop(sprintf("Repair %s missing source",id),call.=FALSE)
  if(is.null(r$action)||!(r$action %in% allowed_actions)) stop(sprintf("Repair %s invalid action",id),call.=FALSE)
  members <- c(as.character(q$record_i$source_record_id),as.character(q$record_j$source_record_id))
  sources <- c(as.character(q$record_i$source),as.character(q$record_j$source))
  pos <- match(as.character(r$source_record_id),members)
  if(is.na(pos)) stop(sprintf("Repair %s targets source_record_id outside its pair",id),call.=FALSE)
  if(!identical(as.character(r$source),sources[[pos]])) stop(sprintf("Repair %s source/source_record_id mismatch",id),call.=FALSE)
  if(r$action %in% c("replace_abstract","set_doi","set_title","set_canonical_preference") && (is.null(r$value)||!nzchar(trimws(as.character(r$value))))) stop(sprintf("Repair %s action %s requires value",id,r$action),call.=FALSE)
  for(field in c("reason","recorded_by","recorded_at_utc")) if(is.null(r[[field]])||!nzchar(trimws(as.character(r[[field]])))) stop(sprintf("Repair %s missing %s",id,field),call.=FALSE)
  key <- paste(id,r$source_record_id,r$action,sep="|")
  if(key %in% repair_keys) stop(sprintf("Duplicate active repair key: %s",key),call.=FALSE)
  repair_keys <- c(repair_keys,key)
}

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
manifest <- list(
  schema="living-evidence-map-workflow01-human-review-state-integrity-v1",
  status=if(length(missing)==0L&&length(uncertain)==0L)"PASS" else "INCOMPLETE",
  queue_sha256=queue_sha,
  queue_cases=length(queue),
  active_decisions=length(decisions),
  duplicate_decision_ids=0,
  unknown_decision_ids=0,
  unresolved_cases=length(missing),
  uncertain_cases=length(uncertain),
  repairs=length(repairs),
  invalid_repair_targets=0,
  decisions_sha256=digest(file=decisions_path,algo="sha256",serialize=FALSE),
  repairs_sha256=digest(file=repairs_path,algo="sha256",serialize=FALSE),
  resume_allowed=(length(missing)==0L&&length(uncertain)==0L)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),output_path,useBytes=TRUE)
cat(sprintf("PASS: state integrity verified: %d/%d decisions, %d repairs, queue SHA %s\n",length(decisions),length(queue),length(repairs),queue_sha))
