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
manifest_path <- arg("--batch-manifest")
decisions_path <- arg("--decisions")
repairs_path <- arg("--repairs",NULL)
output_dir <- arg("--output-dir")
expected_queue_sha <- arg("--expected-queue-sha256",NULL)
if(any(vapply(list(queue_path,manifest_path,decisions_path,output_dir),is.null,logical(1)))) {
  stop("Required: --queue --batch-manifest --decisions --output-dir",call.=FALSE)
}

read_jsonl <- function(path) {
  if(!file.exists(path)) stop(sprintf("File not found: %s",path),call.=FALSE)
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
write_jsonl <- function(x,path) {
  con <- file(path,"wt",encoding="UTF-8")
  on.exit(close(con),add=TRUE)
  for(z in x) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
}

queue <- read_jsonl(queue_path)
manifest <- fromJSON(manifest_path,simplifyVector=FALSE)
decisions <- read_jsonl(decisions_path)
repairs <- if(!is.null(repairs_path) && file.exists(repairs_path)) read_jsonl(repairs_path) else list()

queue_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
if(is.null(manifest$queue_sha256)||!identical(as.character(manifest$queue_sha256),queue_sha)) stop("Batch manifest queue SHA does not match queue file",call.=FALSE)
if(!is.null(expected_queue_sha)&&!identical(expected_queue_sha,queue_sha)) stop("Queue SHA does not match --expected-queue-sha256",call.=FALSE)

queue_ids <- vapply(queue,function(z)as.character(z$review_case_id),character(1))
if(anyDuplicated(queue_ids)) stop("Queue contains duplicate review_case_id",call.=FALSE)
queue_by_id <- setNames(queue,queue_ids)

expected <- unlist(manifest$expected_review_case_ids,use.names=FALSE)
expected <- as.character(expected)
if(length(expected)!=as.integer(manifest$size)) stop("Batch manifest size does not match expected_review_case_ids",call.=FALSE)
if(anyDuplicated(expected)) stop("Batch manifest contains duplicate case IDs",call.=FALSE)
if(any(!expected %in% queue_ids)) stop("Batch manifest contains IDs not present in queue",call.=FALSE)

decision_ids <- vapply(decisions,function(z)as.character(z$review_case_id),character(1))
if(anyDuplicated(decision_ids)) stop("Batch decisions contain duplicate review_case_id",call.=FALSE)
if(!setequal(decision_ids,expected) || length(decision_ids)!=length(expected)) {
  missing <- setdiff(expected,decision_ids)
  extra <- setdiff(decision_ids,expected)
  stop(sprintf("Batch decisions must match expected IDs exactly; missing=%d extra=%d",length(missing),length(extra)),call.=FALSE)
}

valid_decisions <- c("duplicate","not_duplicate","uncertain")
for(d in decisions) {
  if(is.null(d$batch_id)||!identical(as.character(d$batch_id),as.character(manifest$batch_id))) stop(sprintf("%s has wrong/missing batch_id",d$review_case_id),call.=FALSE)
  if(is.null(d$queue_sha256)||!identical(as.character(d$queue_sha256),queue_sha)) stop(sprintf("%s has wrong/missing queue_sha256",d$review_case_id),call.=FALSE)
  if(is.null(d$decision)||!(d$decision %in% valid_decisions)) stop(sprintf("%s has invalid decision",d$review_case_id),call.=FALSE)
  for(field in c("rationale","reviewer","resolved_at_utc")) {
    if(is.null(d[[field]])||!nzchar(trimws(as.character(d[[field]])))) stop(sprintf("%s missing %s",d$review_case_id,field),call.=FALSE)
  }
}

allowed_actions <- c("strip_abstract","replace_abstract","set_doi","set_title","set_canonical_preference")
for(r in repairs) {
  id <- as.character(r$review_case_id)
  if(!(id %in% expected)) stop(sprintf("Repair references case outside batch: %s",id),call.=FALSE)
  if(is.null(r$batch_id)||!identical(as.character(r$batch_id),as.character(manifest$batch_id))) stop(sprintf("Repair %s wrong/missing batch_id",id),call.=FALSE)
  if(is.null(r$queue_sha256)||!identical(as.character(r$queue_sha256),queue_sha)) stop(sprintf("Repair %s wrong/missing queue_sha256",id),call.=FALSE)
  if(is.null(r$source_record_id)||!nzchar(trimws(as.character(r$source_record_id)))) stop(sprintf("Repair %s missing immutable source_record_id",id),call.=FALSE)
  if(is.null(r$action)||!(r$action %in% allowed_actions)) stop(sprintf("Repair %s invalid action",id),call.=FALSE)
  q <- queue_by_id[[id]]
  members <- c(as.character(q$record_i$source_record_id),as.character(q$record_j$source_record_id))
  if(!(as.character(r$source_record_id) %in% members)) stop(sprintf("Repair %s targets source_record_id outside pair",id),call.=FALSE)
  expected_source <- if(identical(as.character(r$source_record_id),members[[1]])) as.character(q$record_i$source) else as.character(q$record_j$source)
  if(is.null(r$source)||!identical(as.character(r$source),expected_source)) stop(sprintf("Repair %s source does not match source_record_id",id),call.=FALSE)
  if(r$action %in% c("replace_abstract","set_doi","set_title","set_canonical_preference") && (is.null(r$value)||!nzchar(trimws(as.character(r$value))))) {
    stop(sprintf("Repair %s action %s requires value",id,r$action),call.=FALSE)
  }
  for(field in c("reason","recorded_by","recorded_at_utc")) {
    if(is.null(r[[field]])||!nzchar(trimws(as.character(r[[field]])))) stop(sprintf("Repair %s missing %s",id,field),call.=FALSE)
  }
}

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
write_jsonl(decisions,file.path(output_dir,"validated_batch_decisions.jsonl"))
write_jsonl(repairs,file.path(output_dir,"validated_batch_repairs.jsonl"))
result <- list(
  schema="living-evidence-map-workflow01-human-review-batch-validation-v1",
  batch_id=manifest$batch_id,
  queue_sha256=queue_sha,
  batch_manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  decisions_sha256=digest(file=decisions_path,algo="sha256",serialize=FALSE),
  repairs_sha256=if(length(repairs))digest(file=repairs_path,algo="sha256",serialize=FALSE) else NULL,
  expected_cases=length(expected),
  validated_decisions=length(decisions),
  validated_repairs=length(repairs),
  complete=TRUE
)
writeLines(toJSON(result,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"batch_validation_manifest.json"),useBytes=TRUE)
cat(sprintf("PASS: validated %d decisions and %d repairs for %s\n",length(decisions),length(repairs),manifest$batch_id))
