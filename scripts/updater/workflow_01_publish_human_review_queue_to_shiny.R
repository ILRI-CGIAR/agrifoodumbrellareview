#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(googlesheets4)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

queue_path <- arg("--queue")
manifest_path <- arg("--manifest")
sheet_id <- arg("--sheet-id")
tab <- arg("--tab","queue_w01_active")
source_run_id <- arg("--source-run-id")
credential_path <- arg("--credential")

required <- list(queue_path,manifest_path,sheet_id,tab,source_run_id,credential_path)
if(any(vapply(required,function(x)is.null(x)||!nzchar(x),logical(1)))) {
  stop("Required: --queue --manifest --sheet-id --tab --source-run-id --credential",call.=FALSE)
}
if(!file.exists(queue_path)) stop(sprintf("Queue file missing: %s",queue_path),call.=FALSE)
if(!file.exists(manifest_path)) stop(sprintf("Manifest file missing: %s",manifest_path),call.=FALSE)
if(!file.exists(credential_path)) stop(sprintf("Google credential file missing: %s",credential_path),call.=FALSE)

lines <- readLines(queue_path,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
if(!length(lines)) stop("Human-review queue is empty; refusing to publish",call.=FALSE)

parsed <- lapply(lines,fromJSON,simplifyVector=FALSE)
ids <- vapply(parsed,function(x)as.character(x$review_case_id),character(1))
schemas <- vapply(parsed,function(x)as.character(x$schema),character(1))
if(any(!nzchar(ids)) || anyDuplicated(ids)) stop("Invalid or duplicate review_case_id",call.=FALSE)
if(any(schemas!="living-evidence-map-workflow01-duplicate-adjudication-case-v1")) {
  stop("Unsupported W01 adjudication case schema",call.=FALSE)
}

manifest <- fromJSON(manifest_path,simplifyVector=FALSE)
if(xor(is.null(manifest$review_id),is.null(manifest$google_sheet_id)))
  stop("Human-review manifest contains incomplete review identity",call.=FALSE)
if(!is.null(manifest$review_id)) {
  if(!nzchar(as.character(manifest$review_id)) ||
     !identical(as.character(manifest$google_sheet_id),as.character(sheet_id)))
    stop("Refusing to publish W01 human-review queue to another review's Google Sheet",call.=FALSE)
}
expected_sha <- as.character(manifest$queue_sha256)
if(!nzchar(expected_sha)) stop("review_manifest.json has no queue_sha256",call.=FALSE)
actual_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
if(!identical(actual_sha,expected_sha)) {
  stop(sprintf("Queue SHA mismatch: manifest=%s actual=%s",expected_sha,actual_sha),call.=FALSE)
}
manifest_count <- manifest$pending_count
if(is.null(manifest_count) || !length(manifest_count)) manifest_count <- manifest$cases
manifest_n <- suppressWarnings(as.integer(unlist(manifest_count,use.names=FALSE)))
if(length(manifest_n)!=1L || is.na(manifest_n[[1L]]) || manifest_n[[1L]]!=length(lines)) {
  manifest_count_display <- paste(as.character(unlist(manifest_count,use.names=FALSE)),collapse=",")
  if(!nzchar(manifest_count_display)) manifest_count_display <- "<missing>"
  stop(sprintf("Queue count mismatch: manifest=%s JSONL=%d",manifest_count_display,length(lines)),call.=FALSE)
}

payload <- data.frame(
  batch_id=rep(paste0("w01-run-",source_run_id),length(lines)),
  queue_sha256=rep(expected_sha,length(lines)),
  case_index=as.character(seq_along(lines)),
  review_case_id=ids,
  case_json=lines,
  stringsAsFactors=FALSE
)

gs4_auth(path=credential_path,cache=FALSE)
if(!is.null(manifest$review_id)) {
  meta <- read_sheet(sheet_id,sheet="review_metadata",col_types="cc")
  if(!all(c("key","value") %in% names(meta)))
    stop("Review destination has invalid review_metadata",call.=FALSE)
  rows <- as.character(meta$value[which(as.character(meta$key)=="review_id")])
  if(length(rows)!=1L || is.na(rows) ||
     !identical(rows,as.character(manifest$review_id)))
    stop("W01 adjudication destination review_metadata does not match its source review",call.=FALSE)
}
tabs <- sheet_names(sheet_id)
if(!tab %in% tabs) sheet_add(sheet_id,sheet=tab)
sheet_write(payload,ss=sheet_id,sheet=tab)

verify <- read_sheet(sheet_id,sheet=tab,col_types="c")
required_cols <- c("batch_id","queue_sha256","case_index","review_case_id","case_json")
if(!all(required_cols %in% names(verify))) stop("Published queue missing required columns",call.=FALSE)
verify <- verify[,required_cols,drop=FALSE]
if(nrow(verify)!=length(lines)) stop("Published queue row count mismatch",call.=FALSE)
if(!identical(as.character(verify$review_case_id),ids)) stop("Published review_case_id order mismatch",call.=FALSE)
if(length(unique(verify$queue_sha256))!=1L || !identical(unique(verify$queue_sha256)[[1L]],expected_sha)) {
  stop("Published queue SHA metadata mismatch",call.=FALSE)
}
reconstructed <- paste0(paste(verify$case_json,collapse="\n"),"\n")
reconstructed_sha <- digest(reconstructed,algo="sha256",serialize=FALSE)
if(!identical(reconstructed_sha,expected_sha)) {
  stop("Published queue failed reconstruction SHA validation",call.=FALSE)
}

cat("PASS: published validated W01 human-review queue to Shiny\n")
cat("Source run:",source_run_id,"\n")
cat("Cases:",length(lines),"\n")
cat("Queue SHA-256:",expected_sha,"\n")
