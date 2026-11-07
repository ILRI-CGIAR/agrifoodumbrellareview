#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(data.table)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

metadata_path <- arg("--metadata")
checkpoint_manifest_path <- arg("--checkpoint-manifest")
output_path <- arg("--output","")

if(is.null(metadata_path) || is.null(checkpoint_manifest_path)) {
  stop("Required: --metadata --checkpoint-manifest",call.=FALSE)
}
if(!file.exists(metadata_path)) stop("Metadata file not found",call.=FALSE)
if(!file.exists(checkpoint_manifest_path)) stop("Checkpoint manifest not found",call.=FALSE)

cm <- fromJSON(checkpoint_manifest_path,simplifyVector=FALSE)
expected <- cm$dedup_index_identity
if(is.null(expected) || is.null(expected$sha256) || is.null(expected$rows)) {
  stop(
    "Checkpoint lacks dedup_index_identity; refusing unsafe reuse of numeric deduplication indices",
    call.=FALSE
  )
}

meta <- fread(
  metadata_path,
  select=c("idx","source","source_record_id"),
  na.strings=c("","NA")
)
if(!identical(as.integer(meta$idx),seq_len(nrow(meta)))) {
  stop("Metadata idx must be contiguous 1..N",call.=FALSE)
}
if(any(is.na(meta$source)) || any(is.na(meta$source_record_id)) ||
   any(!nzchar(meta$source)) || any(!nzchar(meta$source_record_id))) {
  stop("Metadata contains missing immutable source identity",call.=FALSE)
}
keys <- paste(meta$source,meta$source_record_id,sep="::")
if(anyDuplicated(keys)) stop("Metadata contains duplicate immutable source identity",call.=FALSE)

payload <- paste(
  paste(meta$idx,meta$source,meta$source_record_id,sep="\t"),
  collapse="\n"
)
actual_sha <- digest(payload,algo="sha256",serialize=FALSE)
actual_rows <- nrow(meta)

if(actual_rows != as.integer(expected$rows)) {
  stop(sprintf(
    "W01 dedup index identity row-count mismatch: expected %d, found %d",
    as.integer(expected$rows),actual_rows
  ),call.=FALSE)
}
if(!identical(tolower(actual_sha),tolower(as.character(expected$sha256)))) {
  stop(sprintf(
    "W01 dedup index identity mismatch: expected %s, found %s. Numeric pair indices are unsafe to reuse.",
    as.character(expected$sha256),actual_sha
  ),call.=FALSE)
}

result <- list(
  schema="living-evidence-map-workflow01-index-identity-validation-v1",
  status="PASS",
  rows=actual_rows,
  sha256=actual_sha
)
if(nzchar(output_path)) {
  dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
  writeLines(toJSON(result,auto_unbox=TRUE,pretty=TRUE),output_path,useBytes=TRUE)
}
cat(sprintf("PASS: W01 dedup index identity verified for %d manifestations; sha256=%s\n",
            actual_rows,actual_sha))
