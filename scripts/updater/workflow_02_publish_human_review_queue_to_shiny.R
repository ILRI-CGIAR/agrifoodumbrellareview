#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(googlesheets4)
  library(jsonlite)
  library(digest)
})
`%||%` <- function(x,y) if(is.null(x)) y else x

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_path <- arg("--input")
manifest_path <- arg("--manifest")
sheet_id <- arg("--sheet-id")
tab <- arg("--tab","queue_w02_active")
source_run_id <- arg("--source-run-id")
credential <- arg("--credential")

req <- list(input_path,manifest_path,sheet_id,tab,source_run_id,credential)
if(any(vapply(req,function(x)is.null(x)||!nzchar(x),logical(1)))) {
  stop("Required: --input --manifest --sheet-id --tab --source-run-id --credential",call.=FALSE)
}
if(!file.exists(input_path)) stop("W02 human-review JSONL missing",call.=FALSE)
if(!file.exists(manifest_path)) stop("W02 human-review manifest missing",call.=FALSE)
if(!file.exists(credential)) stop("Google credential missing",call.=FALSE)

source_lines <- readLines(input_path,warn=FALSE,encoding="UTF-8")
source_lines <- source_lines[nzchar(trimws(source_lines))]
if(!length(source_lines)) stop("W02 human-review package is empty",call.=FALSE)

manifest <- fromJSON(manifest_path,simplifyVector=FALSE)
expected_source_sha <- as.character(manifest$review_jsonl$sha256 %||% "")
actual_source_sha <- digest(file=input_path,algo="sha256",serialize=FALSE)
if(!nzchar(expected_source_sha)||!identical(actual_source_sha,expected_source_sha)) {
  stop("W02 source review JSONL SHA mismatch",call.=FALSE)
}

source_cases <- lapply(source_lines,fromJSON,simplifyVector=FALSE)
allowed_reasons <- c("title_mismatch","title_guard_failed","returned_doi_mismatch","keyword_identity_guard_failed")
out <- vector("list",length(source_cases))

for(i in seq_along(source_cases)){
  z <- source_cases[[i]]
  rid <- as.character(z$record_id %||% "")
  conflict <- z$conflict %||% list()
  provider <- as.character(conflict$provider %||% "")
  field <- as.character(conflict$field %||% "")
  reason <- as.character(conflict$reason %||% "")
  returned_doi <- as.character(conflict$returned_doi %||% "")
  eid <- as.character(conflict$eid %||% "")
  if(!nzchar(rid)||!nzchar(provider)||!nzchar(reason)) {
    stop(sprintf("Invalid W02 review case at source row %d",i),call.=FALSE)
  }
  if(!reason %in% allowed_reasons) {
    stop(sprintf("Unsupported W02 conflict reason at row %d: %s",i,reason),call.=FALSE)
  }
  case_id <- paste0("w02-",digest(
    paste(rid,provider,field,reason,returned_doi,eid,i,sep="|"),
    algo="sha256",serialize=FALSE
  ))
  out[[i]] <- list(
    schema="living-evidence-map-workflow02-shiny-case-v1",
    review_case_id=case_id,
    source_index=i,
    record_id=rid,
    doi=as.character(z$doi %||% ""),
    provider=provider,
    field=field,
    reason=reason,
    returned_doi=returned_doi,
    canonical=z$canonical %||% list(),
    conflict=conflict,
    provider_response=z$provider_response %||% list()
  )
}

ids <- vapply(out,function(z)z$review_case_id,character(1))
if(anyDuplicated(ids)) stop("Generated W02 review_case_id collision",call.=FALSE)

queue_path <- tempfile("w02_shiny_queue_",fileext=".jsonl")
con <- file(queue_path,"wt",encoding="UTF-8")
for(z in out) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
close(con)
queue_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
queue_lines <- readLines(queue_path,warn=FALSE,encoding="UTF-8")
queue_lines <- queue_lines[nzchar(trimws(queue_lines))]
if(length(queue_lines)!=length(out)) stop("Generated W02 queue count mismatch",call.=FALSE)

batch_id <- paste0("w02-run-",source_run_id)
payload <- data.frame(
  batch_id=rep(batch_id,length(out)),
  queue_sha256=rep(queue_sha,length(out)),
  case_index=as.character(seq_along(out)),
  review_case_id=ids,
  case_json=queue_lines,
  stringsAsFactors=FALSE
)

gs4_auth(path=credential,cache=FALSE)
tabs <- sheet_names(sheet_id)
if(!tab %in% tabs) sheet_add(sheet_id,sheet=tab)
sheet_write(payload,ss=sheet_id,sheet=tab)

verify <- read_sheet(sheet_id,sheet=tab,col_types="c")
required <- c("batch_id","queue_sha256","case_index","review_case_id","case_json")
if(!all(required %in% names(verify))) stop("Published W02 queue missing required columns",call.=FALSE)
verify <- verify[,required,drop=FALSE]
if(nrow(verify)!=length(out)) stop("Published W02 queue row count mismatch",call.=FALSE)
if(!identical(as.character(verify$review_case_id),ids)) stop("Published W02 case order mismatch",call.=FALSE)
reconstructed <- paste0(paste(verify$case_json,collapse="\n"),"\n")
if(!identical(digest(reconstructed,algo="sha256",serialize=FALSE),queue_sha)) {
  stop("Published W02 queue reconstruction SHA mismatch",call.=FALSE)
}

metadata_output <- arg("--metadata-output", "")
if(nzchar(metadata_output)) {
  dir.create(dirname(metadata_output), recursive=TRUE, showWarnings=FALSE)
  writeLines(toJSON(list(batch_id=batch_id,queue_sha256=queue_sha,cases=length(out)),
                    auto_unbox=TRUE,pretty=TRUE),metadata_output,useBytes=TRUE)
}
cat(sprintf("PASS: published validated W02 queue batch=%s cases=%d sha256=%s\n",batch_id,length(out),queue_sha))
