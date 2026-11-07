#!/usr/bin/env Rscript
suppressPackageStartupMessages({
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

batch_root <- arg("--batch-root")
plan_path <- arg("--plan")
input_path <- arg("--input")
current_patch_path <- arg("--current-patch")
output_path <- arg("--output")
if(any(vapply(list(batch_root,plan_path,input_path,current_patch_path,output_path),is.null,logical(1)))) {
  stop("Required: --batch-root --plan --input --current-patch --output",call.=FALSE)
}

plan <- fromJSON(plan_path,simplifyVector=FALSE)
report_paths <- sort(list.files(batch_root,pattern="enrichment_report\\.json$",recursive=TRUE,full.names=TRUE))
if(length(report_paths) != as.integer(plan$batch_count)) {
  stop(sprintf("Expected %d batch reports, found %d",as.integer(plan$batch_count),length(report_paths)),call.=FALSE)
}
reports <- lapply(report_paths,fromJSON,simplifyVector=FALSE)

sum_count <- function(name){
  sum(vapply(reports,function(x) as.numeric((x$counts %||% list())[[name]] %||% 0),numeric(1)))
}
`%||%` <- function(x,y) if(is.null(x)) y else x

sum_names <- c(
  "europepmc_title_filled","europepmc_abstract_filled","europepmc_author_keywords_filled",
  "scopus_attempted","scopus_title_filled","scopus_abstract_filled",
  "scopus_author_keywords_attempted","scopus_author_keywords_filled","scopus_http_404",
  "conflicts_quarantined","still_missing_after","technical_error_records"
)
counts <- list(
  total_records=as.integer(plan$total_records),
  eligible_doi_missing_metadata=as.integer(plan$eligible_records),
  deferred_recent_attempts=as.integer(plan$deferred_recent_attempts)
)
for(n in sum_names) counts[[n]] <- as.integer(sum_count(n))

processed <- sum(vapply(reports,function(x) as.integer(x$processed_eligible_records %||% 0L),integer(1)))
if(processed != as.integer(plan$due_records)) {
  stop(sprintf("Processed batch records (%d) do not equal planned due records (%d)",processed,as.integer(plan$due_records)),call.=FALSE)
}

out <- list(
  schema="living-evidence-map-workflow02-metadata-enrichment-batched-v1",
  workflow="02_metadata_enrichment",
  implementation_language="R",
  execution="deterministic_batched",
  provider_order=c("europe_pmc","scopus"),
  batch_plan=list(
    due_records=as.integer(plan$due_records),
    batch_size=as.integer(plan$batch_size),
    batch_count=as.integer(plan$batch_count),
    plan_sha256=digest(file=plan_path,algo="sha256",serialize=FALSE)
  ),
  processed_eligible_records=processed,
  counts=counts,
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  current_patch_sha256=digest(file=current_patch_path,algo="sha256",serialize=FALSE),
  completed_at=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(out,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),output_path,useBytes=TRUE)
cat(sprintf("PASS: aggregated %d Workflow 02 batches covering %d records\n",length(report_paths),processed))
