#!/usr/bin/env Rscript
# Convert a completed inherited W00 provider manifest to an immutable source
# methods receipt. No external requests are made.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k,d=NULL){i<-match(k,args);if(is.na(i))return(d);if(i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
scalar <- function(x,d=""){if(is.null(x)||!length(x))return(d);as.character(x[[1L]])}
nval <- function(x){if(is.null(x)||!length(x))return(NA_integer_);suppressWarnings(as.integer(x[[1L]]))}
getfirst <- function(x,keys){for(k in keys){z<-x[[k]];if(!is.null(z)&&length(z))return(z)};NULL}
plan_path <- arg("--search-plan")
source <- arg("--source")
root <- arg("--output-root","outputs/workflow00")
parent_run <- arg("--parent-run-id",Sys.getenv("GITHUB_RUN_ID","local"))
if(is.null(plan_path)||!file.exists(plan_path)||is.null(source))stop("Missing search plan/source",call.=FALSE)
plan <- fromJSON(plan_path,simplifyVector=FALSE)
d <- plan$source_definitions[[source]]
if(is.null(d))stop("Unknown source",call.=FALSE)
backend <- scalar(d$backend,source)
if(startsWith(source,"ebsco_")&&backend==source)backend<-"ebsco"
if(backend %in% c("pubmed","ethos","cba","epmc_preprints"))backend<-"europe_pmc"
child <- file.path(root,"harvests",source,"source_child")
manifest_path <- file.path(child,"manifest.json")
meta_path <- file.path(child,"execution_metadata.json")
if(!file.exists(manifest_path)||!file.exists(meta_path))stop("Missing completed source manifest/timing metadata for ",source,call.=FALSE)
m <- fromJSON(manifest_path,simplifyVector=FALSE)
meta <- fromJSON(meta_path,simplifyVector=FALSE)
if(!identical(scalar(m$status),"success")||!identical(scalar(meta$query),scalar(d$query)))
  stop("Source is not complete or plan query differs: ",source,call.=FALSE)
opts <- d$options
if(is.null(opts))opts<-list()
if(backend=="manual_ris"){
  hits <- nval(getfirst(m$records,c("reported_search_results")))
  downloaded <- nval(m$records$unique_records_for_handover)
  searched <- scalar(opts$search_executed_at_utc)
  exported <- scalar(opts$exported_at_utc)
  if(!nzchar(searched)||!nzchar(exported))stop(source,": manual RIS search and export timestamps must be supplied and verified",call.=FALSE)
}else if(backend=="lens"){
  hits<-nval(m$lens_reported_total)
  downloaded<-nval(m$unique_records_written)
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}else if(backend=="openalex"){
  hits<-nval(m$total_results_reported_by_openalex_first_page)
  downloaded<-nval(m$validation$unique_openalex_ids)
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}else if(backend=="scopus"){
  hits<-nval(m$unpartitioned_total)
  downloaded<-nval(m$entries_retrieved)
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}else if(backend=="wos"){
  hits<-nval(m$reported_total)
  downloaded<-nval(m$records_retrieved)
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}else if(backend=="ebsco"){
  hits<-nval(m$reported_total)
  downloaded<-nval(m$records_retrieved)
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}else{
  hits<-nval(getfirst(m,c("total_results_reported_first_page","reported_total")))
  downloaded<-nval(getfirst(m,c("records_retrieved","entries_retrieved")))
  searched<-scalar(meta$started_at_utc);exported<-scalar(meta$completed_at_utc)
}
if(anyNA(c(hits,downloaded))||hits<0L||downloaded<0L||downloaded>hits)
  stop(source,": cannot reconcile source's reported hits and unique downloaded count",call.=FALSE)
# A verified source archive DOI cannot exist before depositing this current run.
# Archive metadata is added only after archive publication, not forged here.
cli <- c("scripts/updater/write_workflow00_search_record.R",
 "--source",source,"--run-type",scalar(plan$run_type),"--search-plan",plan_path,
 "--reported-results",as.character(hits),"--downloaded-results",as.character(downloaded),
 "--parent-run-id",parent_run,"--child-run-id",parent_run,
 "--searched-at-utc",searched,"--exported-at-utc",exported,
 "--source-zenodo-status","unverified",
 "--output-root",file.path(root,"search_records"))
status <- system2(file.path(R.home("bin"),"Rscript"),args=shQuote(cli),stdout="",stderr="")
if(!identical(as.integer(status),0L))stop("Could not write source search receipt",call.=FALSE)
cat(sprintf("PASS: %s provider hits=%d, unique input records=%d, recorded at source\n",source,hits,downloaded))
