#!/usr/bin/env Rscript
# Check one current-run receipt for every planned database, without API calls.
# This is the authoritative preflight input for final methods reporting.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k,default=NULL){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
plan_path<-arg("--search-plan")
receipt_dir<-arg("--receipts-dir")
output<-arg("--output")
require_published<-identical(tolower(arg("--require-published","false")),"true")
if(any(vapply(list(plan_path,receipt_dir,output),is.null,logical(1))))
  stop("Required: --search-plan --receipts-dir --output",call.=FALSE)
if(!file.exists(plan_path)||!dir.exists(receipt_dir))
  stop("Search plan/receipts directory missing",call.=FALSE)
plan<-fromJSON(plan_path,simplifyVector=FALSE)
if(!identical(plan$schema,"evidence-search-plan-v2"))stop("Invalid source-specific search plan",call.=FALSE)
project_id<-as.character(plan$project_id)
if(length(project_id)!=1L||!nzchar(project_id))stop("Missing project identity in planned searches",call.=FALSE)
src<-names(plan$source_definitions)
if(!length(src)||anyDuplicated(src))stop("Missing or duplicate source definitions",call.=FALSE)
paths<-list.files(receipt_dir,pattern="\\.json$",recursive=TRUE,full.names=TRUE)
if(!length(paths))stop("No source receipts supplied",call.=FALSE)
receipts<-lapply(paths,fromJSON,simplifyVector=FALSE)
codes<-vapply(receipts,function(z)if(is.null(z$source))"" else as.character(z$source),character(1))
if(length(paths)!=length(src)||anyDuplicated(codes)||!setequal(src,codes))
  stop("Expected exactly one current-run receipt per planned source",call.=FALSE)
ord<-match(src,codes)
paths<-paths[ord];receipts<-receipts[ord]
scalar<-function(x)if(is.null(x)||!length(x))"" else as.character(x[[1L]])
utc<-function(x)grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",scalar(x))
out<-vector("list",length(src))
for(i in seq_along(src)){
  z<-receipts[[i]];d<-plan$source_definitions[[src[[i]]]]
  if(!identical(scalar(z$project_id),project_id) ||
     !identical(scalar(z$search_string),scalar(d$query)) ||
     !identical(scalar(z$database_name),scalar(d$database)) ||
     !identical(scalar(z$platform),scalar(d$platform)) ||
     !identical(unlist(z$searched_fields,use.names=FALSE),
                unlist(d$fields,use.names=FALSE)) ||
     !identical(scalar(z$search_version),scalar(plan$search_version)) ||
     !identical(scalar(z$run_type),scalar(plan$run_type)))
    stop("Search query/scope/version mismatch for source: ",src[[i]],call.=FALSE)
  if(!utc(z$search_executed_at_utc)||!utc(z$exported_at_utc)||
     scalar(z$exported_at_utc)<scalar(z$search_executed_at_utc))
    stop("Missing/invalid actual search or export timestamp: ",src[[i]],call.=FALSE)
  hits<-as.integer(z$reported_search_results)
  downloaded<-as.integer(z$successfully_downloaded_results)
  if(length(hits)!=1L||length(downloaded)!=1L||
     is.na(hits)||is.na(downloaded)||hits<0L||downloaded<0L||downloaded>hits)
    stop("Invalid database hit/download counts: ",src[[i]],call.=FALSE)
  a<-z$source_archive
  state<-scalar(a$publication_status)
  doi<-scalar(a$doi)
  if(require_published&&(!identical(state,"published_verified")||!nzchar(doi)))
    stop("Final source methods require verified published source archive: ",src[[i]],call.=FALSE)
  out[[i]]<-list(source=src[[i]],database_name=scalar(z$database_name),
    platform=scalar(z$platform),search_string=scalar(z$search_string),
    searched_fields=unname(unlist(z$searched_fields,use.names=FALSE)),
    search_executed_at_utc=scalar(z$search_executed_at_utc),
    exported_at_utc=scalar(z$exported_at_utc),
    provider_reported_hits=hits,downloaded_records=downloaded,
    complete_download=isTRUE(z$complete_download),
    source_archive=list(doi=if(nzchar(doi))doi else NULL,publication_status=state),
    source_receipt_sha256=digest(file=paths[[i]],algo="sha256",serialize=FALSE))
}
result<-list(schema="evidence-search-source-report-v1",project_id=project_id,
  search_version=scalar(plan$search_version),run_type=scalar(plan$run_type),
  source_count=length(out),downloaded_manifestations=sum(vapply(out,function(z)z$downloaded_records,integer(1))),
  total_provider_reported_hits=sum(vapply(out,function(z)z$provider_reported_hits,integer(1))),
  source_searches=out,source_plan_sha256=digest(file=plan_path,algo="sha256",serialize=FALSE),
  report_ready=all(vapply(out,function(z)identical(z$source_archive$publication_status,"published_verified"),logical(1))))
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(result,auto_unbox=TRUE,pretty=TRUE,null="null"),output)
cat(sprintf("PASS: verified %d source receipts, %d downloaded manifestations; report_ready=%s\n",
            length(out),result$downloaded_manifestations,result$report_ready))
