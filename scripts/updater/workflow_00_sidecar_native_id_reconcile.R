#!/usr/bin/env Rscript
# Native-identifier reconciliation for manually supplied RIS and EBSCO sidecars.
# Works for first runs and updates. The immutable original harvest is untouched;
# the filtered source directory is used by deduplication.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k,default=NULL){i<-match(k,args);if(is.na(i))return(default);if(i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
source <- arg("--source")
current_root <- arg("--current-root")
history_root <- arg("--history-root","")
output <- arg("--output-dir")
mode <- arg("--run-type","full")
if(any(vapply(list(source,current_root,output),is.null,logical(1)))||
   !grepl("^[a-z0-9][a-z0-9_-]*$",source) ||
   !mode %in% c("full","fortnightly","monthly","expansion"))
  stop("Invalid --source --current-root --output-dir or --run-type",call.=FALSE)
if(mode!="full"&&(!nzchar(history_root)||!dir.exists(history_root)))
  stop("Update requires an explicit restored history directory",call.=FALSE)
input <- file.path(current_root,"records.jsonl")
if(!file.exists(input))stop("Source sidecar records.jsonl missing: ",input,call.=FALSE)
readjl <- function(p){
  lines<-readLines(p,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  lapply(lines,fromJSON,simplifyVector=FALSE)
}
identity <- function(r){
  provider <- r$source$provider
  id <- r$sidecar_identity$sidecar_record_id
  if(is.null(provider)||!identical(as.character(provider),source)||
     is.null(id)||length(id)!=1L||!nzchar(trimws(as.character(id))))
    stop("Missing or mismatched source/sidecar native ID for ",source,call.=FALSE)
  as.character(id)
}
current <- readjl(input)
current_ids <- if(length(current))vapply(current,identity,character(1)) else character()
keys <- unique(current_ids)
keep <- !duplicated(current_ids)
if(anyDuplicated(current_ids)){
  for(id in unique(current_ids[duplicated(current_ids)])){
    rows<-current[which(current_ids==id)]
    hashes<-vapply(rows,function(z)digest(toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),
      algo="sha256",serialize=FALSE),character(1))
    if(length(unique(hashes))!=1L)stop("Conflicting source payloads for native ID ",id,call.=FALSE)
  }
}
records<-current[keep]
ids<-current_ids[keep]
history_ids<-character()
if(nzchar(history_root)&&dir.exists(history_root)){
  prior_reg <- file.path(history_root,"native_id_reconciliation","updated_native_ids.txt")
  if(file.exists(prior_reg)){
    history_ids<-trimws(readLines(prior_reg,warn=FALSE))
    history_ids<-history_ids[nzchar(history_ids)]
  } else {
    prior <- file.path(history_root,"source_child","records.jsonl")
    if(file.exists(prior)){
      h <- readjl(prior)
      history_ids <- if(length(h))vapply(h,identity,character(1)) else character()
    }
  }
}
history_ids<-unique(history_ids)
if(mode!="full" && !length(history_ids) && !identical(mode,"expansion"))
  stop("No historical native IDs for update source: ",source,call.=FALSE)
known<-ids %in% history_ids
new<-!known
dir.create(file.path(output,"filtered_source"),recursive=TRUE,showWarnings=FALSE)
writejl<-function(xs,p){
  con<-file(p,"wt",encoding="UTF-8")
  on.exit(close(con))
  for(x in xs)writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
}
writejl(records[new],file.path(output,"filtered_source","records.jsonl"))
writejl(records[new],file.path(output,"new_records.jsonl"))
writeLines(ids[new],file.path(output,"new_native_ids.txt"))
writeLines(ids[known],file.path(output,"already_known_native_ids.txt"))
writeLines(sort(unique(c(history_ids,ids))),file.path(output,"updated_native_ids.txt"))
m<-list(schema="evidence-sidecar-native-id-reconciliation-v1",
  status="success",source=source,run_type=mode,
  current_raw_records=length(current_ids),current_unique_records=length(ids),
  exact_duplicate_ids_suppressed=sum(!keep),
  historical_unique_native_ids=length(history_ids),
  already_known_native_ids=sum(known),new_native_ids=sum(new),
  records_passed_downstream=sum(new),
  filtered_source_root="filtered_source",
  bibliographic_deduplication_performed=FALSE,
  reconciliation_key="source provider + source-native sidecar record ID",
  source_sha256=digest(file=input,algo="sha256",serialize=FALSE))
writeLines(toJSON(m,auto_unbox=TRUE,pretty=TRUE,null="null"),
  file.path(output,"reconciliation_manifest.json"))
cat(sprintf("PASS: %s sidecar native ID reconciliation; %d raw, %d unique, %d known, %d new\n",
  source,length(current_ids),length(ids),sum(known),sum(new)))
