#!/usr/bin/env Rscript
# Read-only discovery of W01-ready JSONL handoffs restored from a review's W00 archive.
# Do not silently accept raw API payloads as converted bibliographic records.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
get<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
pointer<-get("--w00-pointer")
root<-get("--restored-root")
output<-get("--output")
x<-fromJSON(pointer,simplifyVector=FALSE)
if(!identical(x$status,"published")||!identical(x$visibility,"restricted")||!nzchar(as.character(x$review_id)))
  stop("Review-specific W00 pointer required",call.=FALSE)
sources<-as.character(unlist(x$sources,use.names=FALSE))
if(!length(sources)||anyDuplicated(sources))stop("Invalid W00 source listing",call.=FALSE)
z<-list()
for(src in sources){
  if(!grepl("^[a-z0-9][a-z0-9_-]*$",src)) stop("Invalid W00 source",call.=FALSE)
  dir<-file.path(root,src)
  if(!dir.exists(dir))stop("Missing restored API source: ",src,call.=FALSE)
  candidates<-c(file.path(dir,"records.jsonl"),
    file.path(dir,paste0(src,"_sidecar_records.jsonl")),
    file.path(dir,paste0(src,"_starter_sidecar_records.jsonl")),
    file.path(dir,"handoff","records.jsonl"),
    file.path(dir,"source_child","records.jsonl"))
  present<-candidates[file.exists(candidates)]
  if(length(present)!=1L)
    stop("W00 source ",src," has no unique prepared records.jsonl; existing API adapter must run first",call.=FALSE)
  lines<-readLines(present,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  ids<-character()
  for(line in lines){
    rec<-fromJSON(line,simplifyVector=FALSE)
    if(identical(src,"lens")) {
      if(!is.list(rec$lens)) stop("Lens record lacks expected lens object",call.=FALSE)
      rid <- rec$identity$lens_id
      if(is.null(rid)) rid <- rec$identity$record_id
    } else {
      provider <- rec$source$provider
      if(identical(as.character(provider),"agricola_via_europe_pmc")) provider <- "agricola"
      if(identical(as.character(provider),"wos_starter")) provider <- "wos"
      if(is.null(provider) || !identical(as.character(provider),src) ||
         is.null(rec$sidecar_identity$sidecar_record_id))
        stop("W00 ",src," has non-W01 JSONL or incorrect provider",call.=FALSE)
      rid <- rec$sidecar_identity$sidecar_record_id
    }
    if(is.null(rid) || !nzchar(trimws(as.character(rid))))
      stop("W00 ",src," contains a missing source identifier",call.=FALSE)
    ids<-c(ids,as.character(rid))
  }
  if(anyDuplicated(ids))stop("Repeated sidecar identities in source ",src,call.=FALSE)
  expected_n <- length(lines)
  if(identical(src,"openalex")) {
    audit_path <- file.path(dir,"compatibility_audit.json")
    ledger_path <- file.path(dir,"duplicate_source_occurrences.jsonl")
    if(!file.exists(audit_path) || !file.exists(ledger_path))
      stop("OpenAlex prepared input lacks mandatory occurrence reconciliation files",call.=FALSE)
    audit <- fromJSON(audit_path,simplifyVector=FALSE)
    expected_n <- as.integer(audit$input$records_read)
    unique_n <- as.integer(audit$input$unique_records)
    duplicates <- as.integer(audit$input$identical_duplicate_occurrences)
    if(anyNA(c(expected_n,unique_n,duplicates)) ||
       expected_n!=unique_n+duplicates || unique_n!=length(lines))
      stop("OpenAlex prepared count does not reconcile with raw occurrence audit",call.=FALSE)
  }
  z[[src]]<-list(kind="workflow00_state",source=src,prepared_path=present[[1L]],
    expected_records=expected_n,sha256=digest(file=present[[1L]],algo="sha256",serialize=FALSE))
}
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
write_json(list(schema="living-evidence-map-workflow01-source-catalogue-v1",
  review_id=x$review_id,sources=z),output,auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS: %d review-specific W00 sidecar sources\\n",length(z)))
