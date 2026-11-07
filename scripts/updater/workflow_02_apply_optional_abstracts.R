#!/usr/bin/env Rscript
# Optional W02 edits are validated against the frozen post-enrichment queue,
# then merged into the normal sparse patch before its existing exact replay.
suppressPackageStartupMessages({library(googlesheets4);library(jsonlite);library(digest)})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k) {i<-match(k,args); if(is.na(i)||i==length(args)) stop("Missing ",k); args[[i+1L]]}
sheet <- arg("--sheet-id"); run <- arg("--source-run-id")
credential <- arg("--credential"); patch_path <- arg("--patch")
input_path <- arg("--input"); output <- arg("--output-patch")
if(!grepl("^[0-9]+$",run)||!grepl("^[A-Za-z0-9_-]{15,150}$",sheet))stop("Invalid W02 review identity")
gs4_auth(path=credential)
tabs <- sheet_names(sheet)
if(!"queue_w02_missing_abstracts" %in% tabs) {
  file.copy(patch_path,output,overwrite=TRUE)
  cat("PASS: optional abstract queue not requested; original W02 patch preserved\n")
  quit(save="no",status=0)
}
q <- read_sheet(sheet,sheet="queue_w02_missing_abstracts",col_types="c")
required_q <- c("source_run_id","queue_sha256","record_id")
if(!all(required_q %in% names(q)) || !nrow(q))stop("Invalid optional W02 abstract queue")
if(any(is.na(q$source_run_id))||any(q$source_run_id!=run))
  stop("Optional abstract queue belongs to a different W02 run; refusing resume")
sha <- unique(q$queue_sha256)
if(length(sha)!=1L||!grepl("^[a-f0-9]{64}$",sha))stop("Invalid optional queue checksum")
if(anyDuplicated(q$record_id)||any(is.na(q$record_id)))stop("Invalid optional record IDs")
if(!"w02_missing_abstract_edits" %in% tabs)
  stop("Optional missing-abstract queue was requested but has no decisions")
d <- read_sheet(sheet,sheet="w02_missing_abstract_edits",col_types="c")
cols <- c("source_run_id","queue_sha256","record_id","abstract","decision","reviewer","provenance","saved_at_utc")
if(!all(cols %in% names(d)))stop("Malformed W02 abstract-edit ledger")
d <- d[!is.na(d$source_run_id)&!is.na(d$queue_sha256)&
       d$source_run_id==run & d$queue_sha256==sha,,drop=FALSE]
if(nrow(d)) d <- d[!duplicated(d$record_id,fromLast=TRUE),,drop=FALSE]
if(!setequal(q$record_id,d$record_id))stop("Optional W02 abstract review incomplete; save or skip all queued records")
if(any(!d$decision %in% c("add","skip")))stop("Invalid optional abstract decisions")
records <- lapply(readLines(input_path,warn=FALSE),function(x)fromJSON(x,simplifyVector=FALSE))
ids <- vapply(records,function(x)as.character(x$identity$record_id),character(1))
if(anyDuplicated(ids))stop("Duplicate canonical IDs")
input <- setNames(records,ids)
patch_lines <- readLines(patch_path,warn=FALSE)
patch_lines <- patch_lines[nzchar(trimws(patch_lines))]
patches <- lapply(patch_lines,function(x)fromJSON(x,simplifyVector=FALSE))
pids <- vapply(patches,function(x)as.character(x$record_id),character(1))
if(anyDuplicated(pids))stop("Duplicate patch IDs")
store <- setNames(patches,pids)
added <- 0L
for(i in seq_len(nrow(d))) {
  if(d$decision[[i]]=="skip")next
  id <- as.character(d$record_id[[i]])
  rec <- input[[id]]
  if(is.null(rec))stop("Abstract record missing from canonical input: ",id)
  abs <- trimws(as.character(d$abstract[[i]]))
  prov <- trimws(as.character(d$provenance[[i]]))
  if(is.na(abs)||!nzchar(abs)||is.na(prov)||!nzchar(prov))stop("Missing edited abstract or provenance: ",id)
  old <- rec$canonical$abstract
  if(!is.null(old)&&nzchar(trimws(as.character(old))))stop("Cannot overwrite a nonempty canonical abstract: ",id)
  p <- store[[id]]
  if(is.null(p)) p <- list(record_id=id,input_doi=rec$canonical$doi,
    title=NULL,abstract=NULL,author_keywords=NULL,metadata_enrichment=list(),audit=list())
  if(is.null(p$input_doi)||!nzchar(as.character(p$input_doi)))stop("Cannot apply abstract without stable DOI identity: ",id)
  if(!is.null(p$abstract))stop("Provider abstract already present; cannot replace: ",id)
  p$abstract <- list(value=abs,provider="manual_abstract",human_adjudicated=TRUE,
                     reviewer=as.character(d$reviewer[[i]]),provenance=prov,
                     queue_sha256=sha)
  store[[id]] <- p
  added <- added+1L
}
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
con <- file(output,"wt",encoding="UTF-8")
for(id in sort(names(store)))
  writeLines(toJSON(store[[id]],auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
close(con)
cat(sprintf("PASS: applied %d optional manual abstracts and %d skips; queue SHA=%s\n",
            added,sum(d$decision=="skip"),sha))
