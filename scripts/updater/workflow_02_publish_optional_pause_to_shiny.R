#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(googlesheets4);library(jsonlite)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Missing ",k);args[[i+1L]]}
ss<-arg("--sheet-id"); run<-arg("--source-run-id"); credential<-arg("--credential")
inventory<-arg("--inventory")
if(!grepl("^[A-Za-z0-9_-]{15,150}$",ss)||!grepl("^[0-9]+$",run))stop("Invalid W02 identity")
gs4_auth(path=credential)
metadata<-read_sheet(ss,sheet="review_metadata",col_types="cc")
if(!all(c("key","value") %in% names(metadata)))stop("Invalid review metadata")
review<-as.character(metadata$value[match("review_id",metadata$key)])
if(length(review)!=1L||is.na(review)||!nzchar(review))stop("Missing review ID")
inv<-fromJSON(inventory)
if(is.null(inv$canonical_records)||is.null(inv$missing_abstract))stop("Invalid inventory")
tab<-"w02_optional_pause"
cols<-c("source_run_id","review_id","google_sheet_id","missing_abstracts","canonical_records","status","created_at_utc")
row<-data.frame(source_run_id=run,review_id=review,google_sheet_id=ss,
  missing_abstracts=as.character(inv$missing_abstract),
  canonical_records=as.character(inv$canonical_records),
  status="waiting_optional_abstracts",
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  stringsAsFactors=FALSE)
tabs<-sheet_names(ss)
if(!tab %in% tabs){
  googlesheets4::sheet_write(row,ss=ss,sheet=tab)
}else{
  old<-read_sheet(ss,sheet=tab,col_types="c")
  if(!all(cols %in% names(old)))stop("Invalid W02 optional pause tab")
  if(any(as.character(old$source_run_id)==run)){
    cat("PASS: W02 optional pause already published for this run\n")
    quit(save="no",status=0)
  }
  googlesheets4::sheet_append(ss,data=row,sheet=tab)
}
cat(sprintf("PASS: W02 optional pause published: run %s, %s missing abstracts\n",
            run,as.character(inv$missing_abstract)))
