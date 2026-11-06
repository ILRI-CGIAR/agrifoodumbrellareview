#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
input<-arg("--input");report<-arg("--report");queue<-arg("--missing-both-queue")
if(any(vapply(list(input,report,queue),is.null,logical(1)))) stop("Required: --input --report --missing-both-queue",call.=FALSE)
`%||%`<-function(x,y)if(is.null(x))y else x
clean<-function(x){if(is.null(x)||!length(x))return(NULL);s<-trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])));if(is.na(s)||!nzchar(s))NULL else s}
miss<-function(x)is.null(clean(x))
con<-file(input,"rt",encoding="UTF-8");on.exit(close(con))
n<-mt<-ma<-mk<-mb<-doi_n<-0L;q<-list()
repeat{
  line<-readLines(con,n=1L,warn=FALSE);if(!length(line))break;if(!nzchar(trimws(line)))next
  r<-fromJSON(line,simplifyVector=FALSE);n<-n+1L
  tmiss<-miss((r$canonical%||%list())$title);amiss<-miss((r$canonical%||%list())$abstract);kvals<-(r$canonical%||%list())$author_keywords;kclean<-if(is.null(kvals))character()else trimws(as.character(unlist(kvals,use.names=FALSE)));kmiss<-!length(kclean[!is.na(kclean)&nzchar(kclean)]);d<-clean((r$canonical%||%list())$doi)
  if(!is.null(d))doi_n<-doi_n+1L
  if(tmiss)mt<-mt+1L;if(amiss)ma<-ma+1L;if(kmiss)mk<-mk+1L
  if(tmiss&&amiss){
    mb<-mb+1L
    q[[length(q)+1L]]<-list(record_id=clean((r$identity%||%list())$record_id),doi=d,metadata_enrichment=r$metadata_enrichment)
  }
}
close(con);on.exit(NULL,add=FALSE)
dir.create(dirname(queue),recursive=TRUE,showWarnings=FALSE);qc<-file(queue,"wt",encoding="UTF-8");if(length(q))for(x in q)writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null"),qc,useBytes=TRUE);close(qc)
out<-list(schema="living-evidence-map-workflow02-inventory-v1",status="PASS",canonical_records=n,records_with_doi=doi_n,missing_title=mt,missing_abstract=ma,missing_author_keywords=mk,missing_both=mb,input_sha256=digest(file=input,algo="sha256",serialize=FALSE),missing_both_queue_sha256=digest(file=queue,algo="sha256",serialize=FALSE),created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
dir.create(dirname(report),recursive=TRUE,showWarnings=FALSE);writeLines(toJSON(out,auto_unbox=TRUE,pretty=TRUE,null="null"),report,useBytes=TRUE)
cat(sprintf("PASS: Workflow 02 inventory: %d records; missing title=%d abstract=%d author_keywords=%d both=%d\n",n,mt,ma,mk,mb))