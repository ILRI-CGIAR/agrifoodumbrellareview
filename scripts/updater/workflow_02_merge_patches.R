#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
previous<-arg("--previous","")
current<-arg("--current")
output<-arg("--output")
if(is.null(current)||is.null(output)) stop("Required: --current --output",call.=FALSE)

readjl<-function(p){
  if(!nzchar(p)||!file.exists(p)) return(list())
  x<-readLines(p,warn=FALSE,encoding="UTF-8");x<-x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
writejl<-function(xs,p){
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  con<-file(p,"wt",encoding="UTF-8");on.exit(close(con))
  if(length(xs)) for(x in xs) writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
}
rid<-function(x) as.character(x$record_id)

prev<-readjl(previous); cur<-readjl(current)
store<-list()
if(length(prev)) for(x in prev) store[[rid(x)]]<-x
if(length(cur)) for(x in cur){
  id<-rid(x); old<-store[[id]]
  if(is.null(old)){ store[[id]]<-x; next }
  if(!is.null(x$title)) old$title<-x$title
  if(!is.null(x$abstract)) old$abstract<-x$abstract
  if(!is.null(x$author_keywords)) old$author_keywords<-x$author_keywords
  if(!is.null(x$input_doi)) old$input_doi<-x$input_doi
  old$metadata_enrichment<-x$metadata_enrichment
  old$audit<-x$audit
  store[[id]]<-old
}
ids<-sort(names(store))
writejl(unname(store[ids]),output)
cat(sprintf("PASS: merged Workflow 02 cumulative patch: %d prior + %d current -> %d records\n",length(prev),length(cur),length(ids)))
