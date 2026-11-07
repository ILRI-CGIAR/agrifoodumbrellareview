#!/usr/bin/env Rscript
# ILRI W00 input-format bridge only. Existing W01 algorithms stay unchanged.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
or_else<-function(a,b)if(is.null(a))b else a
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Required: ",k);args[[i+1L]]}
p<-fromJSON(arg("--pointer"),simplifyVector=FALSE)
if(!identical(p$schema,"ilri-w00-validated-artifacts-v1")||
   !identical(p$status,"validated")||!identical(p$first_run,TRUE)||
   !identical(p$branch,Sys.getenv("WORKFLOW01_BRANCH")))stop("Invalid ILRI W00 pointer")
api_dir<-arg("--api-dir");manual_dir<-arg("--manual-dir");root<-arg("--output-root")
api_exp<-c(scopus=10919L,lens=11241L,openalex=9105L,agricola=192L,wos=9338L)
man_exp<-c(cab=4951L,proquest=528L,lilacs=87L)
if(!setequal(names(p$api_sources),names(api_exp))||!setequal(names(p$manual_sources),names(man_exp))||
  as.integer(p$expected_total_records)!=sum(api_exp)+sum(man_exp))stop("Pointer source-set mismatch")
for(s in names(api_exp))if(as.integer(p$api_sources[[s]]$expected_records)!=api_exp[[s]])stop("API source count mismatch: ",s)
for(s in names(man_exp))if(as.integer(p$manual_sources[[s]]$expected_records)!=man_exp[[s]])stop("Manual source count mismatch: ",s)
scalar<-function(x)if(is.null(x)||!length(x))NULL else as.character(x[[1L]])
read_checked<-function(file,s,n,idfun) {
 if(!file.exists(file))stop("Missing W00 artifact: ",file)
 con<-file(file,"rt",encoding="UTF-8");on.exit(close(con),add=TRUE)
 rows<-vector("list",n);ids<-character(n)
 for(i in seq_len(n)) {
  line<-readLines(con,n=1L,warn=FALSE)
  if(!length(line)||!nzchar(trimws(line)))stop(s,": too few records")
  x<-fromJSON(line,simplifyVector=FALSE)
  id<-idfun(x)
  if(is.null(id)||length(id)!=1L||!nzchar(id))stop(s,": invalid native ID on row ",i)
  rows[[i]]<-x;ids[[i]]<-id
 }
 if(length(readLines(con,n=1L,warn=FALSE)))stop(s,": extra records")
 if(anyDuplicated(ids))stop(s,": repeated native ID")
 list(rows=rows,sha256=digest(file=file,algo="sha256",serialize=FALSE))
}
api<-list()
for(s in names(api_exp)) {
 idfun<-switch(s,
 scopus=function(x)scalar(or_else(x$eid,x$scopus_id)),
 lens=function(x)scalar(or_else(x$source_id,x$raw$lens_id)),
 openalex=function(x)scalar(or_else(x$source_id,x$raw$id)),
 agricola=function(x)scalar(or_else(x$source_uid,x$raw_europepmc_core$id)),
 wos=function(x)scalar(x$uid))
 api[[s]]<-read_checked(file.path(api_dir,s,"unique_records.jsonl"),s,api_exp[[s]],idfun)
 cat(sprintf("PASS %s: %d native records\n",s,api_exp[[s]]))
}
dir.create(root,recursive=TRUE,showWarnings=FALSE)
emit<-function(s,fun,prefix="response") {
 rd<-file.path(root,s,"source_child","raw")
 dir.create(rd,recursive=TRUE,showWarnings=FALSE)
 rows<-api[[s]]$rows
 for(i in seq(1L,length(rows),by=250L)) {
  j<-min(length(rows),i+249L)
  writeLines(toJSON(fun(rows[i:j]),auto_unbox=TRUE,null="null",na="null",digits=NA),
    file.path(rd,sprintf("%s_%06d.json",prefix,i)))
 }
}
emit("scopus",function(xs)list("search-results"=list(entry=lapply(xs,function(x)x$raw_standard))))
emit("openalex",function(xs)list(results=lapply(xs,function(x)x$raw)))
emit("agricola",function(xs)list(resultList=list(result=lapply(xs,function(x)x$raw_europepmc_core))))
emit("wos",function(xs)list(hits=lapply(xs,function(x)x$raw_starter)))
ld<-file.path(root,"lens","source_child");dir.create(ld,recursive=TRUE,showWarnings=FALSE)
con<-file(file.path(ld,"records.jsonl"),"wt",encoding="UTF-8")
for(r in api$lens$rows){
 z<-r$raw;lid<-scalar(or_else(r$source_id,z$lens_id))
 doi<-scalar(z$doi)
 if(is.null(doi))for(x in or_else(z$external_ids,list()))
   if(identical(tolower(or_else(scalar(x$type),"")),"doi")){doi<-scalar(x$value);break}
 journal<-if(is.list(z$source))z$source$title else z$source
 obj<-list(identity=list(lens_id=lid),lens=list(raw_payload=z),
  canonical=list(title=z$title,abstract=z$abstract,doi=doi,
   authors=z$authors,year=z$year_published,source=journal,
   volume=z$volume,issue=z$issue))
 writeLines(toJSON(obj,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
}
close(con)
manual_hash<-list()
for(s in names(man_exp)) {
 src<-file.path(manual_dir,"current_sources",s,"records.jsonl")
 q<-read_checked(src,s,man_exp[[s]],function(x) {
  if(!identical(scalar(x$source$provider),s))stop(s,": provider mismatch")
  scalar(x$sidecar_identity$sidecar_record_id)
 })
 dest<-file.path("current_sources",s,"records.jsonl")
 dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
 if(!file.copy(src,dest,overwrite=TRUE))stop("Copy failed: ",s)
 manual_hash[[s]]<-q$sha256
 cat(sprintf("PASS %s: %d manual RIS records\n",s,man_exp[[s]]))
}
audit<-list(status="validated",schema="ilri-w00-w01-handoff-v1",
 counts=as.list(c(api_exp,man_exp)),api_sha256=lapply(api,function(x)x$sha256),
 manual_sha256=manual_hash,no_zenodo_writes=TRUE)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE),
 file.path(root,"ilri_handoff_audit.json"))
cat(sprintf("PASS: ILRI W00 input handoff, %d manifestations\n",sum(api_exp)+sum(man_exp)))
