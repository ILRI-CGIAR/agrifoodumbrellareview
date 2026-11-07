#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
i<-match("--provider",args)
provider<-if(is.na(i))stop("--provider required") else args[[i+1L]]
stopifnot(provider%in%c("lens","openalex"))
root<-file.path("outputs",provider)
count_files<-sprintf("outputs/%s/count/shard_%d/counts.json",provider,1:5)
if(!all(file.exists(count_files)))stop("Count checkpoint missing")
counts<-lapply(count_files,fromJSON,simplifyVector=FALSE)
if(any(!vapply(counts,function(x)identical(x$status,"complete")&&length(x$components)==7L,logical(1))))stop("Count checkpoint incomplete")
expected<-sum(vapply(counts,function(x)sum(vapply(x$components,function(z)as.integer(z$total),integer(1))),integer(1)))
if(!is.finite(expected)||expected<0L)stop("Invalid count total")
manifests<-lapply(1:5,function(i)fromJSON(sprintf("outputs/%s/shard_%d/manifest.json",provider,i),simplifyVector=FALSE))
if(any(!vapply(manifests,function(x)identical(x$status,"complete")&&length(x$components)==7L,logical(1))))stop("Missing completed shard")
dir.create(root,recursive=TRUE,showWarnings=FALSE)
con<-file(file.path(root,"unique_records.jsonl"),"wt",encoding="UTF-8")
seen<-new.env(hash=TRUE,parent=emptyenv())
provenance<-new.env(hash=TRUE,parent=emptyenv())
raw_count<-0L
for(shard in 1:5){
 for(comp in manifests[[shard]]$components){
  path<-sprintf("outputs/%s/shard_%d/component_%03d.jsonl",provider,shard,as.integer(comp$index))
  if(!file.exists(path)||digest(file=path,algo="sha256",serialize=FALSE)!=comp$sha256)stop(sprintf("Missing/changed %s",path))
  lines<-readLines(path,warn=FALSE,encoding="UTF-8")
  if(length(lines)!=as.integer(comp$expected))stop(sprintf("Incomplete component %s",path))
  ids<-character()
  for(line in lines){
   record<-fromJSON(line,simplifyVector=FALSE)
   id<-as.character(record$source_id)
   if(length(id)!=1L||!nzchar(id))stop("Missing source identifier")
   ids<-c(ids,id);raw_count<-raw_count+1L
   if(!exists(id,envir=seen,inherits=FALSE)){
     assign(id,TRUE,envir=seen)
     assign(id,c(comp$origin),envir=provenance)
     writeLines(line,con,useBytes=TRUE)
   }else{
     assign(id,c(get(id,envir=provenance,inherits=FALSE),comp$origin),envir=provenance)
   }
  }
  if(anyDuplicated(ids))stop("Duplicated identifiers within component")
 }
}
close(con)
if(raw_count!=expected)stop(sprintf("Raw count %d != %d",raw_count,expected))
ids<-ls(seen,all.names=TRUE)
p<-file(file.path(root,"source_id_provenance.jsonl"),"wt",encoding="UTF-8")
for(id in ids)writeLines(toJSON(list(source_id=id,components=unique(get(id,envir=provenance,inherits=FALSE))),auto_unbox=TRUE),p,useBytes=TRUE)
close(p)
summary<-list(provider=provider,publication_years="2015-2026",excluded_type="dataset",
  count_components=35L,raw_component_hits=raw_count,unique_source_ids=length(ids),
  duplicate_occurrences=raw_count-length(ids),deduplication_key=if(provider=="lens")"lens_id" else "OpenAlex work ID",
  fields="title, abstract and keywords",proximity_translation="4 Scopus ordered W/n clauses approximated by AND",
  wildcard_translation=if(provider=="openalex")"Scopus truncation wildcard stripped" else "Scopus truncation retained",
  record_sha256=digest(file=file.path(root,"unique_records.jsonl"),algo="sha256",serialize=FALSE))
write_json(summary,file.path(root,"dedup_summary.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS %s: raw %d unique %d duplicates %d\n",provider,raw_count,length(ids),raw_count-length(ids)))
