#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(jsonlite);library(digest)})
root<-"outputs/wos"
expected_partitions<-35L
count_summaries<-lapply(1:5,function(j)fromJSON(sprintf("%s/count/shard_%d/counts.json",root,j),simplifyVector=FALSE))
expected<-sum(vapply(count_summaries,function(x)
 sum(vapply(x$summaries,function(y)as.integer(y$total_results),integer(1))),integer(1)))
if(expected!=24084L)stop(sprintf("Expected saved WoS sum 24084 but got %d",expected))
manifests<-lapply(1:5,function(j)fromJSON(sprintf("%s/shard_%d/manifest.json",root,j),simplifyVector=FALSE))
if(!all(vapply(manifests,function(x)identical(x$status,"complete"),logical(1))))stop("WoS shard incomplete")
if(sum(vapply(manifests,function(x)length(x$components),integer(1)))!=expected_partitions)stop("Wrong number of WoS components")
dir.create(root,recursive=TRUE,showWarnings=FALSE)
out<-file.path(root,"unique_records.jsonl")
pc<-file(out,"wt",encoding="UTF-8")
seen<-new.env(parent=emptyenv(),hash=TRUE)
origins<-new.env(parent=emptyenv(),hash=TRUE)
raw_count<-0L
for(shard in seq_len(5L)){
  man<-manifests[[shard]]
  for(component in man$components){
    path<-file.path(root,sprintf("shard_%d/component_%03d.jsonl",shard,as.integer(component$component)))
    if(!file.exists(path)||!identical(digest(file=path,algo="sha256",serialize=FALSE),component$sha256))stop(sprintf("Missing/invalid partition: %s",path))
    rows<-readLines(path,warn=FALSE,encoding="UTF-8")
    if(length(rows)!=as.integer(component$expected))stop(sprintf("Count mismatch %s",path))
    for(line in rows){
      rec<-fromJSON(line,simplifyVector=FALSE)
      uid<-as.character(rec$uid)
      if(length(uid)!=1L||!nzchar(uid))stop(sprintf("Invalid UID from %s",path))
      raw_count<-raw_count+1L
      if(!exists(uid,envir=seen,inherits=FALSE)){
        assign(uid,TRUE,envir=seen)
        assign(uid,c(rec$source_component),envir=origins)
        writeLines(line,pc,useBytes=TRUE)
      }else{
        assign(uid,c(get(uid,envir=origins,inherits=FALSE),rec$source_component),envir=origins)
      }
    }
  }
}
close(pc)
if(raw_count!=expected)stop(sprintf("Raw occurrence mismatch %d != %d",raw_count,expected))
uids<-ls(seen,all.names=TRUE)
pfile<-file.path(root,"uid_component_provenance.jsonl")
con<-file(pfile,"wt",encoding="UTF-8")
for(uid in uids)writeLines(toJSON(list(uid=uid,components=unique(get(uid,envir=origins,inherits=FALSE))),auto_unbox=TRUE),con,useBytes=TRUE)
close(con)
report<-list(api_product="Web of Science Starter API",database="WOS",year_start=2015,year_end=2026,
  count_components=expected_partitions,raw_component_hits=raw_count,unique_wos_uids=length(uids),
  duplicate_component_occurrences=raw_count-length(uids),deduplication_key="WoS UID",
  metadata_scope="Starter bibliographic fields, excluding abstracts",
  unique_records_sha256=digest(file=out,algo="sha256",serialize=FALSE),
  provenance_sha256=digest(file=pfile,algo="sha256",serialize=FALSE))
write_json(report,file.path(root,"dedup_summary.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
cat(sprintf("PASS: WoS raw hits=%d; unique WoS UIDs=%d; duplicate occurrences=%d\n",
  raw_count,length(uids),raw_count-length(uids)))
