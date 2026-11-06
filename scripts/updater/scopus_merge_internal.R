#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(jsonlite);library(digest)})
root <- "outputs/scopus"
probes <- fromJSON(file.path(root,"source","search_probe.json"),simplifyVector=FALSE)
expected <- sum(vapply(probes$summaries,function(x)as.integer(x$total_results),integer(1)))
parts <- sort(list.files(root,pattern="^partition_[0-9]{3}\\.jsonl$",recursive=TRUE,full.names=TRUE))
if(length(parts)!=200L)stop(sprintf("Expected 200 partition files, found %d",length(parts)))
shards <- lapply(0:4,function(i)fromJSON(file.path(root,sprintf("shard_%d",i),"manifest.json"),simplifyVector=FALSE))
if(sum(vapply(shards,function(s)length(s$partitions),integer(1)))!=200L)stop("Incomplete manifests")
out <- file.path(root,"unique_records.jsonl")
con <- file(out,"wt",encoding="UTF-8");on.exit(close(con))
seen<-new.env(hash=TRUE,parent=emptyenv())
sources<-new.env(hash=TRUE,parent=emptyenv())
total<-0L;duplicate_count<-0L
for(f in parts){
  expected_sha <- NULL
  for(sh in shards)for(man in sh$partitions)if(endsWith(f,sprintf("partition_%03d.jsonl",as.integer(man$index))))expected_sha<-man$sha256
  if(is.null(expected_sha)||digest(file=f,algo="sha256")!=expected_sha)stop(sprintf("Checksum mismatch: %s",f))
  lines<-readLines(f,warn=FALSE,encoding="UTF-8")
  for(line in lines){
    rec<-fromJSON(line,simplifyVector=FALSE)
    eid<-as.character(rec$eid)
    if(!nzchar(eid))stop("Missing EID")
    total<-total+1L
    if(exists(eid,envir=seen,inherits=FALSE)){
      duplicate_count<-duplicate_count+1L
      assign(eid,c(get(eid,envir=sources,inherits=FALSE),rec$source_partition),envir=sources)
    }else{
      assign(eid,TRUE,envir=seen)
      assign(eid,c(rec$source_partition),envir=sources)
      writeLines(line,con)
    }
  }
}
close(con);on.exit(NULL)
if(total!=expected)stop(sprintf("Raw count discrepancy: expected %d retrieved %d",expected,total))
eids<-ls(seen,all.names=TRUE)
provenance<-file.path(root,"eid_partitions.jsonl")
pcon<-file(provenance,"wt");for(id in eids)writeLines(toJSON(list(eid=id,partitions=unique(get(id,envir=sources,inherits=FALSE))),auto_unbox=TRUE),pcon);close(pcon)
report<-list(search_year_from=2015L,query_partitions=200L,raw_hits=total,unique_eids=length(eids),duplicate_occurrences=duplicate_count,
             deduplication_key="Scopus EID",scopus_view="STANDARD",
             unique_records_sha256=digest(file=out,algo="sha256"),eids_provenance_sha256=digest(file=provenance,algo="sha256"))
write_json(report,file.path(root,"dedup_summary.json"),auto_unbox=TRUE,pretty=TRUE)
cat(sprintf("PASS: raw hits=%d, unique Scopus EIDs=%d, duplicate occurrences=%d\n",total,length(eids),duplicate_count))
