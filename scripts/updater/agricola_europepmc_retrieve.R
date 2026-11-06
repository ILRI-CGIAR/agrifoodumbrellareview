#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
root<-"outputs/agricola"
dir.create(root,recursive=TRUE,showWarnings=FALSE)
countfiles<-sprintf("%s/count/shard_%d/counts.json",root,1:5)
if(!all(file.exists(countfiles)))stop("Missing AGRICOLA counts")
states<-lapply(countfiles,fromJSON,simplifyVector=FALSE)
if(!all(vapply(states,function(z)isTRUE(z$complete)&&z$pending==0L&&length(z$errors)==0L,logical(1))))stop("Count snapshots not complete")
parts<-unlist(lapply(states,function(z)z$summaries),recursive=FALSE)
if(length(parts)!=35L)stop(sprintf("Expected 35 queries, got %d",length(parts)))
expected<-sum(vapply(parts,function(p)as.integer(p$total_results),integer(1)))
if(expected!=479L)stop(sprintf("Expected 479 occurrences, got %d",expected))
endpoint<-"https://www.ebi.ac.uk/europepmc/webservices/rest/search"
query_page<-function(q,cursor){
 for(attempt in 1:5){
  req<-request(endpoint)|>req_headers(Accept="application/json")|>
    req_url_query(query=q,format="json",resultType="core",pageSize=1000,
                  cursorMark=cursor,synonym="false")|>
    req_timeout(120)|>req_error(is_error=function(r)FALSE)
  resp<-tryCatch(req_perform(req),error=identity)
  if(!inherits(resp,"error")){
    st<-resp_status(resp)
    if(st==200L){
      out<-resp_body_json(resp,simplifyVector=FALSE)
      if(!is.null(out$errCode)||is.null(out$hitCount)||is.null(out$resultList))
        stop("Invalid Europe PMC API payload")
      return(out)
    }
    detail<-substr(tryCatch(resp_body_string(resp),error=function(e)""),1,700)
    if(!(st%in%c(429L,500L,502L,503L,504L)))
      stop(sprintf("Europe PMC HTTP %d: %s",st,detail))
  } else detail<-conditionMessage(resp)
  if(attempt==5L)stop(sprintf("API retry exhaustion: %s",detail))
  Sys.sleep(2^attempt)
 }
}
seen<-new.env(hash=TRUE,parent=emptyenv())
provenance<-new.env(hash=TRUE,parent=emptyenv())
output<-file.path(root,"unique_records.jsonl")
con<-file(output,"wt",encoding="UTF-8")
n_raw<-0L
audit<-list()
for(i in seq_along(parts)){
 p<-parts[[i]];expect<-as.integer(p$total_results);origin<-as.character(p$origin)
 rows<-list()
 if(expect>0L){
  cursor<-"*"
  repeat{
   data<-query_page(p$query,cursor)
   reported<-as.integer(data$hitCount)
   if(is.na(reported)||reported!=expect)stop(sprintf("AGRICOLA count changed %s: %d -> %d",origin,expect,reported))
   got<-data$resultList$result
   if(is.null(got)||!length(got))stop(sprintf("Empty page before completing %s",origin))
   rows<-c(rows,got)
   if(length(rows)>=expect)break
   next_cursor<-data$nextCursorMark
   if(is.null(next_cursor)||identical(next_cursor,cursor))stop(sprintf("No further page cursor for %s",origin))
   cursor<-next_cursor
   Sys.sleep(.15)
  }
 }
 if(length(rows)!=expect)stop(sprintf("Incomplete component %s: %d/%d",origin,length(rows),expect))
 ids<-if(length(rows))vapply(rows,function(z)paste0(z$source,":",z$id),character(1)) else character()
 if(any(!vapply(rows,function(z)identical(z$source,"AGR"),logical(1))))stop(sprintf("Non-AGRICOLA hit for %s",origin))
 if(anyDuplicated(ids))stop(sprintf("Duplicate source IDs within component %s",origin))
 for(j in seq_along(rows)){
  uid<-ids[[j]]
  n_raw<-n_raw+1L
  if(exists(uid,envir=seen,inherits=FALSE)){
    assign(uid,c(get(uid,envir=provenance,inherits=FALSE),origin),envir=provenance)
  } else {
    assign(uid,TRUE,envir=seen)
    assign(uid,c(origin),envir=provenance)
    writeLines(toJSON(list(source_uid=uid,source="AGR",source_component=origin,raw_europepmc_core=rows[[j]]),
                      auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
  }
 }
 audit[[i]]<-list(index=i,origin=origin,expected=expect,retrieved=length(rows),
                   query_sha256=p$query_sha256)
 cat(sprintf("PASS component %d %s: %d/%d hits\n",i,origin,length(rows),expect))
}
close(con)
if(n_raw!=expected)stop("Total raw occurrences do not reconcile")
ids<-ls(seen,all.names=TRUE)
prov_path<-file.path(root,"source_id_provenance.jsonl")
pc<-file(prov_path,"wt",encoding="UTF-8")
for(id in ids)writeLines(toJSON(list(source_uid=id,components=unique(get(id,envir=provenance,inherits=FALSE))),
                             auto_unbox=TRUE),pc,useBytes=TRUE)
close(pc)
report<-list(source="Europe PMC AGR subset (AGRICOLA)",database_code="AGR",
  publication_years="2015-2026",query_partitions=length(parts),raw_hits=n_raw,
  unique_source_ids=length(ids),duplicate_occurrences=n_raw-length(ids),
  field_scope="TITLE_ABS; Scopus W/n broadened to AND (four occurrences)",
  search_translation_limitation="Europe PMC AGR-indexed subset does not imply comprehensive AGRICOLA database coverage",
  unique_records_sha256=digest(file=output,algo="sha256",serialize=FALSE),
  provenance_sha256=digest(file=prov_path,algo="sha256",serialize=FALSE),
  checked_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
write_json(report,file.path(root,"dedup_summary.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
write_json(audit,file.path(root,"component_audit.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS AGRICOLA: raw=%d unique=%d duplicate_occurrences=%d\n",n_raw,length(ids),n_raw-length(ids)))
