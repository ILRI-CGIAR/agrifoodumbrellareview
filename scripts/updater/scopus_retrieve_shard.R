#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
args <- commandArgs(TRUE)
getarg <- function(name,default=NULL){i<-match(name,args);if(is.na(i))default else args[[i+1L]]}
shard <- as.integer(getarg("--shard","0"))
if(is.na(shard)||!(shard %in% 0:4))stop("Shard must be 0..4")
src <- "outputs/scopus/source/search_probe.json"
if(!file.exists(src))stop("Verified search_probe.json missing")
snap <- fromJSON(src,simplifyVector=FALSE)
if(length(snap$summaries)!=200L || as.integer(snap$pending)!=0L || length(snap$errors)>0L)stop("Incomplete search-count checkpoint")
key <- Sys.getenv("SCOPUS_API_TOKEN"); inst<-Sys.getenv("SCOPUS_INSTTOKEN")
if(!nzchar(key))stop("SCOPUS_API_TOKEN missing")
root <- sprintf("outputs/scopus/shard_%d",shard)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
partitions <- which(((seq_along(snap$summaries)-1L) %% 5L)==shard)
request_page <- function(query,start,count) {
  for(attempt in seq_len(6L)) {
    req<-request("https://api.elsevier.com/content/search/scopus") |>
      req_headers("X-ELS-APIKey"=key,Accept="application/json")
    if(nzchar(inst))req<-req |> req_headers("X-ELS-Insttoken"=inst)
    req<-req |> req_url_query(query=query,start=start,count=count,view="STANDARD") |>
      req_timeout(120) |> req_error(is_error=function(resp) FALSE)
    response<-tryCatch(req_perform(req),error=identity)
    if(!inherits(response,"error")){
      status<-resp_status(response)
      if(status==200L)return(resp_body_json(response,simplifyVector=FALSE))
      detail<-substr(tryCatch(resp_body_string(response),error=function(e)""),1L,600L)
      if(!(status %in% c(429L,500L,502L,503L,504L)))stop(sprintf("Scopus HTTP %d at offset %d: %s",status,start,detail))
    }else detail<-conditionMessage(response)
    if(attempt==6L)stop(sprintf("Scopus retry exhaustion at offset %d: %s",start,detail))
    Sys.sleep(min(60,2^attempt))
  }
}
sanitize <- function(entry,origin){
  eid<-as.character(entry$eid %||% "")
  if(!nzchar(eid))stop(sprintf("Missing EID in partition %s",origin))
  list(eid=eid,scopus_id=entry[["dc:identifier"]],doi=entry[["prism:doi"]],
       title=entry[["dc:title"]],authors=entry[["dc:creator"]],
       publication=entry[["prism:publicationName"]],cover_date=entry[["prism:coverDate"]],
       document_type=entry[["subtypeDescription"]],citedby_count=entry[["citedby-count"]],
       source_partition=origin,raw_standard=entry)
}
`%||%` <- function(x,y) if(is.null(x)) y else x
summary <- vector("list",length(partitions))
for(k in seq_along(partitions)){
  n <- partitions[[k]]; p<-snap$summaries[[n]]
  total<-as.integer(p$total_results);origin<-as.character(p$origin)
  if(is.na(total)||total<0L||total>=5000L)stop(sprintf("Unsafe partition count for %s: %s",origin,total))
  path<-file.path(root,sprintf("partition_%03d.jsonl",n))
  records<-list()
  if(total>0L)for(start in seq.int(0L,total-1L,by=200L)){
    count<-min(200L,total-start)
    page<-request_page(p$query,start,count)
    now_total<-as.integer(page[["search-results"]][["opensearch:totalResults"]])
    if(is.na(now_total)||now_total!=total)stop(sprintf("Search count changed for %s: baseline=%d current=%d",origin,total,now_total))
    entries<-page[["search-results"]][["entry"]]
    if(is.null(entries)||length(entries)!=count)stop(sprintf("Unexpected page length partition %s offset %d expected %d got %d",origin,start,count,length(entries)))
    records<-c(records,lapply(entries,sanitize,origin=origin))
    Sys.sleep(0.18)
  }
  ids<-vapply(records,function(x)x$eid,character(1))
  if(length(ids)!=total || anyDuplicated(ids))stop(sprintf("Incomplete or duplicated Scopus page retrieval %s",origin))
  con<-file(path,"wt",encoding="UTF-8")
  for(rec in records) writeLines(toJSON(rec,auto_unbox=TRUE,null="null",na="null"),con)
  close(con)
  summary[[k]]<-list(index=n,origin=origin,expected=total,retrieved=length(ids),unique_eids=length(unique(ids)),sha256=digest(file=path,algo="sha256"))
  write_json(summary[seq_len(k)],file.path(root,"progress.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  cat(sprintf("PASS partition %d (%s): %d / %d records\n",n,origin,length(ids),total))
}
write_json(list(shard=shard,partitions=summary,expected=sum(vapply(summary,function(x)x$expected,integer(1)))),
           file.path(root,"manifest.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS shard %d: %d partitions; %d bibliographic hits\n",shard,length(summary),sum(vapply(summary,function(x)x$retrieved,integer(1)))))
