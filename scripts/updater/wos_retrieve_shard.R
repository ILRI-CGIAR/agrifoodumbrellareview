#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
idx<-match("--shard",args)
shard<-if(is.na(idx))stop("--shard required") else as.integer(args[[idx+1L]])
stopifnot(shard %in% 1:5)
secret<-Sys.getenv("WOS_STARTER_API")
if(!nzchar(secret))stop("Missing WOS_STARTER_API")
root<-sprintf("outputs/wos/shard_%d",shard)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
count_file<-sprintf("outputs/wos/count/shard_%d/counts.json",shard)
if(!file.exists(count_file))stop(sprintf("Missing source count checkpoint %s",count_file))
snapshot<-fromJSON(count_file,simplifyVector=FALSE)
if(!isTRUE(snapshot$complete)||length(snapshot$errors)>0L||as.integer(snapshot$pending)!=0L)stop("WoS count snapshot incomplete")
parts<-snapshot$summaries
if(length(parts)<7L)stop("Unexpectedly few WoS search components")
now<-function()format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
req_page<-function(query,page,limit=50L) {
  for(attempt in seq_len(6L)) {
    req<-request("https://api.clarivate.com/apis/wos-starter/v1/documents") |>
      req_headers("X-ApiKey"=secret,Accept="application/json") |>
      req_url_query(db="WOS",q=query,page=page,limit=limit) |>
      req_timeout(120) |> req_error(is_error=function(r)FALSE)
    res<-tryCatch(req_perform(req),error=identity)
    if(!inherits(res,"error")){
      st<-resp_status(res)
      if(st==200L)return(resp_body_json(res,simplifyVector=FALSE))
      message<-substr(tryCatch(resp_body_string(res),error=function(e)""),1L,700L)
      if(!(st %in% c(429L,500L,502L,503L,504L)))
        stop(sprintf("WoS HTTP %d page %d: %s",st,page,message))
    }else message<-conditionMessage(res)
    if(attempt==6L)stop(sprintf("WoS request exhausted retries on page %d: %s",page,message))
    Sys.sleep(min(90,2^attempt))
  }
}
read_recovered<-function(path,expected){
  lines<-readLines(path,warn=FALSE,encoding="UTF-8")
  if(length(lines)!=expected)stop(sprintf("Existing checkpoint incomplete %s: %d / %d",path,length(lines),expected))
  if(length(lines)){
    eids<-vapply(lines,function(x)as.character(fromJSON(x,simplifyVector=FALSE)$uid),character(1))
    if(any(!nzchar(eids))||anyDuplicated(eids))stop(sprintf("Checkpoint UID invalid: %s",path))
  }
  lines
}
manifest<-vector("list",length(parts))
for(i in seq_along(parts)){
  p<-parts[[i]]
  expected<-as.integer(p$total_results)
  if(is.na(expected)||expected<0L)stop("Bad count in saved WoS snapshot")
  origin<-as.character(p$origin)
  out<-file.path(root,sprintf("component_%03d.jsonl",i))
  if(file.exists(out)){
    lines<-read_recovered(out,expected)
    cat(sprintf("RESUME WoS shard %d component %d (%s): %d UIDs\n",shard,i,origin,length(lines)))
  }else{
    total_pages<-if(expected==0L)0L else as.integer(ceiling(expected/50))
    lines<-character()
    if(total_pages>0L){
      for(page in seq_len(total_pages)){
        data<-req_page(p$query,page,50L)
        n<-as.integer(data$metadata$total)
        if(is.na(n)||n!=expected)stop(sprintf("WoS count changed for %s, page %d: expected %d, now %d",origin,page,expected,n))
        hits<-data$hits
        need<-min(50L,expected-(page-1L)*50L)
        if(is.null(hits)||length(hits)!=need)
          stop(sprintf("WoS page incomplete for %s: page %d got %d expected %d",origin,page,length(hits),need))
        for(hit in hits){
          uid<-as.character(hit$uid)
          if(length(uid)!=1L||!nzchar(uid))stop(sprintf("Missing WoS UID in %s page %d",origin,page))
          entry<-list(uid=uid,source_component=origin,raw_starter=hit)
          lines<-c(lines,toJSON(entry,auto_unbox=TRUE,null="null",na="null"))
        }
        Sys.sleep(0.12)
      }
    }
    if(length(lines)!=expected)stop(sprintf("WoS component %s incomplete: %d versus %d",origin,length(lines),expected))
    if(length(lines)){
      ids<-vapply(lines,function(x)as.character(fromJSON(x,simplifyVector=FALSE)$uid),character(1))
      if(anyDuplicated(ids))stop(sprintf("WoS paged results have duplicate UIDs within %s",origin))
    }
    writeLines(lines,out,useBytes=TRUE)
    cat(sprintf("PASS WoS shard %d component %d (%s): %d records\n",shard,i,origin,length(lines)))
  }
  manifest[[i]]<-list(component=i,origin=origin,expected=expected,retrieved=length(lines),query_sha256=p$query_sha256,sha256=digest(file=out,algo="sha256",serialize=FALSE))
  write_json(list(shard=shard,status="in_progress",completed=i,components=manifest[seq_len(i)],checked_at_utc=now()),
             file.path(root,"progress.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
}
report<-list(shard=shard,status="complete",components=manifest,total_retrieved=sum(vapply(manifest,function(x)x$retrieved,integer(1))),completed_at_utc=now())
write_json(report,file.path(root,"manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
cat(sprintf("PASS WoS shard %d: %d components, %d raw occurrences\n",shard,length(parts),report$total_retrieved))
