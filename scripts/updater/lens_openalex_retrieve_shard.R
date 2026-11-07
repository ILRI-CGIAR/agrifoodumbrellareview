#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i))default else args[[i+1L]]}
provider<-arg("--provider");shard<-as.integer(arg("--shard","1"))
stopifnot(provider %in% c("lens","openalex"),shard %in% 1:5)
key<-if(provider=="lens")Sys.getenv("LENS_API_TOKEN") else {
  x<-Sys.getenv("OPENALEX_API_KEY");if(!nzchar(x))x<-Sys.getenv("OPENALEX_API_TOKEN");x
}
if(!nzchar(key))stop("API credential missing")
root<-sprintf("outputs/%s/shard_%d",provider,shard)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
snapshot_path<-sprintf("outputs/%s/count/shard_%d/counts.json",provider,shard)
if(!file.exists(snapshot_path))stop("Missing API count checkpoint")
snapshot<-fromJSON(snapshot_path,simplifyVector=FALSE)
if(snapshot$status!="complete"||length(snapshot$components)!=7L)stop("Count checkpoint incomplete")
parts<-snapshot$components
now<-function()format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
retry_request<-function(req){
 for(attempt in 1:6){
  r<-tryCatch(req_perform(req|>req_timeout(180)|>req_error(is_error=function(x)FALSE)),error=identity)
  if(!inherits(r,"error")){
   st<-resp_status(r)
   if(st==200L)return(resp_body_json(r,simplifyVector=FALSE))
   detail<-substr(tryCatch(resp_body_string(r),error=function(e)""),1L,900L)
   if(!(st%in%c(429L,500L,502L,503L,504L)))stop(sprintf("%s HTTP %d: %s",provider,st,detail))
  }else detail<-conditionMessage(r)
  if(attempt==6L)stop(sprintf("%s retries exhausted: %s",provider,detail))
  Sys.sleep(min(90,2^attempt))
 }
}
get_lens<-function(q,scroll_id=NULL){
 if(is.null(scroll_id)){
  payload<-list(query=list(bool=list(
    must=list(list(query_string=list(query=q,fields=c("title","abstract","keyword"),default_operator="AND"))),
    filter=list(list(range=list(year_published=list(gte=2015,lte=2026)))),
    must_not=list(list(term=list(publication_type="dataset")))
  )),size=500L,scroll="5m")
 }else payload<-list(scroll_id=scroll_id,scroll="5m")
 req<-request("https://api.lens.org/scholarly/search")|>
   req_headers(Authorization=paste("Bearer",key),Accept="application/json")|>
   req_body_json(payload,auto_unbox=TRUE)
 retry_request(req)
}
get_openalex<-function(q,cursor){
 req<-request("https://api.openalex.org/works") |>
   req_url_query("api_key"=key,"search.title_abstract_keywords"=q,
     filter="from_publication_date:2015-01-01,to_publication_date:2026-12-31,type:!dataset",
     per_page=100,cursor=cursor)
 retry_request(req)
}
verify_record<-function(raw){
 if(provider=="lens"){
  id<-raw$lens_id
  yr<-as.integer(raw$year_published)
  if(is.null(id)||!nzchar(as.character(id)))stop("Missing Lens ID")
  if(!is.null(raw$publication_type)&&tolower(as.character(raw$publication_type))=="dataset")stop("Dataset returned by Lens")
 }else{
  id<-raw$id
  yr<-as.integer(raw$publication_year)
  if(is.null(id)||!nzchar(as.character(id)))stop("Missing OpenAlex ID")
  if(!is.null(raw$type)&&tolower(as.character(raw$type))=="dataset")stop("Dataset returned by OpenAlex")
 }
 if(length(yr)!=1L||is.na(yr)||yr<2015L||yr>2026L)stop(sprintf("%s record %s has invalid publication year %s",provider,id,paste(yr,collapse=",")))
 as.character(id)
}
manifest<-vector("list",length(parts))
for(i in seq_along(parts)){
 p<-parts[[i]];expected<-as.integer(p$total)
 if(is.na(expected)||expected<0L)stop("Invalid count checkpoint")
 path<-file.path(root,sprintf("component_%03d.jsonl",i))
 if(file.exists(path)){
   lines<-readLines(path,warn=FALSE,encoding="UTF-8")
   if(length(lines)!=expected)stop("Stale partial component file")
   ids<-vapply(lines,function(x)as.character(fromJSON(x,simplifyVector=FALSE)$source_id),character(1))
   if(anyDuplicated(ids))stop("Existing component has duplicate IDs")
   cat(sprintf("RESUMED %s %d:%d (%d records)\n",provider,shard,i,expected))
 }else{
   lines<-character();ids<-character()
   if(expected>0L){
     cursor<-"*";scroll_id<-NULL
     repeat{
       data<-if(provider=="lens")get_lens(p$query,scroll_id) else get_openalex(p$query,cursor)
       reported<-if(provider=="lens")as.integer(data$total) else as.integer(data$meta$count)
       if(length(reported)!=1L||is.na(reported)||reported!=expected)
         stop(sprintf("%s count drift in %s: expected %d, observed %s",
            provider,p$origin,expected,paste(reported,collapse=",")))
       items<-if(provider=="lens")data$data else data$results
       if(!length(items))stop(sprintf("%s unexpected empty page for %s",provider,p$origin))
       for(raw in items){
         id<-verify_record(raw)
         ids<-c(ids,id)
         entry<-list(source=provider,source_id=id,component=p$origin,raw=raw)
         lines<-c(lines,toJSON(entry,auto_unbox=TRUE,null="null",na="null"))
       }
       if(length(lines)>=expected)break
       if(provider=="lens"){
         next_scroll<-data$scroll_id
         if(is.null(next_scroll)||!nzchar(next_scroll))stop("Missing Lens scroll ID")
         scroll_id<-next_scroll
       }else{
         next_cursor<-data$meta$next_cursor
         if(is.null(next_cursor)||!nzchar(next_cursor)||identical(next_cursor,cursor))stop("Missing OpenAlex cursor")
         cursor<-next_cursor
       }
       Sys.sleep(.2)
     }
   }
   if(length(lines)!=expected||anyDuplicated(ids))
     stop(sprintf("%s component %s returned %d rows, %d unique; expected %d",provider,p$origin,length(lines),length(unique(ids)),expected))
   writeLines(lines,path,useBytes=TRUE)
   cat(sprintf("PASS %s shard %d component %d: %d records\n",provider,shard,i,length(lines)))
 }
 manifest[[i]]<-list(index=i,origin=p$origin,query_sha256=p$query_sha256,
  expected=expected,retrieved=length(lines),sha256=digest(file=path,algo="sha256",serialize=FALSE))
 write_json(list(provider=provider,shard=shard,completed=i,components=manifest[seq_len(i)]),
   file.path(root,"progress.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
}
summary<-list(provider=provider,shard=shard,status="complete",components=manifest,
  occurrences=sum(vapply(manifest,function(x)x$retrieved,integer(1))),finished=now())
write_json(summary,file.path(root,"manifest.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS %s retrieval shard %d: %d occurrences\n",provider,shard,summary$occurrences))
