#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i))default else args[[i+1L]]}
provider<-arg("--provider");shard<-as.integer(arg("--shard","1"))
stopifnot(provider %in% c("lens","openalex"),shard %in% 1:5)
split_top <- function(s, op) {
  s <- trimws(s); chars <- strsplit(s, "", fixed=TRUE)[[1L]]
  needle <- paste0(" ", op, " "); w <- nchar(needle)
  dep <- 0L; quoted <- FALSE; i <- 1L; start <- 1L; out <- character()
  while (i <= length(chars)) {
    c <- chars[[i]]
    if (c == '"') quoted <- !quoted
    if (!quoted) {
      if (c == "(") dep <- dep+1L
      if (c == ")") dep <- dep-1L
      if (dep == 0L && i+w-1L <= length(chars) &&
          paste(chars[i:(i+w-1L)],collapse="") == needle) {
        out <- c(out,trimws(substr(s,start,i-1L)))
        i <- i+w; start <- i; next
      }
    }
    i <- i+1L
  }
  c(out,trimws(substr(s,start,nchar(s))))
}
unwrap <- function(s) {
  s <- trimws(s)
  repeat {
    if (!(startsWith(s,"(") && endsWith(s,")"))) return(s)
    chars <- strsplit(s,"",fixed=TRUE)[[1L]]; dep <- 0L; quoted <- FALSE; early <- FALSE
    for (i in seq_along(chars)) {
      c <- chars[[i]]
      if (c=='"') quoted <- !quoted
      if (!quoted) {
        if (c=="(") dep <- dep+1L
        if (c==")") dep <- dep-1L
        if (dep==0L && i<length(chars)) {early <- TRUE; break}
      }
    }
    if (early) return(s)
    s <- trimws(substr(s,2L,nchar(s)-1L))
  }
}
explode_or <- function(s) {
  s <- unwrap(s)
  parts <- split_top(s,"OR")
  if (length(parts)>1L) return(unlist(lapply(parts,explode_or),use.names=FALSE))
  if (grepl("^TITLE_ABS:[[:space:]]*\\(",s)) {
    inside <- sub("^TITLE_ABS:[[:space:]]*\\(","",s)
    inside <- substr(inside,1L,nchar(inside)-1L)
    inner <- explode_or(inside)
    return(vapply(inner,function(x) paste0("TITLE_ABS: ( ",x," )"),character(1)))
  }
  s
}
join_or <- function(terms) paste0("( ",paste(terms,collapse=" OR ")," )")


source_search<-paste(readLines("user_input/scopus_search_2015_onwards.txt",warn=FALSE,encoding="UTF-8"),collapse=" ")
source_search<-trimws(gsub("[[:space:]]+"," ",source_search))
if(!grepl(" AND PUBYEAR > 2014$",source_search))stop("Unexpected Scopus date filter")
source_search<-sub(" AND PUBYEAR > 2014$","",source_search)
source_search<-gsub("\\\\[*]","*",source_search)
source_search<-gsub("TITLE-ABS-KEY[[:space:]]*\\(","(",source_search)
proximity_count<-length(gregexpr("W/[0-9]+",source_search)[[1L]])
if(proximity_count!=4L)stop("Unexpected proximity clause count")
source_search<-gsub("[[:space:]]+W/[0-9]+[[:space:]]+"," AND ",source_search)
parts<-split_top(unwrap(source_search),"AND")
if(length(parts)!=3L)stop(sprintf("Search should have 3 top-level AND blocks, found %d",length(parts)))
climate<-split_top(unwrap(parts[[1L]]),"OR")
agriculture<-split_top(unwrap(parts[[3L]]),"OR")
review<-unwrap(parts[[2L]])
if(length(climate)!=5L||length(agriculture)!=7L)stop("Expected 5 climate blocks x 7 agricultural blocks")
secret<-if(provider=="lens")Sys.getenv("LENS_API_TOKEN") else {
  k<-Sys.getenv("OPENALEX_API_KEY");if(!nzchar(k))k<-Sys.getenv("OPENALEX_API_TOKEN");k
}
if(!nzchar(secret))stop(sprintf("Missing %s API credential",provider))
root<-sprintf("outputs/%s/count/shard_%d",provider,shard)
dir.create(root,recursive=TRUE,showWarnings=FALSE)
out<-file.path(root,"counts.json")
queries<-lapply(seq_along(agriculture),function(i)list(origin=paste0(shard,":",i),
 climate=unwrap(climate[[shard]]),review=review,agri=unwrap(agriculture[[i]])))
# For both interfaces convert legacy Scopus truncation into stem-like text terms;
# wildcards in OpenAlex text search are not guaranteed. Report this deviation explicitly.
build_text<-function(t){
  s<-paste0("(",t$climate,") AND (",t$review,") AND (",t$agri,")")
  if(provider=="openalex")s<-gsub("[*]","",s)
  s
}
fetch_count<-function(t){
 q<-build_text(t)
 if(provider=="lens"){
  payload<-list(query=list(bool=list(
     must=list(list(query_string=list(query=q,fields=c("title","abstract","keyword"),default_operator="AND"))),
     filter=list(list(range=list(year_published=list(gte=2015,lte=2026)))),
     must_not=list(list(term=list(publication_type="dataset")))
  )),size=0L)
  request_obj<-request("https://api.lens.org/scholarly/search") |>
    req_headers(Authorization=paste("Bearer",secret),Accept="application/json") |>
    req_body_json(payload,auto_unbox=TRUE)
 }else{
  request_obj<-request("https://api.openalex.org/works") |>
    req_url_query("api_key"=secret,
      "search.title_abstract_keywords"=q,
      filter="from_publication_date:2015-01-01,to_publication_date:2026-12-31,type:!dataset",
      per_page=1)
 }
 for(attempt in 1:4){
  r<-tryCatch(req_perform(request_obj |> req_timeout(120) |>
      req_error(is_error=function(x)FALSE)),error=identity)
  if(!inherits(r,"error")){
   st<-resp_status(r)
   body<-tryCatch(resp_body_json(r,simplifyVector=FALSE),error=function(e)list())
   if(st==200L){
     total<-if(provider=="lens")body$total else body$meta$count
     if(!is.null(total))return(list(http=st,total=as.integer(total),query=q))
   }
   msg<-substr(tryCatch(resp_body_string(r),error=function(e)""),1L,900L)
   if(!(st %in% c(429L,500L,502L,503L,504L)))return(list(http=st,error=msg,query=q))
  }else msg<-conditionMessage(r)
  if(attempt<4)Sys.sleep(2^attempt)
 }
 list(http="transport_error",error=msg,query=q)
}
result<-list(provider=provider,shard=shard,status="incomplete",dataset_exclusion=if(provider=="lens")"must_not term publication_type=dataset" else "type:!dataset",
 year_filter="2015-2026",field_scope="title, abstract and keywords",
 proximity_translation="four Scopus W/n clauses replaced by AND",
 wildcard_translation=if(provider=="openalex")"asterisk truncation stripped; OpenAlex stemming" else "asterisk truncation retained",
 components=list(),errors=list())
save<-function()write_json(result,out,pretty=TRUE,auto_unbox=TRUE,null="null")
for(i in seq_along(queries)){
 t<-queries[[i]]
 a<-fetch_count(t)
 cat(sprintf("%s shard %d component %d HTTP %s total %s\n",provider,shard,i,
    as.character(a$http),if(is.null(a$total))"NA" else as.character(a$total)))
 if(!identical(a$http,200L)||is.null(a$total)){
  result$errors[[length(result$errors)+1L]]<-list(origin=t$origin,http=a$http,error=a$error)
  save();break
 }
 result$components[[length(result$components)+1L]]<-list(
    origin=t$origin,query=a$query,total=as.integer(a$total),
    query_sha256=digest(a$query,algo="sha256",serialize=FALSE))
 save()
 Sys.sleep(0.2)
}
result$status<-if(!length(result$errors)&&length(result$components)==7L)"complete" else "failed"
save()
cat(sprintf("%s shard %d result %s: %d successful components; %d summed hits\n",
 provider,shard,result$status,length(result$components),
 sum(vapply(result$components,function(x)x$total,integer(1)))))
if(result$status!="complete")stop("Search count probe failed; inspect counts.json")
