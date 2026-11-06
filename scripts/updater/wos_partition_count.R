#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
argv <- commandArgs(trailingOnly=TRUE)
i <- match("--shard",argv); shard <- if(is.na(i)) 1L else as.integer(argv[[i+1L]])
stopifnot(shard %in% 1:5)
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
  if (grepl("^TS=[[:space:]]*\\(",s)) {
    inside <- sub("^TS=[[:space:]]*\\(","",s)
    inside <- substr(inside,1L,nchar(inside)-1L)
    inner <- explode_or(inside)
    return(vapply(inner,function(x) paste0("TS= ( ",x," )"),character(1)))
  }
  s
}
join_or <- function(terms) paste0("( ",paste(terms,collapse=" OR ")," )")

input<-paste(readLines("user_input/scopus_search_2015_onwards.txt",warn=FALSE,encoding="UTF-8"),collapse=" ")
input<-trimws(gsub("[[:space:]]+"," ",input))
input<-gsub("\\\\[*]","*",input)
input<-sub(" AND PUBYEAR > 2014$","",input)
input<-gsub("TITLE-ABS-KEY[[:space:]]*\\(","TS=(",input)
input<-gsub("[[:space:]]+W/([0-9]+)[[:space:]]+"," NEAR/\\1 ",input)
main<-split_top(unwrap(input),"AND")
stopifnot(length(main)==3L)
climate<-split_top(unwrap(main[[1L]]),"OR")
agri<-split_top(unwrap(main[[3L]]),"OR")
stopifnot(length(climate)==5L,length(agri)==7L)
review<-unwrap(main[[2L]])
key<-Sys.getenv("WOS_STARTER_API")
if(!nzchar(key))stop("WOS_STARTER_API missing")
dir.create(sprintf("outputs/wos/shard_%d",shard),recursive=TRUE,showWarnings=FALSE)
out<-sprintf("outputs/wos/shard_%d/counts.json",shard)
queue<-lapply(seq_along(agri),function(j)list(
  climate=climate[[shard]],review=review,agri=agri[[j]],origin=paste0(shard,":",j),depth=0L))
done<-list();splits<-list();errors<-list();requests<-0L
build_query<-function(task)paste0("(",task$climate,") AND (",task$review,") AND (",task$agri,") AND PY=(2015-2026)")
save_state<-function()write_json(list(shard=shard,complete=length(queue)==0L&&length(errors)==0L,
  completed_count_queries=length(done),pending=length(queue),requests=requests,summaries=done,splits=splits,errors=errors),
  out,pretty=TRUE,auto_unbox=TRUE,null="null")
get_count<-function(q){
  for(attempt in 1:4){
    req<-request("https://api.clarivate.com/apis/wos-starter/v1/documents") |>
      req_headers("X-ApiKey"=key,Accept="application/json") |>
      req_url_query(db="WOS",q=q,limit=1,page=1) |>
      req_timeout(120) |> req_error(is_error=function(resp)FALSE)
    res<-tryCatch(req_perform(req),error=identity)
    if(!inherits(res,"error")){
      status<-resp_status(res)
      body<-tryCatch(resp_body_json(res,simplifyVector=FALSE),error=function(e)list())
      if(status==200L)return(list(status=status,total=body$metadata$total))
      excerpt<-substr(tryCatch(resp_body_string(res),error=function(e)""),1L,500L)
      if(!(status %in% c(429L,500L,502L,503L,504L)))
        return(list(status=status,excerpt=excerpt))
    } else excerpt<-conditionMessage(res)
    if(attempt==4L)return(list(status="transport_error",excerpt=excerpt))
    Sys.sleep(2^attempt)
  }
}
while(length(queue)>0L){
  if(requests>=350L){errors[[length(errors)+1L]]<-list(reason="350 request safety cap reached");break}
  task<-queue[[1L]];queue<-queue[-1L]
  query<-build_query(task);requests<-requests+1L
  ans<-get_count(query)
  cat(sprintf("Shard %d request %d %s HTTP %s chars %d hits %s\n",
    shard,requests,task$origin,ans$status,nchar(query),if(is.null(ans$total))"NA" else as.character(ans$total)))
  if(identical(ans$status,200L)&&!is.null(ans$total)){
    done[[length(done)+1L]]<-list(origin=task$origin,query=query,
      query_sha256=digest(query,algo="sha256",serialize=FALSE),
      query_characters=nchar(query),total_results=as.integer(ans$total))
  }else if(identical(ans$status,414L)||identical(ans$status,413L)||(
    identical(ans$status,400L)&&grepl("too long|too large|length",ans$excerpt,ignore.case=TRUE))){
    concepts<-list(task$climate,task$review,task$agri)
    leaves<-lapply(concepts,explode_or)
    eligible<-which(lengths(leaves)>1L)
    if(!length(eligible)){
      errors[[length(errors)+1L]]<-list(origin=task$origin,reason="No smaller OR query possible",http=ans$status)
      save_state();break
    }
    k<-eligible[[which.max(vapply(leaves[eligible],function(z)sum(nchar(z)),numeric(1)))]]
    mid<-floor(length(leaves[[k]])/2L)
    children<-lapply(1:2,function(j){
      z<-task
      group<-if(j==1L) leaves[[k]][seq_len(mid)] else leaves[[k]][(mid+1L):length(leaves[[k]])]
      z[[k]]<-join_or(group);z$origin<-paste0(task$origin,".",j);z$depth<-task$depth+1L
      z
    })
    queue<-c(children,queue)
    splits[[length(splits)+1L]]<-list(origin=task$origin,concept=k,children=vapply(children,function(z)z$origin,character(1)))
  }else{
    errors[[length(errors)+1L]]<-list(origin=task$origin,http=ans$status,excerpt=ans$excerpt,query_characters=nchar(query))
    save_state();break
  }
  save_state()
  Sys.sleep(0.1)
}
save_state()
cat(sprintf("Shard %d finished: complete=%s, requests=%d, successful=%d, pending=%d, errors=%d, summed_hits=%d\n",
 shard,length(queue)==0L&&length(errors)==0L,requests,length(done),length(queue),length(errors),
 sum(vapply(done,function(z)as.integer(z$total_results),integer(1)))))
if(length(queue)||length(errors))stop("WoS count shard incomplete; see counts.json")
