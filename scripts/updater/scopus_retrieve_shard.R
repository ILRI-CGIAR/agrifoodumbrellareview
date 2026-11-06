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
  if (grepl("^TITLE-ABS-KEY[[:space:]]*\\(",s)) {
    inside <- sub("^TITLE-ABS-KEY[[:space:]]*\\(","",s)
    inside <- substr(inside,1L,nchar(inside)-1L)
    inner <- explode_or(inside)
    return(vapply(inner,function(x) paste0("TITLE-ABS-KEY ( ",x," )"),character(1)))
  }
  s
}
join_or <- function(terms) paste0("( ",paste(terms,collapse=" OR ")," )")


split_oversized_query <- function(query) {
  pieces <- split_top(unwrap(query),"AND")
  if(length(pieces)<3L) stop("Cannot safely subdivide Scopus search")
  leaves <- lapply(pieces,explode_or)
  eligible <- which(lengths(leaves)>1L)
  if(!length(eligible))stop("No splittable OR terms remain")
  k <- eligible[[which.max(vapply(leaves[eligible],function(x)sum(nchar(x)),numeric(1)))]]
  mid <- floor(length(leaves[[k]])/2L)
  if(mid<1L)stop("Invalid OR split")
  split_groups <- list(leaves[[k]][seq_len(mid)],leaves[[k]][(mid+1L):length(leaves[[k]])])
  lapply(split_groups,function(group){
    z <- pieces
    z[[k]] <- paste0("( ",paste(group,collapse=" OR ")," )")
    paste(z,collapse=" AND ")
  })
}
get_partition_records <- function(query,origin,depth=0L) {
  if(depth>14L)stop(sprintf("Exceeded query split depth for %s",origin))
  try_result <- tryCatch({
    head <- request_page(query,0L,1L)
    total <- as.integer(head[["search-results"]][["opensearch:totalResults"]])
    if(is.na(total)||total<0L||total>=5000L)stop("Scopus subquery outside safe API limit")
    gathered <- list()
    if(total>0L)for(start in seq.int(0L,total-1L,by=200L)) {
      count <- min(200L,total-start)
      page <- if(start==0L && total==1L)head else request_page(query,start,count)
      got <- as.integer(page[["search-results"]][["opensearch:totalResults"]])
      if(is.na(got)||got!=total)stop(sprintf("Scopus count changed during retrieval for %s",origin))
      entries <- page[["search-results"]][["entry"]]
      if(is.null(entries)||length(entries)!=count)stop(sprintf("Page incomplete for %s at %d",origin,start))
      gathered <- c(gathered,lapply(entries,sanitize,origin=origin))
      Sys.sleep(0.12)
    }
    ids <- if(length(gathered))vapply(gathered,function(z)z$eid,character(1)) else character()
    if(length(ids)!=total||anyDuplicated(ids))stop(sprintf("Nonunique or incomplete EIDs in %s",origin))
    gathered
  },error=identity)
  if(!inherits(try_result,"error"))return(try_result)
  if(!grepl("Request header is too large",conditionMessage(try_result),fixed=TRUE))
    stop(try_result)
  pieces <- split_oversized_query(query)
  message(sprintf("Split oversized Scopus retrieval query %s into two OR-equivalent children",origin))
  unlist(lapply(seq_along(pieces),function(j)
    get_partition_records(pieces[[j]],paste0(origin,".",j),depth+1L)),recursive=FALSE)
}

summary <- vector("list",length(partitions))
for(k in seq_along(partitions)){
  n <- partitions[[k]]; p<-snap$summaries[[n]]
  total<-as.integer(p$total_results);origin<-as.character(p$origin)
  if(is.na(total)||total<0L||total>=5000L)stop(sprintf("Unsafe partition count for %s: %s",origin,total))
  path<-file.path(root,sprintf("partition_%03d.jsonl",n))
  if(file.exists(path)){
    lines<-readLines(path,warn=FALSE,encoding="UTF-8")
    records<-lapply(lines,fromJSON,simplifyVector=FALSE)
    ids<-if(length(records))vapply(records,function(z)as.character(z$eid),character(1))else character()
    if(length(ids)!=total||anyDuplicated(ids))stop(sprintf("Restored partition %s incomplete",origin))
    cat(sprintf("RESUMED partition %d (%s): %d records\n",n,origin,length(ids)))
  }else{
    raw <- get_partition_records(p$query,origin)
    raw_ids<-if(length(raw))vapply(raw,function(z)z$eid,character(1))else character()
    records<-raw[!duplicated(raw_ids)]
    ids<-if(length(records))vapply(records,function(z)z$eid,character(1))else character()
    if(length(ids)!=total)
      stop(sprintf("Partition %s union mismatch: %d unique EIDs vs %d original hits",origin,length(ids),total))
    con<-file(path,"wt",encoding="UTF-8")
    for(rec in records) {
      rec$source_partition<-origin
      writeLines(toJSON(rec,auto_unbox=TRUE,null="null",na="null"),con)
    }
    close(con)
    cat(sprintf("PASS partition %d (%s): %d / %d records\n",n,origin,length(ids),total))
  }
  summary[[k]]<-list(index=n,origin=origin,expected=total,retrieved=length(ids),unique_eids=length(unique(ids)),sha256=digest(file=path,algo="sha256"))
  write_json(summary[seq_len(k)],file.path(root,"progress.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
  cat(sprintf("PASS partition %d (%s): %d / %d records\n",n,origin,length(ids),total))
}
write_json(list(shard=shard,partitions=summary,expected=sum(vapply(summary,function(x)x$expected,integer(1)))),
           file.path(root,"manifest.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS shard %d: %d partitions; %d bibliographic hits\n",shard,length(summary),sum(vapply(summary,function(x)x$retrieved,integer(1)))))
