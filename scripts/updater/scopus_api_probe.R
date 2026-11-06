#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
or_else <- function(x,y) if (is.null(x)) y else x

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
input <- paste(readLines("user_input/scopus_search_2015_onwards.txt",warn=FALSE,encoding="UTF-8"),collapse=" ")
input <- trimws(gsub("[[:space:]]+"," ",input))
input <- gsub("\\\\[*]","*",input) # Reverse Markdown escaping of wildcards
input <- sub(" AND PUBYEAR > 2014$","",input)
main <- split_top(unwrap(input),"AND")
stopifnot(length(main)==3L)
cparts <- split_top(unwrap(main[[1L]]),"OR")
apart <- split_top(unwrap(main[[3L]]),"OR")
stopifnot(length(cparts)==5L,length(apart)==7L)
review <- unwrap(main[[2L]])
queue <- unlist(lapply(seq_along(cparts),function(i)
  lapply(seq_along(apart),function(j) list(
    climate=cparts[[i]],review=review,agrifood=apart[[j]],
    origin=paste0(i,":",j),split_depth=0L))),recursive=FALSE)
stopifnot(length(queue)==35L)
key <- Sys.getenv("SCOPUS_API_TOKEN")
if(!nzchar(key)) stop("SCOPUS_API_TOKEN missing")
inst <- Sys.getenv("SCOPUS_INSTTOKEN")
outdir <- "outputs/scopus";dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
done <- list(); errors <- list(); splits <- list(); attempts <- 0L
save_state <- function() write_json(list(
  starting_subqueries=35L,successful_count_queries=length(done),
  pending=length(queue),partition_events=length(splits),
  summaries=done,errors=errors,splits=splits),
  file.path(outdir,"search_probe.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
while(length(queue)>0L) {
  if(attempts>=300L) {errors[[length(errors)+1L]]<-list(reason="Safety limit of 300 requests reached");break}
  task <- queue[[1L]];queue <- queue[-1L];attempts <- attempts+1L
  query <- paste0("(",task$climate,") AND (",task$review,") AND (",task$agrifood,") AND PUBYEAR > 2014")
  req <- request("https://api.elsevier.com/content/search/scopus") |>
    req_headers("X-ELS-APIKey"=key,Accept="application/json")
  if(nzchar(inst))req <- req |> req_headers("X-ELS-Insttoken"=inst)
  req <- req |> req_url_query(query=query,count=1,view="STANDARD") |>
    req_timeout(90) |> req_error(is_error=function(resp)FALSE)
  resp <- tryCatch(req_perform(req),error=identity)
  if(inherits(resp,"error")) {
    errors[[length(errors)+1L]] <- list(origin=task$origin,reason=conditionMessage(resp),query_length=nchar(query))
    save_state();break
  }
  status <- resp_status(resp)
  body <- tryCatch(resp_body_json(resp,simplifyVector=FALSE),error=function(e)list())
  total <- body[["search-results"]][["opensearch:totalResults"]]
  cat(sprintf("Request %d | parent %s | HTTP %d | %d chars | matches %s\n",
    attempts,task$origin,status,nchar(query),or_else(total,"n/a")))
  if(status==200L && !is.null(total)) {
    done[[length(done)+1L]] <- list(origin=task$origin,split_depth=task$split_depth,
      query_sha256=digest(query,algo="sha256",serialize=FALSE),
      query_characters=nchar(query),total_results=as.integer(total))
  } else {
    details <- tryCatch(resp_body_string(resp),error=function(e)"")
    if (status %in% c(400L,414L) &&
        grepl("too large|URI Too Long|Request-URI Too Long",details,ignore.case=TRUE)) {
      concepts <- list(task$climate,task$review,task$agrifood)
      leaves <- lapply(concepts,explode_or)
      eligible <- which(lengths(leaves)>1L)
      if(!length(eligible)) {
        errors[[length(errors)+1L]] <- list(origin=task$origin,status=status,
          reason="Oversize response but no OR partition remains",excerpt=substr(details,1,500))
        save_state();break
      }
      k <- eligible[[which.max(vapply(leaves[eligible],
          function(x)sum(nchar(x)),numeric(1))]]
      half <- floor(length(leaves[[k]])/2L)
      splitparts <- list(leaves[[k]][seq_len(half)],leaves[[k]][(half+1L):length(leaves[[k]])])
      newtasks <- lapply(seq_along(splitparts), function(j) {
        z<-task;z[[k]]<-join_or(splitparts[[j]])
        z$split_depth <- task$split_depth+1L
        z$origin <- paste0(task$origin,".",j)
        z
      })
      queue <- c(newtasks,queue)
      splits[[length(splits)+1L]]<-list(parent=task$origin,concept=k,
          child_lengths=vapply(splitparts,function(x)nchar(join_or(x)),integer(1)))
      cat(sprintf("Split oversized query %s into two OR-equivalent components\n",task$origin))
    } else {
      errors[[length(errors)+1L]] <- list(origin=task$origin,status=status,
        query_length=nchar(query),excerpt=substr(details,1,1200))
      save_state();break
    }
  }
  save_state()
  Sys.sleep(0.2)
}
cat(sprintf("Counts completed: %d; partitions: %d; errors: %d; pending: %d\n",
  length(done),length(splits),length(errors),length(queue)))
if(length(queue)>0L || length(errors)>0L) stop("Scopus count search incomplete; inspect diagnostic")
cat("PASS: all partitioned Scopus count requests completed\n")
