#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(xml2);library(jsonlite);library(digest)})
split_top <- function(s, op) {
  s <- trimws(s); chars <- strsplit(s, "", fixed=TRUE)[[1L]]
  needle <- paste0(" ", op, " "); w <- nchar(needle)
  dep <- 0L; quoted <- FALSE; i <- 1L; start <- 1L; out <- character()
  while(i <= length(chars)) {
    ch <- chars[[i]]
    if(ch == '"') quoted <- !quoted
    if(!quoted) {
      if(ch == "(") dep <- dep + 1L
      if(ch == ")") dep <- dep - 1L
      if(dep == 0L && i+w-1L <= length(chars) && paste(chars[i:(i+w-1L)],collapse="") == needle) {
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
    if(!(startsWith(s,"(") && endsWith(s,")"))) return(s)
    chars <- strsplit(s,"",fixed=TRUE)[[1L]]; dep <- 0L; quoted <- FALSE; early <- FALSE
    for(i in seq_along(chars)) {
      ch <- chars[[i]]
      if(ch == '"') quoted <- !quoted
      if(!quoted) {
        if(ch == "(") dep <- dep+1L
        if(ch == ")") dep <- dep-1L
        if(dep == 0L && i < length(chars)) {early <- TRUE; break}
      }
    }
    if(early) return(s)
    s <- trimws(substr(s,2L,nchar(s)-1L))
  }
}
scopus <- paste(readLines("user_input/scopus_search_2015_onwards.txt",warn=FALSE,encoding="UTF-8"),collapse=" ")
scopus <- trimws(gsub("[[:space:]]+"," ",scopus))
if(!grepl(" AND PUBYEAR > 2014$",scopus)) stop("Unexpected Scopus date expression")
scopus <- sub(" AND PUBYEAR > 2014$","",scopus)
scopus <- gsub("\\\\[*]","*",scopus)
scopus <- gsub("TITLE-ABS-KEY[[:space:]]*\\(","(",scopus)
near <- regmatches(scopus,gregexpr("W/[0-9]+",scopus))[[1L]]
if(length(near)!=4L) stop("Expected exactly four Scopus proximity operators")
scopus <- gsub("[[:space:]]+W/([0-9]+)[[:space:]]+"," N\\1 ",scopus,perl=TRUE)
parts <- split_top(unwrap(scopus),"AND")
if(length(parts)!=3L) stop("Expected three main concept blocks")
climate <- split_top(unwrap(parts[[1L]]),"OR")
review <- unwrap(parts[[2L]])
agri <- split_top(unwrap(parts[[3L]]),"OR")
if(length(climate)!=5L || length(agri)!=7L) stop("Expected 5 x 7 components")
field_block <- function(s) {
  s <- unwrap(s)
  sprintf("(TI (%s) OR AB (%s))",s,s)
}
query_for <- function(i,j) {
  paste0("(",field_block(climate[[i]])," AND ",field_block(review)," AND ",field_block(agri[[j]]),") AND (DT 20150101-)")
}
root <- "outputs/econlit/count";dir.create(root,recursive=TRUE,showWarnings=FALSE)
queries <- lapply(seq_len(5L),function(i)lapply(seq_len(7L),function(j)list(
  component=sprintf("%d:%d",i,j),query=query_for(i,j))))
queries <- unlist(queries,recursive=FALSE)
write_json(list(database="EconLit",database_code="ecn",fields=c("TI","AB"),date_filter="DT 20150101-",date_scope="2015 onwards",translation="Scopus W/n -> EBSCO Nn",components=queries),
  file.path(root,"translated_queries.json"),pretty=TRUE,auto_unbox=TRUE)
uid <- Sys.getenv("EBSCO_EHOST_UID");pwd <- Sys.getenv("EBSCO_EHOST_PWD")
if(!nzchar(uid)||!nzchar(pwd))stop("EBSCO_EHOST_UID / EBSCO_EHOST_PWD unavailable")
fetch <- function(endpoint, params) {
  last_error <- ""
  for(attempt in seq_len(5L)) {
    outcome <- tryCatch({
      r <- request(paste0("https://eit.ebscohost.com/Services/SearchService.asmx/",endpoint)) |>
        req_url_query(!!!params) |> req_timeout(150) |>
        req_error(is_error=function(x)FALSE) |> req_perform()
      status <- resp_status(r)
      if(status != 200L) stop(sprintf("HTTP %d",status))
      response_bytes <- resp_body_raw(r)
      x <- tryCatch(read_xml(response_bytes),error=identity)
      if(inherits(x,"error")) {
        body <- rawToChar(response_bytes)
        declarations <- gregexpr("<?xml",body,fixed=TRUE)[[1L]]
        hits <- regmatches(body,gregexpr("<Hits>[[:space:]]*[0-9]+[[:space:]]*</Hits>",body,perl=TRUE))[[1L]]
        message(sprintf("EBSCO malformed-XML diagnostics: bytes=%d, xml_declarations=%d, hit_elements=%d, begins_with_xml=%s, xml_error=%s",
          length(response_bytes),if(declarations[[1L]]==-1L)0L else length(declarations),
          if(length(hits)==1L&&identical(hits,""))0L else length(hits),
          grepl("^[[:space:]]*<\\?xml",body),conditionMessage(x)))
        stop("EBSCO returned invalid XML; see structural diagnostics")
      }
      err <- xml_find_first(x,"//*[local-name()='Error' or local-name()='error']")
      if(!inherits(err,"xml_missing")) stop(sprintf("API error: %s",xml_text(err)))
      x
    },error=identity)
    if(!inherits(outcome,"error")) return(outcome)
    last_error <- conditionMessage(outcome)
    message(sprintf("EBSCO %s attempt %d/5 failed: %s",endpoint,attempt,last_error))
    if(attempt < 5L) Sys.sleep(min(30,2^attempt))
  }
  stop(sprintf("EBSCO %s failed after 5 attempts: %s",endpoint,last_error),call.=FALSE)
}
info <- fetch("Info",list(prof=uid,pwd=pwd,authType="profile"))
dbs <- xml_find_all(info,"//*[local-name()='db']")
db <- dbs[which(xml_attr(dbs,"shortName")=="ecn")]
if(length(db)!=1L)stop("EBSCO credential does not expose EconLit (ecn)")
fields <- unique(xml_attr(xml_find_all(db[[1L]],".//*[local-name()='dbTag']"),"name"))
if(length(setdiff(c("TI","AB"),fields))) stop("EconLit profile does not advertise TI and AB")
count_one <- function(q) {
  xml <- fetch("Search",list(prof=uid,pwd=pwd,authType="profile",db="ecn",query=q,format="brief",startrec="1",numrec="1"))
  hits <- xml_find_all(xml,"//*[local-name()='Hits']")
  v <- suppressWarnings(as.integer(trimws(xml_text(hits))))
  v <- v[!is.na(v)]
  if(!length(v))stop("Search response contains no Hits")
  v[[1L]]
}
result <- list(database="EconLit",provider="EBSCOhost EIT",date_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),status="incomplete",source="user_input/scopus_search_2015_onwards.txt",fields=c("TI","AB"),from_date="2015-01-01",components=list(),error=NULL)
save <- function()write_json(result,file.path(root,"count_report.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
save()
for(k in seq_along(queries)) {
  q <- queries[[k]]
  n <- tryCatch(count_one(q$query),error=identity)
  if(inherits(n,"error")) {result$error <- list(component=q$component,message=conditionMessage(n));save();stop(conditionMessage(n))}
  result$components[[k]] <- list(component=q$component,hits=n,query_sha256=digest(q$query,algo="sha256",serialize=FALSE))
  save()
  cat(sprintf("EconLit component %s: %d hits\n",q$component,n))
  Sys.sleep(.3)
}
result$status <- "complete"
result$summed_component_hits <- sum(vapply(result$components,function(x)x$hits,integer(1)))
result$unique_results <- NULL
result$note <- "Component totals overlap and must not be interpreted as unique search results. No records harvested."
save()
cat(sprintf("PASS EconLit 35 components, summed hits %d (NOT deduplicated)\n",result$summed_component_hits))
