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
        n_declarations <- if(declarations[[1L]] == -1L) 0L else length(declarations)
        message(sprintf("EBSCO malformed-XML diagnostics: bytes=%d, xml_declarations=%d, hit_elements=%d, begins_with_xml=%s, xml_error=%s",
          length(response_bytes),n_declarations,length(hits),
          grepl("^[[:space:]]*<\\?xml",body),conditionMessage(x)))
        # EIT sometimes returns two XML documents in one HTTP 200 response.
        # For count-only calls, accept a unique numeric Hits element only,
        # with an XML prefix, no explicit API error and no ambiguous count.
        if(endpoint == "Search" && n_declarations == 2L &&
           grepl("^[[:space:]]*<\\?xml",body) &&
           length(hits) == 1L &&
           !grepl("<(?:[[:alnum:]_]+:)?(?:Error|Fault)(?:[[:space:]>])",body,perl=TRUE)) {
          count <- sub(".*<Hits>[[:space:]]*([0-9]+)[[:space:]]*</Hits>.*","\\1",hits,perl=TRUE)
          if(grepl("^[0-9]+$",count)) {
            message("EBSCO count-only: recovered a single unambiguous Hits value from a malformed multi-declaration XML response")
            x <- read_xml(sprintf("<CountResponse><Hits>%s</Hits></CountResponse>",count))
          }
        }
        if(inherits(x,"error") && n_declarations==2L) {
          starts <- as.integer(declarations)
          pieces <- c(substr(body,starts[[1L]],starts[[2L]]-1L),substr(body,starts[[2L]],nchar(body)))
          for(pi in seq_along(pieces)) {
            parsed_piece <- tryCatch(read_xml(pieces[[pi]]),error=identity)
            if(inherits(parsed_piece,"error")) {
              message(sprintf("EBSCO response segment %d: invalid XML (%s)",pi,conditionMessage(parsed_piece)))
            } else {
              root_name <- xml_name(xml_root(parsed_piece))
              err_nodes <- xml_find_all(parsed_piece,"//*[local-name()='Error' or local-name()='error' or local-name()='Fault']")
              err_text <- if(length(err_nodes)) substr(gsub("[[:space:]]+"," ",xml_text(err_nodes[[1L]])),1L,400L) else "none"
              message(sprintf("EBSCO response segment %d: root=%s, api_error=%s",pi,root_name,err_text))
            }
          }
        }
        if(inherits(x,"error")) stop("EBSCO returned invalid XML without a safely recoverable count; see structural diagnostics")
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
# Large forest/fibre blocks exceed EBSCO's EIT query capacity. Split
# their OR terms, retrieve accession numbers only, and count their union.
flatten_or <- function(s) {
  s <- unwrap(s)
  terms <- split_top(s,"OR")
  if(length(terms)>1L) return(unlist(lapply(terms,flatten_or),use.names=FALSE))
  s
}
forestry_terms <- flatten_or(agri[[7L]])
if(length(forestry_terms)<10L) stop("Unexpected forestry OR component structure")
extract_ids <- function(doc) {
  rec <- xml_find_all(doc,"//*[local-name()='rec']")
  if(!length(rec)) return(character())
  vapply(rec,function(n) {
    header <- xml_find_first(n,".//*[local-name()='header']")
    id <- if(!inherits(header,"xml_missing")) xml_attr(header,"uiTerm") else NA_character_
    if(is.na(id)||!nzchar(id)) {
      ui <- xml_find_first(n,".//*[local-name()='ui' and not(@type)]")
      id <- if(!inherits(ui,"xml_missing")) trimws(xml_text(ui)) else NA_character_
    }
    if(is.na(id)||!nzchar(id)) stop("EconLit brief response has no accession identifier")
    id
  },character(1))
}
count_forestry_exact <- function(i) {
  term_chunks <- split(forestry_terms,ceiling(seq_along(forestry_terms)/4L))
  unique_ids <- new.env(hash=TRUE,parent=emptyenv())
  raw <- 0L
  for(chunk_index in seq_along(term_chunks)) {
    chunk <- paste(term_chunks[[chunk_index]],collapse=" OR ")
    q <- paste0("(",field_block(climate[[i]])," AND ",field_block(review),
                " AND ",field_block(chunk),") AND (DT 20150101-)")
    d <- fetch("Search",list(prof=uid,pwd=pwd,authType="profile",
        db="ecn",query=q,format="brief",startrec="1",numrec="100"))
    hit_nodes <- xml_find_all(d,"//*[local-name()='Hits']")
    hits <- suppressWarnings(as.integer(trimws(xml_text(hit_nodes))))
    hits <- hits[!is.na(hits)]
    if(length(hits)!=1L) stop("Chunk returned an ambiguous hit count")
    count <- hits[[1L]]
    if(count>4880L) stop("A forestry chunk exceeds EIT safe pagination limit")
    got <- extract_ids(d)
    if(length(got)>count) stop("More IDs than count from EBSCO")
    if(count>0L) {
      while(length(got)<count) {
        d <- fetch("Search",list(prof=uid,pwd=pwd,authType="profile",
          db="ecn",query=q,format="brief",
          startrec=as.character(length(got)+1L),numrec="100"))
        next_ids <- extract_ids(d)
        if(!length(next_ids)) stop("Missing EconLit records during chunk pagination")
        got <- c(got,next_ids)
      }
    }
    if(length(got)!=count||anyDuplicated(got)) stop("Chunk accession count mismatch")
    for(id in got) assign(id,TRUE,envir=unique_ids)
    raw <- raw+count
    cat(sprintf("EconLit forestry %d:7 chunk %d/%d: %d records, %d unique cumulative\\n",
      i,chunk_index,length(term_chunks),count,length(ls(unique_ids,all.names=TRUE))))
    Sys.sleep(.2)
  }
  list(unique=length(ls(unique_ids,all.names=TRUE)),
       raw_chunk_occurrences=raw,chunks=length(term_chunks))
}
result <- list(database="EconLit",provider="EBSCOhost EIT",date_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),status="incomplete",source="user_input/scopus_search_2015_onwards.txt",fields=c("TI","AB"),from_date="2015-01-01",components=list(),error=NULL)
save <- function()write_json(result,file.path(root,"count_report.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
save()
for(k in seq_along(queries)) {
  q <- queries[[k]]
  parts_index <- as.integer(strsplit(q$component,":",fixed=TRUE)[[1L]])
  checked <- tryCatch(if(parts_index[[2L]]==7L) count_forestry_exact(parts_index[[1L]]) else
    list(unique=count_one(q$query),raw_chunk_occurrences=NULL,chunks=1L),error=identity)
  if(inherits(checked,"error")) {
    result$error <- list(component=q$component,message=conditionMessage(checked));save()
    stop(conditionMessage(checked))
  }
  n <- as.integer(checked$unique)
  result$components[[k]] <- list(component=q$component,hits=n,
    method=if(parts_index[[2L]]==7L)"partitioned_accession_id_union" else "direct_api_count",
    query_sha256=digest(q$query,algo="sha256",serialize=FALSE),
    chunk_count=checked$chunks,raw_chunk_occurrences=checked$raw_chunk_occurrences)
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
