#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
  library(stringdist)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_path <- arg("--input")
output_path <- arg("--output")
audit_path <- arg("--audit")
report_path <- arg("--report")
limit_arg <- arg("--limit",NULL)
record_ids_path <- arg("--record-ids",NULL)
progress_every <- as.integer(arg("--progress-every","100"))
delay <- as.numeric(arg("--delay","0.5"))
max_rate_limit_wait <- as.numeric(arg("--max-rate-limit-wait","60"))
scopus_rate_limit_trip <- as.integer(arg("--scopus-rate-limit-trip","3"))
skip_scopus <- tolower(arg("--skip-scopus","false")) %in% c("true","1","yes")
recheck_after_days <- as.numeric(arg("--recheck-after-days","90"))
enrich_author_keywords <- tolower(arg("--enrich-author-keywords","false")) %in% c("true","1","yes")
if(is.na(delay) || delay < 0) stop("--delay must be >= 0",call.=FALSE)
if(is.na(max_rate_limit_wait) || max_rate_limit_wait < 0) stop("--max-rate-limit-wait must be >= 0",call.=FALSE)
if(is.na(scopus_rate_limit_trip) || scopus_rate_limit_trip < 1L) stop("--scopus-rate-limit-trip must be >= 1",call.=FALSE)
if(is.na(recheck_after_days) || recheck_after_days < 0) stop("--recheck-after-days must be >= 0",call.=FALSE)
if(any(vapply(list(input_path,output_path,audit_path,report_path),is.null,logical(1)))) {
  stop("Required: --input --output --audit --report",call.=FALSE)
}
limit <- if(is.null(limit_arg)) Inf else as.integer(limit_arg)
if(!is.infinite(limit) && (is.na(limit)||limit<1L)) stop("--limit must be positive",call.=FALSE)
if(is.na(progress_every) || progress_every < 1L) stop("--progress-every must be >= 1",call.=FALSE)
selected_record_ids <- NULL
if(!is.null(record_ids_path)){
  if(!file.exists(record_ids_path)) stop("--record-ids file not found",call.=FALSE)
  selected_record_ids <- unique(trimws(readLines(record_ids_path,warn=FALSE,encoding="UTF-8")))
  selected_record_ids <- selected_record_ids[nzchar(selected_record_ids)]
  if(!length(selected_record_ids)) stop("--record-ids contains no record IDs",call.=FALSE)
}

scopus_key <- Sys.getenv("SCOPUS_API_TOKEN")
scopus_insttoken <- Sys.getenv("SCOPUS_INSTTOKEN")
if(!skip_scopus && !nzchar(scopus_key)) stop("SCOPUS_API_TOKEN is required unless --skip-scopus true",call.=FALSE)
if(!skip_scopus && !nzchar(scopus_insttoken)) stop("SCOPUS_INSTTOKEN is required unless --skip-scopus true",call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
now_utc <- function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")

clean_text <- function(x){
  if(is.null(x)||!length(x)) return(NULL)
  s <- trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])))
  if(is.na(s)||!nzchar(s)) NULL else s
}
norm_doi <- function(x){
  s <- clean_text(x)
  if(is.null(s)) return(NULL)
  s <- tolower(s)
  s <- sub("^https?://(dx\\.)?doi\\.org/","",s,perl=TRUE)
  s <- sub("^doi:\\s*","",s,perl=TRUE)
  s <- sub("[[:space:]]+$","",s)
  s <- sub("[[:space:][:punct:]]+$","",s)
  if(!nzchar(s)) NULL else s
}
norm_title <- function(x){
  s <- clean_text(x)
  if(is.null(s)) return(NULL)
  s <- iconv(s,from="",to="ASCII//TRANSLIT",sub="")
  if(is.na(s)) return(NULL)
  s <- tolower(s)
  s <- trimws(gsub("[^a-z0-9]+"," ",s))
  if(!nzchar(s)) NULL else s
}
title_similarity <- function(a,b){
  aa <- norm_title(a); bb <- norm_title(b)
  if(is.null(aa)||is.null(bb)) return(NA_real_)
  1 - stringdist(aa,bb,method="jw",p=0.1)
}
clean_abstract <- function(x){
  s <- clean_text(x)
  if(is.null(s)) return(NULL)
  s <- gsub("<!\\[CDATA\\[(.*?)\\]\\]>","\\1",s,perl=TRUE)
  s <- gsub("<[^>]+>"," ",s,perl=TRUE)
  s <- gsub("&amp;","&",s,fixed=TRUE)
  s <- gsub("&lt;","<",s,fixed=TRUE)
  s <- gsub("&gt;",">",s,fixed=TRUE)
  s <- gsub("&quot;","\"",s,fixed=TRUE)
  s <- gsub("&#39;","'",s,fixed=TRUE)
  s <- trimws(gsub("[[:space:]]+"," ",s))
  if(!nzchar(s)) NULL else substr(s,1L,12000L)
}
is_missing <- function(x) is.null(clean_text(x))

clean_keyword_values <- function(x){
  vals <- character()
  walk <- function(z,nm=NULL){
    if(is.null(z)) return(invisible(NULL))
    if(is.atomic(z) && !is.list(z)){
      zz <- as.character(z)
      zz <- zz[!is.na(zz)]
      if(length(zz)) vals <<- c(vals,zz)
      return(invisible(NULL))
    }
    if(!is.list(z)) return(invisible(NULL))
    nms <- names(z)
    if(!is.null(nms)){
      preferred <- intersect(c("$","#text","text","value","keyword"),nms)
      if(length(preferred)){
        for(k in preferred) walk(z[[k]],k)
        return(invisible(NULL))
      }
      for(k in seq_along(z)){
        key <- nms[[k]] %||% ""
        if(grepl("^@",key) || key %in% c("code","id")) next
        walk(z[[k]],key)
      }
    } else {
      for(v in z) walk(v)
    }
    invisible(NULL)
  }
  walk(x)
  vals <- trimws(gsub("[[:space:]]+"," ",vals))
  vals <- vals[nzchar(vals)]
  if(!length(vals)) return(character())
  vals[!duplicated(tolower(vals))]
}
keywords_missing <- function(x) length(clean_keyword_values(x))==0L

retained_scopus_eid <- function(r){
  mans <- r$manifestations %||% list()
  vals <- character()
  for(m in mans){
    if(!identical(tolower(clean_text(m$source) %||% ""), "scopus")) next
    mm <- m$manifestation_metadata %||% list()
    ids <- mm$identifiers %||% list()
    sid <- mm$source_identity %||% list()
    cand <- c(
      clean_text(sid$scopus_eid),
      clean_text(ids$scopus_eid)
    )
    cand <- cand[!vapply(cand,is.null,logical(1))]
    if(length(cand)) vals <- c(vals,as.character(cand))
  }
  vals <- unique(trimws(vals[nzchar(trimws(vals))]))
  if(!length(vals)) NULL else vals[[1L]]
}

retryable <- function(status) identical(status,429L) || status>=500L

rate_limit_wait_seconds <- function(resp,fallback){
  retry_after <- clean_text(resp_header(resp,"retry-after"))
  if(!is.null(retry_after)){
    sec <- suppressWarnings(as.numeric(retry_after))
    if(!is.na(sec) && sec>=0) return(sec)
    dt <- suppressWarnings(as.POSIXct(retry_after,tz="GMT",format="%a, %d %b %Y %H:%M:%S %Z"))
    if(!is.na(dt)) return(max(0,as.numeric(difftime(dt,Sys.time(),units="secs"))))
  }
  reset <- clean_text(resp_header(resp,"x-ratelimit-reset"))
  if(!is.null(reset)){
    epoch <- suppressWarnings(as.numeric(reset))
    if(!is.na(epoch)) return(max(0,epoch-as.numeric(Sys.time())))
  }
  fallback
}

perform_retry <- function(req,max_attempts=4L){
  errors <- character()
  for(attempt in seq_len(max_attempts)){
    resp <- tryCatch(req_perform(req),error=identity)
    if(!inherits(resp,"error")){
      st <- resp_status(resp)
      if(st<400L || !retryable(st)) return(list(resp=resp,attempts=attempt,errors=errors))
      errors <- c(errors,sprintf("HTTP %d",st))
      if(attempt<max_attempts){
        fallback <- c(1,2,4)[min(attempt,3L)]
        wait <- if(identical(st,429L)) rate_limit_wait_seconds(resp,fallback) else fallback
        if(identical(st,429L) && wait>max_rate_limit_wait){
          stop(sprintf(
            "RATE_LIMIT_EXHAUSTED: HTTP 429 requested %.1f s wait, exceeding configured cap %.1f s",
            wait,max_rate_limit_wait
          ),call.=FALSE)
        }
        Sys.sleep(max(fallback,wait))
      }
    } else {
      errors <- c(errors,conditionMessage(resp))
      if(attempt<max_attempts) Sys.sleep(c(1,2,4)[min(attempt,3L)])
    }
  }
  stop(sprintf("Provider failed after %d attempts: %s",max_attempts,paste(errors,collapse=" | ")),call.=FALSE)
}

epmc_lookup <- function(d){
  req <- request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
    req_url_query(query=sprintf('DOI:"%s"',d),format="json",resultType="core",pageSize=5) |>
    req_headers(Accept="application/json",`User-Agent`="LivingEvidenceMap-Workflow02/1.0")
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L) return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,outcome=paste0("http_",st),attempts=z$attempts))
  dat <- resp_body_json(z$resp,simplifyVector=FALSE)
  hits <- dat$resultList$result %||% list()
  exact <- Filter(function(h) identical(norm_doi(h$doi),d),hits)
  if(!length(exact)) return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,outcome="no_exact_doi_match",attempts=z$attempts))
  h <- exact[[1L]]
  kws <- clean_keyword_values((h$keywordList %||% list())$keyword %||% NULL)
  list(
    status=st,
    title=clean_text(h$title),
    abstract=clean_abstract(h$abstractText),
    author_keywords=kws,
    returned_doi=norm_doi(h$doi),
    outcome=if(!is.null(clean_abstract(h$abstractText))||!is.null(clean_text(h$title))||length(kws))"exact_doi_metadata_returned" else "exact_doi_no_metadata",
    attempts=z$attempts
  )
}

scopus_extract <- function(obj){
  rr <- obj[["abstracts-retrieval-response"]] %||% obj
  core <- rr[["coredata"]] %||% list()
  title <- clean_text(core[["dc:title"]] %||% core[["title"]])
  doi <- norm_doi(core[["prism:doi"]] %||% core[["doi"]])
  eid <- clean_text(core[["eid"]] %||% rr[["eid"]])
  abstract <- clean_abstract(core[["dc:description"]] %||% core[["description"]])
  if(is.null(abstract)){
    item <- rr[["item"]]
    if(!is.null(item)){
      biblio <- (item[["bibrecord"]] %||% list())[["head"]]
      biblio <- (biblio %||% list())[["abstracts"]]
      if(is.null(biblio)) biblio <- ((item[["bibliography"]] %||% list())[["abstracts"]])
      if(!is.null(biblio)) abstract <- clean_abstract(paste(unlist(biblio,use.names=FALSE),collapse=" "))
    }
  }
  head <- (((rr[["item"]] %||% list())[["bibrecord"]] %||% list())[["head"]] %||% list())
  citation_info <- head[["citation-info"]] %||% list()
  keyword_candidates <- list(
    (rr[["authkeywords"]] %||% list())[["author-keyword"]],
    (rr[["author-keywords"]] %||% list())[["author-keyword"]],
    (head[["author-keywords"]] %||% list())[["author-keyword"]],
    (citation_info[["author-keywords"]] %||% list())[["author-keyword"]]
  )
  author_keywords <- character()
  for(k in keyword_candidates){
    z <- clean_keyword_values(k)
    if(length(z)) author_keywords <- c(author_keywords,z)
  }
  if(length(author_keywords)) author_keywords <- author_keywords[!duplicated(tolower(author_keywords))]
  list(title=title,abstract=abstract,doi=doi,eid=eid,author_keywords=author_keywords)
}

scopus_headers <- function(req) {
  req |>
    req_headers(
      `X-ELS-APIKey`=scopus_key,
      `X-ELS-Insttoken`=scopus_insttoken,
      Accept="application/json"
    ) |>
    req_user_agent("LivingEvidenceMap-Workflow02/1.0") |>
    req_error(is_error=function(resp) FALSE)
}

extract_scopus_eid <- function(entry){
  vals <- c(
    clean_text(entry[["eid"]]),
    clean_text(entry[["dc:identifier"]]),
    clean_text(entry[["scopus-id"]]),
    clean_text(entry[["scopus_id"]])
  )
  vals <- vals[!vapply(vals,is.null,logical(1))]
  vals <- trimws(as.character(vals))
  vals <- vals[nzchar(vals)]
  if(!length(vals)) return(NULL)

  normalise_id <- function(x){
    y <- trimws(x)
    y <- sub("^SCOPUS_ID:\\s*","",y,ignore.case=TRUE,perl=TRUE)
    y <- sub("^EID:\\s*","",y,ignore.case=TRUE,perl=TRUE)
    if(grepl("^2-s2\\.0-",y,ignore.case=TRUE)) return(y)
    if(grepl("^[0-9]+$",y)) return(paste0("2-s2.0-",y))
    y
  }

  vals <- unique(vapply(vals,normalise_id,character(1)))
  vals <- vals[nzchar(vals)]
  if(!length(vals)) NULL else vals
}

scopus_search_doi <- function(d){
  req <- request("https://api.elsevier.com/content/search/scopus") |>
    req_url_query(query=sprintf("DOI(%s)",d),count=5,view="STANDARD") |>
    scopus_headers()
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L) return(list(status=st,entries=list(),outcome=paste0("http_",st),attempts=z$attempts))
  parsed <- tryCatch(resp_body_json(z$resp,simplifyVector=FALSE),error=function(e)NULL)
  if(is.null(parsed)) return(list(status=st,entries=list(),outcome="invalid_json",attempts=z$attempts))
  sr <- parsed[["search-results"]] %||% list()
  total_raw <- sr[["opensearch:totalResults"]] %||% sr[["totalResults"]] %||% "0"
  total <- suppressWarnings(as.integer(as.character(total_raw[[1L]] %||% total_raw)))
  if(is.na(total)) total <- 0L
  entries <- sr[["entry"]] %||% list()

  # Elsevier may return HTTP 200 with an entry-like error/empty object even when
  # totalResults is zero. Treat totalResults as authoritative for presence.
  if(total <= 0L) {
    return(list(status=st,entries=list(),total_results=0L,outcome="no_hits",attempts=z$attempts))
  }

  # Keep only genuine record-like entries. Error-only objects are not Scopus hits.
  if(is.list(entries) && length(entries)) {
    looks_like_record <- function(e) {
      if(!is.list(e)) return(FALSE)
      has_id <- any(c("eid","dc:identifier","scopus-id","scopus_id") %in% names(e))
      has_biblio <- any(c("dc:title","prism:doi","prism:publicationName") %in% names(e))
      !(!is.null(e[["error"]]) && !has_id && !has_biblio) && (has_id || has_biblio)
    }
    entries <- Filter(looks_like_record, entries)
  }

  list(
    status=st,
    entries=entries,
    total_results=total,
    outcome=if(length(entries))"search_hits" else "total_results_without_usable_entry",
    attempts=z$attempts
  )
}

scopus_lookup_eid <- function(eid){
  endpoint <- paste0("https://api.elsevier.com/content/abstract/eid/",URLencode(eid,reserved=TRUE))
  req <- request(endpoint) |>
    req_url_query(view="META_ABS") |>
    scopus_headers()
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L) return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=eid,outcome=paste0("http_",st),attempts=z$attempts))
  parsed <- tryCatch(resp_body_json(z$resp,simplifyVector=FALSE),error=function(e)NULL)
  if(is.null(parsed)) return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=eid,outcome="invalid_json",attempts=z$attempts))
  ex <- scopus_extract(parsed)
  list(status=st,title=ex$title,abstract=ex$abstract,author_keywords=ex$author_keywords,returned_doi=ex$doi,eid=ex$eid %||% eid,
       outcome=if(!is.null(ex$title)||!is.null(ex$abstract)||length(ex$author_keywords))"metadata_returned" else "success_no_metadata",
       attempts=z$attempts)
}

scopus_lookup_eid_full <- function(eid){
  endpoint <- paste0("https://api.elsevier.com/content/abstract/eid/",URLencode(eid,reserved=TRUE))
  req <- request(endpoint) |>
    req_url_query(view="FULL") |>
    scopus_headers()
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L) return(list(status=st,title=NULL,author_keywords=character(),returned_doi=NULL,eid=eid,outcome=paste0("http_",st),attempts=z$attempts,route="retained_eid_full"))
  parsed <- tryCatch(resp_body_json(z$resp,simplifyVector=FALSE),error=function(e)NULL)
  if(is.null(parsed)) return(list(status=st,title=NULL,author_keywords=character(),returned_doi=NULL,eid=eid,outcome="invalid_json",attempts=z$attempts,route="retained_eid_full"))
  ex <- scopus_extract(parsed)
  list(status=st,title=ex$title,author_keywords=ex$author_keywords,returned_doi=ex$doi,eid=ex$eid %||% eid,
       outcome=if(length(ex$author_keywords))"metadata_returned" else "success_no_keywords",
       attempts=z$attempts,route="retained_eid_full")
}

scopus_lookup_doi_full <- function(d){
  endpoint <- paste0("https://api.elsevier.com/content/abstract/doi/",URLencode(d,reserved=TRUE))
  req <- request(endpoint) |>
    req_url_query(view="FULL") |>
    scopus_headers()
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L) return(list(status=st,title=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,outcome=paste0("http_",st),attempts=z$attempts,route="direct_doi_full"))
  parsed <- tryCatch(resp_body_json(z$resp,simplifyVector=FALSE),error=function(e)NULL)
  if(is.null(parsed)) return(list(status=st,title=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,outcome="invalid_json",attempts=z$attempts,route="direct_doi_full"))
  ex <- scopus_extract(parsed)
  list(status=st,title=ex$title,author_keywords=ex$author_keywords,returned_doi=ex$doi,eid=ex$eid,
       outcome=if(length(ex$author_keywords))"metadata_returned" else "success_no_keywords",
       attempts=z$attempts,route="direct_doi_full")
}

scopus_lookup <- function(d){
  endpoint <- paste0("https://api.elsevier.com/content/abstract/doi/",URLencode(d,reserved=TRUE))
  req <- request(endpoint) |>
    req_url_query(view="META_ABS") |>
    scopus_headers()
  z <- perform_retry(req)
  st <- resp_status(z$resp)
  if(st>=400L){
    return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,
                outcome=paste0("http_",st),attempts=z$attempts,route="direct_doi"))
  }
  parsed <- tryCatch(resp_body_json(z$resp,simplifyVector=FALSE),error=function(e)NULL)
  if(is.null(parsed)){
    return(list(status=st,title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,
                outcome="invalid_json",attempts=z$attempts,route="direct_doi"))
  }
  ex <- scopus_extract(parsed)
  list(
    status=st,title=ex$title,abstract=ex$abstract,author_keywords=ex$author_keywords,
    returned_doi=ex$doi,eid=ex$eid,
    outcome=if(!is.null(ex$title)||!is.null(ex$abstract)||length(ex$author_keywords))"direct_doi_metadata_returned" else "direct_doi_no_metadata",
    attempts=z$attempts,route="direct_doi"
  )
}

read_jsonl <- function(path){
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  lapply(seq_along(lines),function(i)tryCatch(fromJSON(lines[[i]],simplifyVector=FALSE),
    error=function(e)stop(sprintf("Invalid JSON line %d: %s",i,conditionMessage(e)),call.=FALSE)))
}
write_jsonl <- function(xs,path){
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  con <- file(path,"wt",encoding="UTF-8"); on.exit(close(con))
  for(x in xs) writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
}

rows <- read_jsonl(input_path)
input_sha <- digest(file=input_path,algo="sha256",serialize=FALSE)
audit <- list()
counts <- list(
  total_records=length(rows),
  eligible_doi_missing_metadata=0L,
  europepmc_title_filled=0L,
  europepmc_abstract_filled=0L,
  europepmc_author_keywords_filled=0L,
  scopus_attempted=0L,
  scopus_title_filled=0L,
  scopus_abstract_filled=0L,
  scopus_author_keywords_attempted=0L,
  scopus_author_keywords_filled=0L,
  scopus_http_404=0L,
  conflicts_quarantined=0L,
  still_missing_after=0L,
  deferred_recent_attempts=0L,
  technical_error_records=0L,
  scopus_rate_limit_events=0L,
  scopus_rate_limit_circuit_skips=0L
)
processed_eligible <- 0L
consecutive_scopus_rate_limits <- 0L
scopus_rate_limit_circuit_open <- FALSE

previous_attempt_due <- function(meta){
  if(is.null(meta) || !is.list(meta)) return(TRUE)
  if(isTRUE(meta$technical_error)) return(TRUE)
  completed <- clean_text(meta$completed_at)
  if(is.null(completed)) return(TRUE)
  t <- suppressWarnings(as.POSIXct(completed,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
  if(is.na(t)) return(TRUE)
  age_days <- as.numeric(difftime(Sys.time(),t,units="days"))
  is.na(age_days) || age_days >= recheck_after_days
}

for(i in seq_along(rows)){
  r <- rows[[i]]
  if(is.null(r$canonical)) r$canonical <- list()
  record_id <- clean_text((r$identity %||% list())$record_id %||% (r$identity %||% list())$lens_id)
  d <- norm_doi(r$canonical$doi)
  missing_title_before <- is_missing(r$canonical$title)
  missing_abstract_before <- is_missing(r$canonical$abstract)
  missing_author_keywords_before <- keywords_missing(r$canonical$author_keywords)
  eligible <- !is.null(d) && (missing_title_before || missing_abstract_before || (enrich_author_keywords && missing_author_keywords_before))
  if(!eligible) next
  counts$eligible_doi_missing_metadata <- counts$eligible_doi_missing_metadata + 1L
  if(!previous_attempt_due(r$metadata_enrichment)){
    counts$deferred_recent_attempts <- counts$deferred_recent_attempts + 1L
    next
  }
  if(!is.null(selected_record_ids) && (is.null(record_id) || !(record_id %in% selected_record_ids))) next
  if(processed_eligible>=limit) next
  processed_eligible <- processed_eligible + 1L
  if(processed_eligible == 1L || processed_eligible %% progress_every == 0L){
    target <- if(is.null(selected_record_ids)) if(is.infinite(limit)) "all due" else as.character(limit) else as.character(length(selected_record_ids))
    cat(sprintf("PROGRESS: processed %d / %s selected eligible records\n",processed_eligible,target))
    flush.console()
  }

  rec_audit <- list(
    record_id=record_id,
    doi=d,
    missing_title_before=missing_title_before,
    missing_abstract_before=missing_abstract_before,
    missing_author_keywords_before=missing_author_keywords_before,
    retained_scopus_eid=retained_scopus_eid(r),
    europe_pmc=NULL,
    scopus=NULL,
    scopus_keywords=NULL,
    applied=list(),
    quarantined=list()
  )

  ep <- tryCatch(
    epmc_lookup(d),
    error=function(e) list(outcome="technical_error",error=conditionMessage(e),title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,status=NULL,attempts=NULL)
  )
  rec_audit$europe_pmc <- ep

  if(identical(ep$returned_doi,d)){
    # For records with an existing canonical title, all Europe PMC fields require
    # independent title agreement. When title itself is missing, exact DOI is the
    # available identity guard for filling the title; other fields are not applied
    # until a title-consistent identity can be established.
    canonical_title_before <- clean_text(r$canonical$title)
    ep_sim <- if(!is.null(canonical_title_before) && !is.null(ep$title)) title_similarity(canonical_title_before,ep$title) else NA_real_
    ep_title_guard <- !is.null(canonical_title_before) && !is.null(ep$title) && !is.na(ep_sim) && ep_sim>=0.90

    if(is_missing(r$canonical$title) && !is.null(ep$title)){
      r$canonical$title <- ep$title
      counts$europepmc_title_filled <- counts$europepmc_title_filled + 1L
      rec_audit$applied <- c(rec_audit$applied,list(list(provider="europe_pmc",field="title",identity_guard="exact_doi")))
      canonical_title_before <- ep$title
      ep_sim <- 1
      ep_title_guard <- TRUE
    }

    if(is_missing(r$canonical$abstract) && !is.null(ep$abstract)){
      if(ep_title_guard){
        r$canonical$abstract <- ep$abstract
        counts$europepmc_abstract_filled <- counts$europepmc_abstract_filled + 1L
        rec_audit$applied <- c(rec_audit$applied,list(list(provider="europe_pmc",field="abstract",title_similarity=ep_sim)))
      } else {
        counts$conflicts_quarantined <- counts$conflicts_quarantined + 1L
        rec_audit$quarantined <- c(rec_audit$quarantined,list(list(provider="europe_pmc",field="abstract",reason="title_guard_failed",title_similarity=ep_sim)))
      }
    }

    if(enrich_author_keywords && keywords_missing(r$canonical$author_keywords) && length(ep$author_keywords)){
      if(ep_title_guard){
        r$canonical$author_keywords <- ep$author_keywords
        counts$europepmc_author_keywords_filled <- counts$europepmc_author_keywords_filled + 1L
        rec_audit$applied <- c(rec_audit$applied,list(list(provider="europe_pmc",field="author_keywords",title_similarity=ep_sim)))
      } else {
        counts$conflicts_quarantined <- counts$conflicts_quarantined + 1L
        rec_audit$quarantined <- c(rec_audit$quarantined,list(list(provider="europe_pmc",field="author_keywords",reason="title_guard_failed",title_similarity=ep_sim)))
      }
    }
  }

  still_missing_title <- is_missing(r$canonical$title)
  still_missing_abstract <- is_missing(r$canonical$abstract)

  if(still_missing_title || still_missing_abstract){
    if(skip_scopus){
      rec_audit$scopus <- list(
        outcome="skipped_disabled",
        reason="Scopus temporarily disabled for this Workflow 02 run",
        title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,status=NULL,attempts=0L
      )
    } else {
      counts$scopus_attempted <- counts$scopus_attempted + 1L
      if(scopus_rate_limit_circuit_open){
      sc <- list(
        outcome="technical_error",
        error="SCOPUS_RATE_LIMIT_CIRCUIT_OPEN: request deferred after repeated HTTP 429 responses",
        title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,status=429L,attempts=0L,
        rate_limit_deferred=TRUE
      )
      counts$scopus_rate_limit_circuit_skips <- counts$scopus_rate_limit_circuit_skips + 1L
    } else {
      sc <- tryCatch(
        scopus_lookup(d),
        error=function(e)list(outcome="technical_error",error=conditionMessage(e),title=NULL,abstract=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,status=NULL,attempts=NULL)
      )
      rate_limited <- identical(clean_text(sc$outcome),"technical_error") &&
        grepl("429|RATE_LIMIT",clean_text(sc$error) %||% "",ignore.case=TRUE)
      if(rate_limited){
        consecutive_scopus_rate_limits <- consecutive_scopus_rate_limits + 1L
        counts$scopus_rate_limit_events <- counts$scopus_rate_limit_events + 1L
        if(consecutive_scopus_rate_limits >= scopus_rate_limit_trip){
          scopus_rate_limit_circuit_open <- TRUE
        }
      } else {
        consecutive_scopus_rate_limits <- 0L
      }
    }
    rec_audit$scopus <- sc
    if(identical(sc$status,404L)) counts$scopus_http_404 <- counts$scopus_http_404 + 1L

    if(identical(sc$returned_doi,d)){
      if(still_missing_title && !is.null(sc$title)){
        r$canonical$title <- sc$title
        counts$scopus_title_filled <- counts$scopus_title_filled + 1L
        rec_audit$applied <- c(rec_audit$applied,list(list(provider="scopus",field="title",eid=sc$eid)))
      }
      if(still_missing_abstract && !is.null(sc$abstract)){
        sim <- title_similarity(r$canonical$title,sc$title)
        if(is.null(sc$title) || is_missing(r$canonical$title) || is.na(sim) || sim>=0.90){
          r$canonical$abstract <- sc$abstract
          counts$scopus_abstract_filled <- counts$scopus_abstract_filled + 1L
          rec_audit$applied <- c(rec_audit$applied,list(list(provider="scopus",field="abstract",eid=sc$eid,title_similarity=sim)))
        } else {
          counts$conflicts_quarantined <- counts$conflicts_quarantined + 1L
          rec_audit$quarantined <- c(rec_audit$quarantined,list(list(provider="scopus",field="abstract",reason="title_mismatch",title_similarity=sim,eid=sc$eid)))
        }
      }
    } else if(!is.null(sc$returned_doi)){
      counts$conflicts_quarantined <- counts$conflicts_quarantined + 1L
      rec_audit$quarantined <- c(rec_audit$quarantined,list(list(provider="scopus",reason="returned_doi_mismatch",returned_doi=sc$returned_doi)))
    }
      Sys.sleep(delay)
    }
  }

  still_missing_title <- is_missing(r$canonical$title)
  still_missing_abstract <- is_missing(r$canonical$abstract)
  still_missing_author_keywords <- keywords_missing(r$canonical$author_keywords)

  # Author-keyword repair order after Europe PMC:
  # 1) retained Scopus EID -> FULL;
  # 2) if no retained EID, direct DOI -> FULL;
  # 3) if direct DOI returns no record/404, stop. No DOI-search -> EID fallback.
  eid_for_keywords <- retained_scopus_eid(r)
  if(enrich_author_keywords && still_missing_author_keywords && !skip_scopus){
    counts$scopus_author_keywords_attempted <- counts$scopus_author_keywords_attempted + 1L
    sk <- if(!is.null(eid_for_keywords)){
      tryCatch(
        scopus_lookup_eid_full(eid_for_keywords),
        error=function(e) list(outcome="technical_error",error=conditionMessage(e),title=NULL,author_keywords=character(),returned_doi=NULL,eid=eid_for_keywords,status=NULL,attempts=NULL,route="retained_eid_full")
      )
    } else {
      tryCatch(
        scopus_lookup_doi_full(d),
        error=function(e) list(outcome="technical_error",error=conditionMessage(e),title=NULL,author_keywords=character(),returned_doi=NULL,eid=NULL,status=NULL,attempts=NULL,route="direct_doi_full")
      )
    }
    rec_audit$scopus_keywords <- sk
    sim <- if(!is.null(sk$title) && !is_missing(r$canonical$title)) title_similarity(r$canonical$title,sk$title) else NA_real_
    guard_ok <- identical(sk$returned_doi,d) && !is.null(sk$title) && !is_missing(r$canonical$title) && !is.na(sim) && sim>=0.90
    if(guard_ok && length(sk$author_keywords)){
      r$canonical$author_keywords <- sk$author_keywords
      counts$scopus_author_keywords_filled <- counts$scopus_author_keywords_filled + 1L
      rec_audit$applied <- c(rec_audit$applied,list(list(
        provider="scopus",field="author_keywords",eid=sk$eid,title_similarity=sim,
        route=sk$route,
        identity_guard=if(!is.null(eid_for_keywords))"retained_eid+exact_doi+title" else "direct_doi+exact_doi+title"
      )))
    } else if(length(sk$author_keywords) && !guard_ok){
      counts$conflicts_quarantined <- counts$conflicts_quarantined + 1L
      rec_audit$quarantined <- c(rec_audit$quarantined,list(list(
        provider="scopus",field="author_keywords",reason="keyword_identity_guard_failed",
        route=sk$route,eid=sk$eid,returned_doi=sk$returned_doi,title_similarity=sim
      )))
    }
    Sys.sleep(delay)
  }

  still_missing_title <- is_missing(r$canonical$title)
  still_missing_abstract <- is_missing(r$canonical$abstract)
  still_missing_author_keywords <- keywords_missing(r$canonical$author_keywords)
  if(still_missing_title || still_missing_abstract || (enrich_author_keywords && still_missing_author_keywords)) counts$still_missing_after <- counts$still_missing_after + 1L

  technical_error <- identical(clean_text(ep$outcome),"technical_error") ||
    (!is.null(rec_audit$scopus) && identical(clean_text(rec_audit$scopus$outcome),"technical_error")) ||
    (!is.null(rec_audit$scopus_keywords) && identical(clean_text(rec_audit$scopus_keywords$outcome),"technical_error"))
  if(technical_error) counts$technical_error_records <- counts$technical_error_records + 1L
  filled_fields <- vapply(rec_audit$applied,function(z) clean_text(z$field) %||% "",character(1))
  filled_fields <- unique(filled_fields[nzchar(filled_fields)])
  r$metadata_enrichment <- list(
    workflow="workflow_02_metadata_enrichment",
    implementation_language="R",
    providers=c("europe_pmc","scopus"),
    completed_at=now_utc(),
    doi=d,
    title_missing_after=still_missing_title,
    abstract_missing_after=still_missing_abstract,
    author_keywords_missing_after=still_missing_author_keywords,
    europe_pmc_outcome=clean_text(ep$outcome),
    scopus_outcome=if(is.null(rec_audit$scopus)) NULL else clean_text(rec_audit$scopus$outcome),
    scopus_keywords_outcome=if(is.null(rec_audit$scopus_keywords)) NULL else clean_text(rec_audit$scopus_keywords$outcome),
    technical_error=technical_error,
    filled_fields=filled_fields,
    recheck_after_days=recheck_after_days
  )
  rows[[i]] <- r
  audit[[length(audit)+1L]] <- rec_audit
}

if(!is.null(selected_record_ids)){
  audited_ids <- vapply(audit,function(x) clean_text(x$record_id) %||% "",character(1))
  missing_selected <- setdiff(selected_record_ids,audited_ids)
  if(length(missing_selected)) stop(sprintf("%d selected record IDs were not processed; first: %s",length(missing_selected),paste(head(missing_selected,5),collapse=", ")),call.=FALSE)
}

write_jsonl(rows,output_path)
write_jsonl(audit,audit_path)

report <- list(
  schema="living-evidence-map-workflow02-metadata-enrichment-v1",
  workflow="02_metadata_enrichment",
  implementation_language="R",
  provider_order=if(skip_scopus)c("europe_pmc") else c("europe_pmc","scopus"),
  policy=list(
    eligibility=if(enrich_author_keywords)"DOI present and title, abstract, or author keywords missing" else "DOI present and title or abstract missing",
    overwrite_existing_fields=FALSE,
    europe_pmc_match="exact normalised DOI plus canonical/provider title similarity >= 0.90 for abstract/author-keyword fills; exact DOI may fill a missing title",
    scopus_match="direct DOI Abstract Retrieval with view=META_ABS only; no Search-to-EID fallback",
    abstract_title_guard="if a title is present on both sides, Jaro-Winkler similarity must be >= 0.90; otherwise quarantine",
    provider_fallback=if(skip_scopus)"Scopus disabled for this run; unresolved metadata is retained unchanged and is not classified as a technical failure." else "existing Scopus DOI/EID path retained for missing title/abstract; author keywords are retained only when returned opportunistically by an already-required Europe PMC or Scopus lookup; no dedicated keyword-only Scopus FULL request",
    repeat_policy=sprintf("successful/no-result attempts are deferred for %.0f days; technical failures are eligible for retry on the next run",recheck_after_days),
    rate_limit_policy=sprintf("honour Retry-After/X-RateLimit-Reset up to %.0f seconds; open Scopus circuit after %d consecutive rate-limit failures",max_rate_limit_wait,scopus_rate_limit_trip)
  ),
  trial_limit=if(is.infinite(limit)) NULL else limit,
  selected_record_ids_file=if(is.null(record_ids_path)) NULL else record_ids_path,
  selected_record_ids=if(is.null(selected_record_ids)) NULL else length(selected_record_ids),
  processed_eligible_records=processed_eligible,
  counts=counts,
  input_sha256=input_sha,
  output_sha256=digest(file=output_path,algo="sha256",serialize=FALSE),
  audit_sha256=digest(file=audit_path,algo="sha256",serialize=FALSE),
  completed_at=now_utc()
)
dir.create(dirname(report_path),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),report_path,useBytes=TRUE)
cat(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),"\n")
