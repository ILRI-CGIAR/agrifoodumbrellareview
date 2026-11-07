#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(jsonlite);library(httr2);library(digest)})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
input_path <- arg("--input")
output_path <- arg("--output")
audit_path <- arg("--audit")
report_path <- arg("--report")
limit <- as.integer(arg("--limit","0"))
batch_size <- as.integer(arg("--batch-size","100"))
if(any(vapply(list(input_path,output_path,audit_path,report_path),is.null,logical(1)))) stop("Required: --input --output --audit --report",call.=FALSE)
if(!file.exists(input_path)) stop("Input lean canonical JSONL not found",call.=FALSE)
if(is.na(limit)||limit<0L) stop("--limit must be >= 0",call.=FALSE)
if(is.na(batch_size)||batch_size<1L||batch_size>100L) stop("--batch-size must be 1..100",call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
clean <- function(x){if(is.null(x)||!length(x))return("");s<-trimws(as.character(x[[1L]]));if(is.na(s))"" else s}
strip_doi <- function(x){x<-tolower(clean(x));x<-sub("^https?://(dx\\.)?doi\\.org/","",x);x<-sub("^doi:","",x);x}
openalex_key <- Sys.getenv("OPENALEX_API_KEY")
checked_at <- format(Sys.Date(),"%Y-%m-%d")

patterns <- list(
  retracted=c("editorial expression of retraction","notice of retraction","retraction notice","retraction","retracted"),
  withdrawn=c("notice of withdrawal","withdrawal notice","withdrawal","withdrawn"),
  expression_of_concern=c("editorial expression of concern","expression of concern"),
  warning=c("editorial warning","warning notice"),
  corrected=c("publisher correction","author correction","notice of correction","correction notice","correction","corrigendum","erratum","errata")
)
prefix_match <- function(title){
  t <- clean(title)
  if(!nzchar(t)) return(list(matched=FALSE,code=NULL,detected_type=NULL,matched_prefix=NULL))
  candidates <- list()
  n <- 0L
  for(code in names(patterns)){
    for(p in patterns[[code]]){
      esc <- gsub("([][{}()+*^$|\\\\.?])","\\\\\\1",p)
      rx <- paste0("(?i)^[[:space:][:punct:]]*",esc,"\\b")
      if(grepl(rx,t,perl=TRUE)){
        n<-n+1L
        candidates[[n]]<-list(code=code,prefix=p,n=nchar(p))
      }
    }
  }
  if(!length(candidates)) return(list(matched=FALSE,code=NULL,detected_type=NULL,matched_prefix=NULL))
  lens <- vapply(candidates,function(z)z$n,integer(1))
  z <- candidates[[which.max(lens)]]
  list(matched=TRUE,code=z$code,detected_type=z$prefix,matched_prefix=z$prefix)
}
precedence <- c(retracted=1L,withdrawn=2L,expression_of_concern=3L,warning=4L,corrected=5L,normal=6L)
resolve_code <- function(oa,title){
  positives <- character()
  if(isTRUE(oa$is_retracted)) positives <- c(positives,"retracted")
  if(identical(clean(oa$type),"retraction")) positives <- c(positives,"retracted")
  if(identical(clean(oa$type),"erratum")) positives <- c(positives,"corrected")
  if(isTRUE(title$matched) && nzchar(clean(title$code))) positives <- c(positives,title$code)
  positives <- unique(positives)
  if(!length(positives)) return(list(code="normal",conflict=FALSE,multiple_positive_signals=FALSE,positive_codes=character()))
  ord <- order(precedence[positives],na.last=TRUE)
  code <- positives[[ord[[1L]]]]
  # Multiple positive status signals are not an unresolved conflict when
  # deterministic precedence yields a definitive analytical code.
  list(code=code,conflict=FALSE,multiple_positive_signals=length(positives)>1L,positive_codes=positives)
}

read_records <- function(path,limit=0L){
  con<-file(path,"rt",encoding="UTF-8");on.exit(close(con),add=TRUE)
  out<-list();n<-0L
  repeat{
    ln<-readLines(con,n=1L,warn=FALSE)
    if(!length(ln))break
    if(!nzchar(trimws(ln)))next
    n<-n+1L
    out[[n]]<-fromJSON(ln,simplifyVector=FALSE)
    if(limit>0L && n>=limit)break
  }
  out
}
rows <- read_records(input_path,limit)
if(!length(rows)) stop("No canonical records read",call.=FALSE)
ids <- vapply(rows,function(r)clean((r$identity%||%list())$record_id),character(1))
if(any(!nzchar(ids))||anyDuplicated(ids)) stop("Missing or duplicate record_id in input subset",call.=FALSE)

meta <- lapply(rows,function(r){
  can<-r$canonical%||%list()
  refs<-r$manifestation_refs%||%list()
  refs<-if(length(refs)) unlist(refs,use.names=FALSE) else character()
  oa_refs<-sub("^openalex:","",refs[grepl("^openalex:",refs,ignore.case=TRUE)])
  oa_refs<-oa_refs[nzchar(oa_refs)]
  list(record_id=clean((r$identity%||%list())$record_id),title=clean(can$title),doi=strip_doi(can$doi),
       openalex_id=if(length(oa_refs))oa_refs[[1L]] else "")
})

make_req <- function(url,query=list()){
  req<-request(url)
  if(length(query)) req<-do.call(req_url_query,c(list(req),query))
  if(nzchar(openalex_key)) req<-req_headers(req,Authorization=paste("Bearer",openalex_key))
  req |> req_timeout(60) |> req_retry(max_tries=5,retry_on_failure=TRUE,backoff=function(tries)min(30,2^(tries-1L)))
}
oa_map <- new.env(parent=emptyenv())
audit <- list(); audit_n <- 0L

doi_idx <- which(vapply(meta,function(z)nzchar(z$doi),logical(1)))
if(length(doi_idx)){
  batches <- split(doi_idx,ceiling(seq_along(doi_idx)/batch_size))
  for(b in batches){
    dois <- vapply(meta[b],function(z)z$doi,character(1))
    filt <- paste(dois,collapse="|")
    req <- make_req("https://api.openalex.org/works",list(filter=paste0("doi:",filt),per_page=100,select="id,doi,is_retracted,type"))
    resp <- req |> req_error(is_error=function(resp)FALSE) |> req_perform()
    st <- resp_status(resp)
    if(st!=200L){
      audit_n<-audit_n+1L;audit[[audit_n]]<-list(kind="doi_batch_error",http_status=st,dois=dois)
      for(d in dois) oa_map[[paste0("doi:",d)]] <- list(
        work_id=NULL,is_retracted=NULL,type=NULL,lookup_status=paste0("http_",st)
      )
      next
    }
    body <- resp_body_json(resp,simplifyVector=FALSE)
    for(w in body$results%||%list()){
      d <- strip_doi(w$doi)
      if(nzchar(d)) oa_map[[paste0("doi:",d)]] <- list(work_id=sub("^https://openalex.org/","",clean(w$id)),is_retracted=isTRUE(w$is_retracted),type=clean(w$type),lookup_status="found")
    }
  }
}

for(i in seq_along(meta)){
  z<-meta[[i]]
  if(nzchar(z$doi) && exists(paste0("doi:",z$doi),envir=oa_map,inherits=FALSE)) next
  if(!nzchar(z$doi) && nzchar(z$openalex_id)){
    wid<-sub("^https://openalex.org/","",z$openalex_id,ignore.case=TRUE)
    req<-make_req(paste0("https://api.openalex.org/works/",URLencode(wid,reserved=TRUE)),list(select="id,doi,is_retracted,type"))
    resp<-req |> req_error(is_error=function(resp)FALSE) |> req_perform()
    st<-resp_status(resp)
    if(st==200L){
      w<-resp_body_json(resp,simplifyVector=FALSE)
      oa_map[[paste0("wid:",wid)]]<-list(work_id=sub("^https://openalex.org/","",clean(w$id)),is_retracted=isTRUE(w$is_retracted),type=clean(w$type),lookup_status="found")
    } else {
      oa_map[[paste0("wid:",wid)]]<-list(work_id=wid,is_retracted=NULL,type=NULL,lookup_status=if(st==404L)"not_found" else paste0("http_",st))
      if(st!=404L){audit_n<-audit_n+1L;audit[[audit_n]]<-list(kind="openalex_id_error",record_id=z$record_id,openalex_id=wid,http_status=st)}
    }
  }
}

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
dir.create(dirname(audit_path),recursive=TRUE,showWarnings=FALSE)
outcon<-file(output_path,"wt",encoding="UTF-8");on.exit(close(outcon),add=TRUE)
counts<-setNames(integer(length(precedence)),names(precedence)); conflicts<-0L; multiple_signals<-0L; excluded<-0L; found<-0L; notfound<-0L; unavailable<-0L; technical_lookup_errors<-0L

for(i in seq_along(meta)){
  z<-meta[[i]]
  lookup_type<-NULL;lookup_id<-NULL
  oa<-list(work_id=NULL,is_retracted=NULL,type=NULL,lookup_status="unavailable")
  if(nzchar(z$doi)){
    lookup_type<-"doi";lookup_id<-z$doi
    key<-paste0("doi:",z$doi)
    if(exists(key,envir=oa_map,inherits=FALSE)) oa<-oa_map[[key]] else oa$lookup_status<-"not_found"
  } else if(nzchar(z$openalex_id)){
    lookup_type<-"openalex";lookup_id<-sub("^https://openalex.org/","",z$openalex_id,ignore.case=TRUE)
    key<-paste0("wid:",lookup_id)
    if(exists(key,envir=oa_map,inherits=FALSE)) oa<-oa_map[[key]]
  }
  if(identical(oa$lookup_status,"found")) {
    found<-found+1L
  } else if(identical(oa$lookup_status,"not_found")) {
    notfound<-notfound+1L
  } else {
    unavailable<-unavailable+1L
    if(grepl("^http_",oa$lookup_status%||%"")) technical_lookup_errors<-technical_lookup_errors+1L
  }
  tm<-prefix_match(z$title)
  res<-resolve_code(oa,tm)
  code<-res$code
  counts[[code]]<-counts[[code]]+1L
  if(isTRUE(res$conflict))conflicts<-conflicts+1L
  if(isTRUE(res$multiple_positive_signals))multiple_signals<-multiple_signals+1L
  excl<-code%in%c("retracted","withdrawn")
  if(excl)excluded<-excluded+1L

  state<-list(
    record_id=z$record_id,
    publication_status=list(
      code=code,
      exclude_from_workflow04=excl,
      needs_review=grepl("^http_",oa$lookup_status%||%""),
      evidence_conflict=isTRUE(res$conflict),
      multiple_positive_signals=isTRUE(res$multiple_positive_signals),
      positive_codes=if(length(res$positive_codes))res$positive_codes else list(),
      lookup=list(identifier_type=lookup_type,identifier=lookup_id,status=oa$lookup_status),
      evidence=list(
        openalex=list(work_id=oa$work_id,is_retracted=oa$is_retracted,work_type=oa$type),
        title_notice=list(matched=isTRUE(tm$matched),detected_type=tm$detected_type,matched_prefix=tm$matched_prefix)
      ),
      checked_at=checked_at
    )
  )
  writeLines(toJSON(state,auto_unbox=TRUE,null="null",na="null",digits=NA),outcon,useBytes=TRUE)
}
close(outcon);on.exit(NULL,add=FALSE)

acon<-file(audit_path,"wt",encoding="UTF-8");on.exit(close(acon),add=TRUE)
if(length(audit)) for(a in audit) writeLines(toJSON(a,auto_unbox=TRUE,null="null",na="null"),acon,useBytes=TRUE)
close(acon);on.exit(NULL,add=FALSE)

report<-list(
  schema="living-evidence-map-workflow03-publication-status-v1",
  status="PASS",
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  records_scanned=length(meta),
  full_input_requested=limit==0L,
  code_counts=as.list(counts),
  exclude_from_workflow04=excluded,
  evidence_conflicts=conflicts,
  multiple_positive_signals=multiple_signals,
  openalex_lookup=list(found=found,not_found=notfound,unavailable_or_error=unavailable,technical_errors=technical_lookup_errors),
  output_sha256=digest(file=output_path,algo="sha256",serialize=FALSE),
  checked_at=checked_at
)
if(technical_lookup_errors>0L) report$status <- "FAIL_PROVIDER_ERRORS"
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),report_path,useBytes=TRUE)
if(technical_lookup_errors>0L) {
  stop(sprintf("Workflow 03 blocked: %d record(s) have unresolved OpenAlex technical lookup errors",technical_lookup_errors),call.=FALSE)
}
cat(sprintf("PASS: Workflow 03 publication-status scan processed %d records; excluded=%d conflicts=%d\n",length(meta),excluded,conflicts))
