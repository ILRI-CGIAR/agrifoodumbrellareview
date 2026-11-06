#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args); if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
input_path <- arg("--input")
enriched_path <- arg("--enriched")
audit_path <- arg("--audit")
enrichment_report_path <- arg("--enrichment-report")
patch_path <- arg("--patch")
retry_path <- arg("--retry-queue")
report_path <- arg("--report")
required <- list(input_path,enriched_path,audit_path,enrichment_report_path,patch_path,retry_path,report_path)
if(any(vapply(required,is.null,logical(1)))) stop("Missing required argument",call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
readjl <- function(p){
  x <- readLines(p,warn=FALSE,encoding="UTF-8"); x <- x[nzchar(trimws(x))]
  lapply(seq_along(x),function(i) tryCatch(fromJSON(x[[i]],simplifyVector=FALSE),
    error=function(e) stop(sprintf("Invalid JSONL %s line %d: %s",p,i,conditionMessage(e)),call.=FALSE)))
}
writejl <- function(xs,p){
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  con <- file(p,"wt",encoding="UTF-8"); on.exit(close(con))
  if(length(xs)) for(x in xs) writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
}
clean <- function(x){
  if(is.null(x)||!length(x)) return(NULL)
  s<-trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])))
  if(is.na(s)||!nzchar(s)) NULL else s
}
missing <- function(x) is.null(clean(x))
keywords_missing <- function(x){
  if(is.null(x) || !length(x)) return(TRUE)
  vals <- trimws(gsub("[[:space:]]+"," ",as.character(unlist(x,use.names=FALSE))))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  !length(vals)
}
rid <- function(r) clean((r$identity %||% list())$record_id)
same_without_allowed <- function(a,b){
  aa<-a; bb<-b
  if(is.null(aa$canonical)) aa$canonical<-list()
  if(is.null(bb$canonical)) bb$canonical<-list()
  aa$canonical$title<-NULL; aa$canonical$abstract<-NULL; aa$canonical$author_keywords<-NULL
  bb$canonical$title<-NULL; bb$canonical$abstract<-NULL; bb$canonical$author_keywords<-NULL
  aa$metadata_enrichment<-NULL; bb$metadata_enrichment<-NULL
  identical(aa,bb)
}

inp <- readjl(input_path); out <- readjl(enriched_path); aud <- readjl(audit_path)
er <- fromJSON(enrichment_report_path,simplifyVector=FALSE)
if(length(inp)!=length(out)) stop("Input/output record counts differ",call.=FALSE)
eligible_total <- as.integer(er$counts$eligible_doi_missing_metadata %||% length(aud))
deferred <- as.integer(er$counts$deferred_recent_attempts %||% 0L)
attemptable <- max(0L,eligible_total-deferred)
lim <- er$trial_limit %||% Inf
processed_reported <- er$processed_eligible_records %||% NULL
expected_audit <- if(!is.null(processed_reported)) {
  as.integer(processed_reported)
} else if(is.infinite(lim)) attemptable else min(attemptable,as.integer(lim))
if(length(aud) != expected_audit) {
  stop(sprintf("Audit cardinality does not match processed eligible records: expected %d found %d",expected_audit,length(aud)),call.=FALSE)
}
aud_by_id <- setNames(aud,vapply(aud,function(x) clean(x$record_id) %||% "",character(1)))
if(anyDuplicated(names(aud_by_id))) stop("Duplicate record IDs in enrichment audit",call.=FALSE)

patches <- list(); retry <- list()
changed_title <- 0L; changed_abstract <- 0L; changed_author_keywords <- 0L; attempted <- 0L; technical <- 0L
for(i in seq_along(inp)){
  a<-inp[[i]]; b<-out[[i]]
  ida<-rid(a); idb<-rid(b)
  if(is.null(ida)||!identical(ida,idb)) stop(sprintf("Record identity/order mismatch at line %d",i),call.=FALSE)
  if(!identical(a$identity,b$identity)) stop(sprintf("identity changed for %s",ida),call.=FALSE)
  if(!identical(a$manifestations,b$manifestations)) stop(sprintf("manifestations changed for %s",ida),call.=FALSE)
  if(!identical((a$canonical %||% list())$doi,(b$canonical %||% list())$doi)) stop(sprintf("DOI changed for %s",ida),call.=FALSE)
  if(!same_without_allowed(a,b)) stop(sprintf("Workflow 02 changed a non-permitted field for %s",ida),call.=FALSE)

  at<-(a$canonical %||% list())$title; bt<-(b$canonical %||% list())$title
  aa<-(a$canonical %||% list())$abstract; ba<-(b$canonical %||% list())$abstract
  ak<-(a$canonical %||% list())$author_keywords; bk<-(b$canonical %||% list())$author_keywords
  tchg <- !identical(at,bt); achg <- !identical(aa,ba); kchg <- !identical(ak,bk)
  if(tchg && (!missing(at) || missing(bt))) stop(sprintf("Invalid title overwrite for %s",ida),call.=FALSE)
  if(achg && (!missing(aa) || missing(ba))) stop(sprintf("Invalid abstract overwrite for %s",ida),call.=FALSE)
  if(kchg && (!keywords_missing(ak) || keywords_missing(bk))) stop(sprintf("Invalid author_keywords overwrite for %s",ida),call.=FALSE)
  if(tchg) changed_title<-changed_title+1L
  if(achg) changed_abstract<-changed_abstract+1L
  if(kchg) changed_author_keywords<-changed_author_keywords+1L

  meta <- b$metadata_enrichment
  meta_changed <- !identical(a$metadata_enrichment,b$metadata_enrichment)
  if(meta_changed || tchg || achg || kchg){
    attempted<-attempted+1L
    au <- aud_by_id[[ida]]
    if(is.null(au)) stop(sprintf("new Workflow 02 state exists without audit row for %s",ida),call.=FALSE)
    applied <- au$applied %||% list()
    provider_for <- function(field){
      hits <- Filter(function(z) identical(clean(z$field),field),applied)
      if(!length(hits)) NULL else clean(hits[[1L]]$provider)
    }
    p <- list(
      record_id=ida,
      input_doi=clean((a$canonical %||% list())$doi),
      title=if(tchg) list(value=bt,provider=provider_for("title")) else NULL,
      abstract=if(achg) list(value=ba,provider=provider_for("abstract")) else NULL,
      author_keywords=if(kchg) list(value=bk,provider=provider_for("author_keywords")) else NULL,
      metadata_enrichment=meta,
      audit=au
    )
    patches[[length(patches)+1L]] <- p

    eptech <- identical(clean((au$europe_pmc %||% list())$outcome),"technical_error")
    sctech <- identical(clean((au$scopus %||% list())$outcome),"technical_error")
    sktech <- identical(clean((au$scopus_keywords %||% list())$outcome),"technical_error")
    if(eptech || sctech || sktech){
      technical<-technical+1L
      retry[[length(retry)+1L]] <- list(
        record_id=ida,
        doi=clean((a$canonical %||% list())$doi),
        title_missing_after=missing(bt),
        abstract_missing_after=missing(ba),
        author_keywords_missing_after=keywords_missing(bk),
        europe_pmc_technical_error=eptech,
        scopus_technical_error=sctech,
        scopus_keywords_technical_error=sktech,
        audit=au
      )
    }
  } else if(tchg || achg || kchg) stop(sprintf("Changed metadata without Workflow 02 provenance for %s",ida),call.=FALSE)
}

writejl(patches,patch_path); writejl(retry,retry_path)
report <- list(
  schema="living-evidence-map-workflow02-patch-report-v1",
  status="PASS",
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  enriched_sha256=digest(file=enriched_path,algo="sha256",serialize=FALSE),
  enrichment_report_sha256=digest(file=enrichment_report_path,algo="sha256",serialize=FALSE),
  patch_sha256=digest(file=patch_path,algo="sha256",serialize=FALSE),
  retry_queue_sha256=digest(file=retry_path,algo="sha256",serialize=FALSE),
  records=length(inp),
  attempted_records=attempted,
  patch_records=length(patches),
  title_fills=changed_title,
  abstract_fills=changed_abstract,
  author_keyword_fills=changed_author_keywords,
  technical_retry_records=length(retry),
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null"),report_path,useBytes=TRUE)
cat(sprintf("PASS: Workflow 02 patch ledger: %d attempted records; %d title fills; %d abstract fills; %d author-keyword fills; %d technical retries\n",
            attempted,changed_title,changed_abstract,changed_author_keywords,length(retry)))
