#!/usr/bin/env Rscript
# Audit-only recovery of original W02 run. No API calls.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
a<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,a);if(is.na(i)||i==length(a))stop("Missing ",k);a[[i+1L]]}
review<-arg("--review-jsonl");manifest<-arg("--review-manifest")
approved_path<-arg("--accept-keys");original_patch<-arg("--patch")
out_patch<-arg("--out-patch");out_audit<-arg("--out-audit")
out_decisions<-arg("--out-decisions");out_report<-arg("--out-report")
source("scripts/updater/workflow_02_title_review_rules.R",local=TRUE)
or<-function(x,y)if(is.null(x))y else x
cl<-function(x){if(is.null(x)||!length(x))return(NULL);y<-trimws(as.character(x[[1L]]));if(is.na(y)||!nzchar(y))NULL else y}
doi<-function(x){v<-tolower(or(cl(x),""));v<-sub("^https?://(dx\\.)?doi\\.org/","",v,perl=TRUE);v<-sub("^doi:\\s*","",v,perl=TRUE);sub("[.,;:]+$","",v,perl=TRUE)}
readjl<-function(path){x<-readLines(path,warn=FALSE);x<-x[nzchar(trimws(x))];lapply(x,fromJSON,simplifyVector=FALSE)}
writejl<-function(xs,p){dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);c<-file(p,"wt",encoding="UTF-8");on.exit(close(c));for(z in xs)writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),c,useBytes=TRUE)}
m<-fromJSON(manifest,simplifyVector=FALSE)
s<-digest(file=review,algo="sha256",serialize=FALSE)
stopifnot(identical(m$review_jsonl$sha256,s))
cases<-readjl(review)
stopifnot(length(cases)==100L,as.integer(m$blocking_conflicts)==100L,as.integer(m$blocking_records)==94L)
keys<-vapply(cases,function(z)paste(cl(z$record_id),cl(z$conflict$provider),sep="@"),character(1))
stopifnot(!anyDuplicated(keys))
accept<-trimws(readLines(approved_path,warn=FALSE));accept<-accept[nzchar(accept)&!startsWith(accept,"#")]
stopifnot(length(accept)==70L,!anyDuplicated(accept),all(accept %in% keys))
patches<-readjl(original_patch)
ids<-vapply(patches,function(z)or(cl(z$record_id),""),character(1))
stopifnot(length(patches)==5595L,!anyDuplicated(ids))
index<-setNames(seq_along(ids),ids)
groups<-split(seq_along(cases),vapply(cases,function(z)z$record_id,character(1)))
decisions<-vector("list",length(cases))
filled<-0L
for(id in names(groups)){
  pos<-index[[id]]
  if(is.null(pos))stop("Review case missing from patch: ",id)
  p<-patches[[pos]]
  if(is.null(p$audit))stop("Patch has no audit: ",id)
  q<-or(p$audit$quarantined,list())
  chosen<-groups[[id]][keys[groups[[id]]] %in% accept]
  if(length(chosen)){
    chosen<-chosen[order(vapply(chosen,function(i)if(identical(cases[[i]]$conflict$provider,"europe_pmc"))0L else 1L,integer(1)))]
    for(i in chosen){
      z<-cases[[i]];rp<-z$provider_response
      if(is.null(rp))stop("No provider response: ",keys[[i]])
      if(!nzchar(doi(p$input_doi))||!identical(doi(z$canonical$doi),doi(p$input_doi))||!identical(doi(rp$returned_doi),doi(p$input_doi)))
        stop("Exact DOI identity guard failed: ",keys[[i]])
      title_decision<-w02_title_equivalence(z$canonical$title,rp$title)
      if(!title_decision %in% c("exact_after_markup_normalisation","complete_title_with_appended_translation"))
        stop("Title identity rule failed: ",keys[[i]]," / ",title_decision)
      if(is.null(cl(rp$abstract)))stop("Accepted provider abstract missing: ",keys[[i]])
      if(is.null(p$abstract)){p$abstract<-list(value=rp$abstract,provider=z$conflict$provider);filled<-filled+1L}
    }
  }
  for(i in groups[[id]]){
    z<-cases[[i]];accepted<-keys[[i]] %in% accept
    ix<-which(vapply(q,function(v)identical(cl(v$provider),cl(z$conflict$provider))&&
      identical(cl(v$field),cl(z$conflict$field))&&identical(cl(v$reason),cl(z$conflict$reason)),logical(1)))
    if(length(ix)!=1L)stop("Quarantine identity is not unique: ",keys[[i]]," (",length(ix),")")
    q<-q[-ix]
    entry<-list(record_id=id,provider=z$conflict$provider,field=z$conflict$field,
      decision=if(accepted)"accept_provider_abstract" else "reject_provider_abstract",
      rationale=if(accepted)"user_approved_title_equivalence_with_exact_doi" else "no_equivalence_or_unresolved_correction",
      source_run_id="42405126427",review_manifest_sha256=digest(file=manifest,algo="sha256",serialize=FALSE))
    decisions[[i]]<-entry
    p$audit$human_review_decisions<-c(or(p$audit$human_review_decisions,list()),list(entry))
    if(accepted)p$audit$applied<-c(or(p$audit$applied,list()),list(list(provider=z$conflict$provider,field="abstract",basis="human_review_title_and_doi")))
  }
  p$audit$quarantined<-q
  patches[[pos]]<-p
}
stopifnot(all(vapply(decisions,function(z)!is.null(z),logical(1))))
writejl(patches,out_patch)
writejl(lapply(patches,function(z)z$audit),out_audit)
writejl(decisions,out_decisions)
report<-list(schema="ilri-w02-reviewed-recovery-v1",source_run_id="42405126427",status="PASS",
 review_sha256=s,accepted_conflicts=70L,rejected_conflicts=30L,accepted_abstract_fills=filled,
 reviewed_conflicts=100L,record_count=94L,source_patch_sha256=digest(file=original_patch,algo="sha256",serialize=FALSE),
 reconciled_patch_sha256=digest(file=out_patch,algo="sha256",serialize=FALSE),
 decisions_sha256=digest(file=out_decisions,algo="sha256",serialize=FALSE),api_calls=0L)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null"),out_report)
cat(sprintf("PASS: 100 W02 decisions recorded, %d abstract fills, no API calls\n",filled))
