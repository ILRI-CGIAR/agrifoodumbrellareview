#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i<-match(flag,args); if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
input_path<-arg("--input"); patch_path<-arg("--patch"); output_path<-arg("--output"); report_path<-arg("--report")
mode<-arg("--mode","fill_missing")
if(any(vapply(list(input_path,patch_path,output_path,report_path),is.null,logical(1)))) stop("Required: --input --patch --output --report",call.=FALSE)
if(!(mode %in% c("fill_missing","exact"))) stop("--mode must be fill_missing or exact",call.=FALSE)

`%||%`<-function(x,y) if(is.null(x)) y else x
clean<-function(x){
  if(is.null(x)||!length(x)) return(NULL)
  s<-trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])))
  if(is.na(s)||!nzchar(s)) NULL else s
}
missing<-function(x) is.null(clean(x))
norm_doi<-function(x){
  z<-clean(x)
  if(is.null(z)) return(NULL)
  z<-tolower(z)
  z<-sub("^https?://(dx\\.)?doi\\.org/","",z,perl=TRUE)
  z<-sub("^doi:\\s*","",z,perl=TRUE)
  z<-sub("[?#].*$","",z,perl=TRUE)
  z<-sub("/full/html?$","",z,perl=TRUE,ignore.case=TRUE)
  z<-sub("\\.(html?|pdf|xml)$","",z,perl=TRUE,ignore.case=TRUE)
  z<-sub("[.,;:]+$","",z,perl=TRUE)
  if(!nzchar(z)) NULL else z
}
field_missing<-function(field,x){
  if(identical(field,"author_keywords")){
    if(is.null(x)||!length(x)) return(TRUE)
    z<-trimws(gsub("[[:space:]]+"," ",as.character(unlist(x,use.names=FALSE))))
    return(!length(z[!is.na(z)&nzchar(z)]))
  }
  missing(x)
}
readjl<-function(p){
  x<-readLines(p,warn=FALSE,encoding="UTF-8");x<-x[nzchar(trimws(x))]
  lapply(seq_along(x),function(i) fromJSON(x[[i]],simplifyVector=FALSE))
}
patches<-readjl(patch_path)
ids<-vapply(patches,function(x) clean(x$record_id)%||%"",character(1))
if(any(!nzchar(ids))||anyDuplicated(ids)) stop("Patch ledger has missing/duplicate record IDs",call.=FALSE)
pmap<-setNames(patches,ids)

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
pin<-file(input_path,"rt",encoding="UTF-8");pout<-file(output_path,"wt",encoding="UTF-8")
on.exit({try(close(pin),silent=TRUE);try(close(pout),silent=TRUE)},add=TRUE)
seen<-character(); applied_fields<-0L; conflicts<-list(); stale_patches<-list(); records<-0L
repeat{
  line<-readLines(pin,n=1L,warn=FALSE)
  if(!length(line)) break
  if(!nzchar(trimws(line))) next
  records<-records+1L
  r<-fromJSON(line,simplifyVector=FALSE)
  rid<-clean((r$identity%||%list())$record_id)
  if(is.null(rid)) stop(sprintf("Input line %d missing identity.record_id",records),call.=FALSE)
  p<-pmap[[rid]]
  if(!is.null(p)){
    seen<-c(seen,rid)
    if(is.null(r$canonical)) r$canonical<-list()

    patch_doi<-norm_doi(p$input_doi)
    current_doi<-norm_doi(r$canonical$doi)
    doi_compatible<-!is.null(patch_doi) && !is.null(current_doi) && identical(patch_doi,current_doi)
    if(!doi_compatible){
      stale<-list(
        record_id=rid,
        patch_input_doi=p$input_doi,
        current_doi=r$canonical$doi,
        reason=if(is.null(patch_doi))"patch_missing_input_doi" else if(is.null(current_doi))"current_record_missing_doi" else "input_doi_changed"
      )
      stale_patches[[length(stale_patches)+1L]]<-stale
      if(mode=="exact"){
        stop(sprintf(
          "Exact patch identity conflict for %s: patch DOI=%s current DOI=%s",
          rid,
          clean(p$input_doi)%||%"<missing>",
          clean(r$canonical$doi)%||%"<missing>"
        ),call.=FALSE)
      }
      writeLines(toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),pout,useBytes=TRUE)
      next
    }

    for(field in c("title","abstract","author_keywords")){
      z<-p[[field]]
      if(is.null(z)) next
      newv<-z$value
      cur<-r$canonical[[field]]
      can_apply<-field_missing(field,cur) || identical(cur,newv)
      if(mode=="exact" && !can_apply) stop(sprintf("Exact patch conflict for %s %s",rid,field),call.=FALSE)
      if(can_apply){
        r$canonical[[field]]<-newv
        applied_fields<-applied_fields+1L
      } else {
        conflicts[[length(conflicts)+1L]]<-list(record_id=rid,field=field,current_value=cur,patch_value=newv)
      }
    }
    r$metadata_enrichment<-p$metadata_enrichment
  }
  writeLines(toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),pout,useBytes=TRUE)
}
close(pin);close(pout);on.exit(NULL,add=FALSE)
missing_patch_ids<-setdiff(ids,seen)
if(mode=="exact" && length(missing_patch_ids)) stop(sprintf("Exact replay missing %d patch record IDs",length(missing_patch_ids)),call.=FALSE)
report<-list(
  schema="living-evidence-map-workflow02-patch-application-v1",
  status=if(length(conflicts))"PASS_WITH_CONFLICTS" else "PASS",
  mode=mode,
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  patch_sha256=digest(file=patch_path,algo="sha256",serialize=FALSE),
  output_sha256=digest(file=output_path,algo="sha256",serialize=FALSE),
  input_records=records,
  patch_records=length(patches),
  matched_patch_records=length(unique(seen)),
  missing_patch_record_ids=missing_patch_ids,
  applied_fields=applied_fields,
  conflicts=conflicts,
  stale_patch_records=length(stale_patches),
  stale_patches=stale_patches,
  completed_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
dir.create(dirname(report_path),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),report_path,useBytes=TRUE)
cat(sprintf("PASS: applied Workflow 02 patch to %d records; matched %d patch records; %d field conflicts; %d stale patches skipped\n",
            records,length(unique(seen)),length(conflicts),length(stale_patches)))
