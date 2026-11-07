#!/usr/bin/env Rscript
# Assemble existing W01 catalogue from prepared API records and new Shiny RIS restoration.
# This function never downloads records or launches deduplication.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- which(args==flag)
  if(!length(i)) return(default)
  if(length(i)!=1L || i==length(args)) stop("Invalid argument ",flag,call.=FALSE)
  args[[i+1L]]
}
reference <- arg("--input-manifest")
api_catalogue <- arg("--api-catalogue")
manual_manifest <- arg("--manual-manifest")
output <- arg("--output")
if(any(vapply(list(reference,api_catalogue,manual_manifest,output),is.null,logical(1))))
  stop("Required: --input-manifest --api-catalogue --manual-manifest --output",call.=FALSE)
read <- function(p) {if(!file.exists(p))stop("Missing manifest: ",p,call.=FALSE);fromJSON(p,simplifyVector=FALSE)}
refs<-read(reference);api<-read(api_catalogue);manual<-read(manual_manifest)
if(!identical(refs$schema,"living-evidence-map-workflow01-review-input-references-v1") ||
   !identical(api$schema,"living-evidence-map-workflow01-source-catalogue-v1") ||
   !identical(manual$schema,"living-evidence-map-workflow01-restored-manual-ris-v1") ||
   !identical(as.character(refs$review_id),as.character(manual$review_id)) ||
   !identical(as.character(refs$review_id),as.character(api$review_id)))
  stop("W01 input manifest/review identity mismatch",call.=FALSE)
sources<-api$sources
if(is.null(sources)||!length(sources)||is.null(names(sources)))
  stop("Prepared API catalogue must include sources",call.=FALSE)
expected<-refs$manual_ris
if(is.null(expected)) expected<-list()
rows<-manual$sources
if(is.null(rows)) rows<-list()
reference_ids<-vapply(expected,function(x)as.character(x$submission_id),character(1))
actual_ids<-vapply(rows,function(x)as.character(x$submission_id),character(1))
if(anyDuplicated(actual_ids)||!setequal(reference_ids,actual_ids))
  stop("Manual RIS submissions differ from frozen W01 input references",call.=FALSE)
for(z in rows){
  submission<-as.character(z$submission_id)
  match_id<-match(submission,reference_ids)
  ref<-expected[[match_id]]
  src<-as.character(z$source)
  path<-as.character(z$path)
  expected_n<-as.integer(z$records)
  if(!grepl("^[a-z0-9][a-z0-9_-]*$",src) || src %in% names(sources) ||
     !file.exists(path) || is.na(expected_n) || expected_n<0L ||
     !identical(as.character(z$zenodo_record_id),as.character(ref$zenodo_record_id)) ||
     !identical(expected_n,as.integer(ref$records_downloaded)) ||
     !grepl("^[0-9a-fA-F]{64}$",as.character(z$sha256)) ||
     !identical(tolower(as.character(z$sha256)),
                tolower(digest::digest(file=path,algo="sha256",serialize=FALSE))))
    stop("Invalid manual RIS source manifest for submission ",submission,call.=FALSE)
  sources[[src]]<-list(kind="prepared_manual_ris",source=src,prepared_path=path,
    expected_records=expected_n,sha256=tolower(as.character(z$sha256)),submission_id=submission,
    zenodo_record_id=as.character(z$zenodo_record_id))
}
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
write_json(list(schema="living-evidence-map-workflow01-source-catalogue-v1",
  description="Verified review-specific API and manual RIS sources",
  review_id=refs$review_id,google_sheet_id=refs$google_sheet_id,
  sources=sources),output,auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS: W01 catalogue includes %d API and %d manual RIS sources\\n",
  length(api$sources),length(rows)))
