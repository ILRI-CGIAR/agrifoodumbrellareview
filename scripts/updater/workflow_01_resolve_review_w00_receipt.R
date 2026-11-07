#!/usr/bin/env Rscript
# Resolve a review's published W00 handoff receipt; read-only against Google Sheets.
suppressPackageStartupMessages({library(jsonlite);library(googlesheets4)})
args<-commandArgs(trailingOnly=TRUE)
get<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
sheet<-get("--sheet-id");credential<-get("--credential")
review<-get("--review-id");output<-get("--output")
if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{3,127}$",review))stop("Invalid review ID",call.=FALSE)
if(!file.exists(credential))stop("Missing service account",call.=FALSE)
gs4_auth(path=credential,cache=FALSE)
m<-as.data.frame(read_sheet(sheet,sheet="review_metadata",col_types="cc"))
e<-as.data.frame(read_sheet(sheet,sheet="workflow_events",col_types="cccccccc"))
if(!identical(names(m),c("key","value"))||
 !identical(names(e),c("event_id","timestamp_utc","review_id","workflow","event_type","github_run_id","zenodo_record_id","details_json")))
 stop("Unexpected review Sheets schema",call.=FALSE)
if(anyDuplicated(m$key))stop("Duplicate metadata keys",call.=FALSE)
v<-function(k){i<-match(k,m$key);if(is.na(i))stop("Missing ",k,call.=FALSE);as.character(m$value[[i]])}
if(!identical(v("review_id"),review)||!identical(v("latest_w00_status"),"success"))stop("Review mismatch or W00 not successful",call.=FALSE)
run<-v("latest_w00_run_id");record<-v("latest_w00_zenodo_record_id")
if(!grepl("^[0-9]+$",run)||!grepl("^[0-9]+$",record))stop("Invalid W00 run or record",call.=FALSE)
id<-paste0("W00:",run,":zenodo_published")
rows<-which(e$event_id==id & e$review_id==review & e$workflow=="W00" & e$event_type=="zenodo_published")
if(length(rows)!=1L)stop("Missing or ambiguous published W00 event",call.=FALSE)
entry<-e[rows,,drop=FALSE]
if(!identical(as.character(entry$github_run_id[[1L]]),run)||
   !identical(as.character(entry$zenodo_record_id[[1L]]),record))stop("Event/metadata disagreement",call.=FALSE)
details<-fromJSON(as.character(entry$details_json[[1L]]),simplifyVector=FALSE)
receipt<-details$receipt
if(is.null(receipt))stop("W00 event lacks complete receipt; cannot restore safely",call.=FALSE)
if(!identical(receipt$status,"published")||!identical(receipt$visibility,"restricted")||
  !identical(as.character(receipt$review_id),review)||
  !identical(as.character(receipt$github_run_id),run)||
  !identical(as.character(receipt$zenodo_record_id),record))
 stop("Review-specific W00 receipt mismatch",call.=FALSE)
if(is.null(receipt$zenodo_deposition_id)||!grepl("^[0-9]+$",as.character(receipt$zenodo_deposition_id))||
 is.null(receipt$archive_files)||!length(receipt$archive_files)||is.null(receipt$sources)||!length(receipt$sources))
 stop("W00 receipt missing archive restore fields",call.=FALSE)
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
write_json(receipt,output,auto_unbox=TRUE,pretty=TRUE,null="null")
cat("PASS: wrote validated review-specific W00 pointer to ",output,"\n",sep="")

# Optional input manifest: a frozen list of references, not research records.
manifest_flag <- match("--input-manifest",args)
if(!is.na(manifest_flag)){
  if(manifest_flag==length(args))stop("Missing --input-manifest value",call.=FALSE)
  destination<-args[[manifest_flag+1L]]
  manual<-list()
  if("manual_search_registry" %in% sheet_names(sheet)){
    tab<-as.data.frame(read_sheet(sheet,sheet="manual_search_registry",col_types="c"))
    required<-c("review_key","submission_id","zenodo_record_id","zenodo_receipt_json",
      "file_count","records_downloaded")
    if(!all(required %in% names(tab)))stop("Invalid manual RIS registry schema",call.=FALSE)
    # Shiny's review_key may hash a Sheet-derived configuration ID rather
    # than the canonical review_metadata ID. Select by independently
    # archived receipt review_id, then verify every selected record below.
    belongs<-vapply(seq_len(nrow(tab)),function(i) {
      raw<-as.character(tab$zenodo_receipt_json[[i]])
      if(is.na(raw)||!nzchar(raw)) return(FALSE)
      z<-tryCatch(fromJSON(raw,simplifyVector=FALSE),error=function(e)NULL)
      !is.null(z) && identical(as.character(z$review_id),review)
    },logical(1))
    candidates<-tab[belongs,,drop=FALSE]
    if(nrow(candidates)){
      if(anyDuplicated(candidates$submission_id))stop("Duplicate manual RIS submission ID",call.=FALSE)
      manual<-lapply(seq_len(nrow(candidates)),function(i){
        z<-candidates[i,,drop=FALSE]
        record_id<-as.character(z$zenodo_record_id[[1L]])
        if(is.na(record_id)||!grepl("^[0-9]+$",record_id))
          stop("Invalid manual RIS Zenodo record ID",call.=FALSE)
        receipt_text<-as.character(z$zenodo_receipt_json[[1L]])
        if(is.na(receipt_text)||!nzchar(receipt_text))
          stop("Manual RIS submission is missing its Zenodo receipt",call.=FALSE)
        receipt_data<-fromJSON(receipt_text,simplifyVector=FALSE)
        submission<-as.character(z$submission_id[[1L]])
        if(!identical(as.character(receipt_data$status), "published") ||
           !identical(as.character(receipt_data$review_id), review) ||
           !identical(as.character(receipt_data$submission_id), submission) ||
           !identical(as.character(receipt_data$zenodo_record_id), record_id))
          stop("Manual RIS receipt does not match registered review/submission/record",call.=FALSE)
        files<-receipt_data$files
        nfiles<-suppressWarnings(as.integer(z$file_count[[1L]]))
        nrecords<-suppressWarnings(as.integer(z$records_downloaded[[1L]]))
        if(is.na(nfiles)||nfiles<1L||is.na(nrecords)||nrecords<0L||
           length(files)!=nfiles)
          stop("Manual RIS file or record counts do not match receipt",call.=FALSE)
        names_in_archive<-vapply(files,function(x)as.character(x$archive_name),character(1))
        hashes<-vapply(files,function(x)as.character(x$sha256),character(1))
        if(any(!grepl("^[0-9]{3}_[A-Za-z0-9._-]+$",names_in_archive)) ||
           anyDuplicated(names_in_archive) ||
           any(!grepl("^[0-9a-fA-F]{64}$",hashes)))
          stop("Manual RIS receipt contains invalid files or checksums",call.=FALSE)
        list(submission_id=submission,
          zenodo_record_id=record_id,
          receipt=receipt_data,
          file_count=as.integer(z$file_count[[1L]]),
          records_downloaded=as.integer(z$records_downloaded[[1L]]))
      })
    }
  }
  manifest<-list(schema="living-evidence-map-workflow01-review-input-references-v1",
    review_id=review,google_sheet_id=sheet,
    w00=list(github_run_id=run,zenodo_record_id=record,receipt=receipt),
    manual_ris=manual,
    generated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
  dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
  write_json(manifest,destination,auto_unbox=TRUE,pretty=TRUE,null="null")
  cat("PASS: W01 input manifest includes ",length(manual)," manual RIS submission(s)\n",sep="")
}
