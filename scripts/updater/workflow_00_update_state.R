#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
prior_path <- arg("--prior-state")
receipt_path <- arg("--receipt")
output_path <- arg("--output")
registry_root <- arg("--registry-root","data/workflow00/source_id_registry")
if (is.null(prior_path)||is.null(receipt_path)||is.null(output_path)) stop("Required: --prior-state --receipt --output",call.=FALSE)
if (!file.exists(prior_path)||!file.exists(receipt_path)) stop("Prior state or receipt not found",call.=FALSE)

prior <- fromJSON(prior_path,simplifyVector=FALSE)
receipt <- fromJSON(receipt_path,simplifyVector=FALSE)
if (!identical(prior$schema,"living-evidence-map-workflow00-state-v1") || !identical(prior$status,"accepted")) stop("Prior state is not accepted v1 state",call.=FALSE)
if (!identical(receipt$status,"published") || !identical(receipt$visibility,"restricted")) stop("Receipt is not a published restricted W00 archive",call.=FALSE)
if (!identical(receipt$run_type,"full")) stop("Automatic rolling-state promotion is disabled for fortnightly/expansion deltas until source-native ID reconciliation is consistent for all selected sources",call.=FALSE)

sources <- trimws(as.character(unlist(receipt$sources,use.names=FALSE)))
sources <- sources[nzchar(sources)]
if (!length(sources)) stop("Receipt contains no sources",call.=FALSE)
if (any(!grepl("^[a-z0-9][a-z0-9_-]*$",sources))) {
  stop("Receipt contains an invalid source identifier",call.=FALSE)
}
if (anyDuplicated(sources)) {
  stop("Receipt contains duplicate source identifiers",call.=FALSE)
}
if (is.null(prior$sources) || !is.list(prior$sources)) {
  stop("Prior state lacks a valid sources object",call.=FALSE)
}

archive_files <- receipt$archive_files
if (is.data.frame(archive_files)) archive_files <- lapply(seq_len(nrow(archive_files)),function(i)as.list(archive_files[i,,drop=FALSE]))
state <- prior
state$state_id <- sprintf("state-after-w00-run-%s",receipt$github_run_id)
state$state_type <- "rolling_composite"
state$search_version <- receipt$search_version
state$created_from_existing_archives <- FALSE
state$updated_at_utc <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")

pointer <- sprintf("docs/search_record/zenodo/run-%s.json",receipt$github_run_id)
for (src in sources) {
  filename <- sprintf("agrifoodumbrellareview_workflow00_run-%s_%s-harvest.tar.gz",receipt$github_run_id,src)
  match <- Filter(function(a)identical(as.character(a$filename),filename),archive_files)
  if (length(match)!=1L) stop(sprintf("Receipt does not contain one harvest archive for %s",src),call.=FALSE)
  a <- match[[1L]]
  state$sources[[src]] <- list(
    archive_pointer=pointer,
    github_run_id=as.character(receipt$github_run_id),
    zenodo_record_id=as.character(receipt$zenodo_record_id),
    doi=as.character(receipt$doi),
    harvest_file=list(filename=filename,bytes=as.numeric(a$bytes),sha256=as.character(a$sha256)),
    native_id_registry=file.path(registry_root,sprintf("%s_native_ids.txt",src))
  )
}
dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(state,auto_unbox=TRUE,pretty=TRUE,null="null"),output_path)
cat(sprintf("PASS: wrote rolling Workflow 00 state %s updating sources %s\n",state$state_id,paste(sources,collapse=", ")))
