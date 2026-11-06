#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

state_path <- arg("--state")
if (is.null(state_path) || !file.exists(state_path)) stop("Required existing --state file",call.=FALSE)

x <- fromJSON(state_path,simplifyVector=FALSE)
if (!identical(x$schema,"living-evidence-map-workflow00-state-v1")) stop("Unsupported Workflow 00 state schema",call.=FALSE)
if (!identical(x$status,"accepted")) stop("Workflow 00 state is not accepted",call.=FALSE)
if (is.null(x$state_id) || !nzchar(as.character(x$state_id))) stop("Workflow 00 state lacks state_id",call.=FALSE)
if (is.null(x$search_version) || !nzchar(as.character(x$search_version))) stop("Workflow 00 state lacks search_version",call.=FALSE)
if (is.null(x$sources) || !is.list(x$sources)) stop("Workflow 00 state lacks sources",call.=FALSE)

actual <- names(x$sources)
if (is.null(actual) || !length(actual)) {
  stop("Workflow 00 state must contain at least one source",call.=FALSE)
}
if (any(!nzchar(actual)) || any(!grepl("^[a-z0-9][a-z0-9_-]*$",actual))) {
  stop("Workflow 00 state contains an invalid source identifier",call.=FALSE)
}
if (anyDuplicated(actual)) {
  stop("Workflow 00 state contains duplicate source identifiers",call.=FALSE)
}

hex64 <- "^[0-9a-fA-F]{64}$"
for (src in actual) {
  z <- x$sources[[src]]
  required <- c("archive_pointer","github_run_id","zenodo_record_id","doi","harvest_file","native_id_registry")
  missing <- required[vapply(required,function(nm)is.null(z[[nm]]),logical(1))]
  if (length(missing)) stop(sprintf("%s state missing: %s",src,paste(missing,collapse=", ")),call.=FALSE)

  pointer <- as.character(z$archive_pointer)
  registry <- as.character(z$native_id_registry)
  if (!file.exists(pointer)) stop(sprintf("%s archive pointer not found: %s",src,pointer),call.=FALSE)
  if (!file.exists(registry) || file.info(registry)$size <= 0) stop(sprintf("%s native-ID registry missing/empty: %s",src,registry),call.=FALSE)

  ids <- trimws(readLines(registry,warn=FALSE,encoding="UTF-8"))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) stop(sprintf("%s native-ID registry contains no IDs",src),call.=FALSE)
  if (anyDuplicated(ids)) stop(sprintf("%s native-ID registry contains duplicate IDs",src),call.=FALSE)

  p <- fromJSON(pointer,simplifyVector=FALSE)
  if (!identical(p$status,"published") || !identical(p$visibility,"restricted")) {
    stop(sprintf("%s archive pointer is not a published restricted archive",src),call.=FALSE)
  }
  ps <- trimws(as.character(unlist(p$sources,use.names=FALSE)))
  if (!(src %in% ps)) stop(sprintf("%s is not listed in archive pointer %s",src,pointer),call.=FALSE)
  if (!identical(as.character(z$github_run_id),as.character(p$github_run_id))) stop(sprintf("%s github_run_id mismatch",src),call.=FALSE)
  if (!identical(as.character(z$zenodo_record_id),as.character(p$zenodo_record_id))) stop(sprintf("%s zenodo_record_id mismatch",src),call.=FALSE)

  hf <- z$harvest_file
  if (is.null(hf$filename) || is.null(hf$bytes) || is.null(hf$sha256)) stop(sprintf("%s harvest_file metadata incomplete",src),call.=FALSE)
  if (!grepl(hex64,as.character(hf$sha256))) stop(sprintf("%s harvest SHA-256 is invalid",src),call.=FALSE)
  if (as.numeric(hf$bytes) <= 0) stop(sprintf("%s harvest byte size is invalid",src),call.=FALSE)

  af <- p$archive_files
  if (is.data.frame(af)) af <- lapply(seq_len(nrow(af)),function(i)as.list(af[i,,drop=FALSE]))
  matches <- Filter(function(a) identical(as.character(a$filename),as.character(hf$filename)),af)
  if (length(matches)!=1L) stop(sprintf("%s harvest file not uniquely present in archive pointer",src),call.=FALSE)
  a <- matches[[1L]]
  if (!identical(as.numeric(a$bytes),as.numeric(hf$bytes))) stop(sprintf("%s harvest byte-size mismatch",src),call.=FALSE)
  if (!identical(tolower(as.character(a$sha256)),tolower(as.character(hf$sha256)))) stop(sprintf("%s harvest SHA-256 mismatch",src),call.=FALSE)
}
cat(sprintf("PASS: Workflow 00 state %s validates with %d source(s): %s\n",x$state_id,length(actual),paste(actual,collapse=", ")))
