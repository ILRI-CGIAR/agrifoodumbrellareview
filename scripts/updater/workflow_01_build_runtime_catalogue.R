#!/usr/bin/env Rscript
# Construct a topic-independent W01 source catalogue from the W00 source list.
# Optional project-specific manual RIS registries are supplied at runtime,
# not embedded in reusable pipeline code.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
get_arg <- function(flag, default=NULL) {
  idx <- which(args == flag)
  if (!length(idx)) return(default)
  if (length(idx)!=1L || idx==length(args)) stop(paste("Invalid argument",flag),call.=FALSE)
  args[[idx+1L]]
}
sources_arg <- get_arg("--w00-sources")
output <- get_arg("--output")
manual_path <- get_arg("--manual-config")
status_path <- get_arg("--w00-status")
prepared_root <- get_arg("--prepared-root","current_sources")
if (is.null(sources_arg) || is.null(output)) {
  stop("Required: --w00-sources CSV --output PATH [--manual-config PATH]",call.=FALSE)
}
src <- trimws(strsplit(sources_arg,",",fixed=TRUE)[[1L]])
src <- unique(src[nzchar(src)])
if (!length(src) || any(!grepl("^[a-z0-9][a-z0-9_-]*$",src))) {
  stop("W00 sources must be non-empty valid source IDs",call.=FALSE)
}
counts <- list()
if (!is.null(status_path)) {
  if (!file.exists(status_path)) stop("W00 status missing",call.=FALSE)
  counts <- fromJSON(status_path,simplifyVector=FALSE)$search$sources
}
find_prepared <- function(s) {
  root <- file.path(prepared_root,s)
  options <- c(file.path(root,"records.jsonl"),
    file.path(root,paste0(s,"_sidecar_records.jsonl")),
    file.path(root,paste0(s,"_starter_sidecar_records.jsonl")))
  present <- options[file.exists(options)]
  if (length(present)!=1L) stop(sprintf("Expected one prepared W00 input for %s, found %d",s,length(present)),call.=FALSE)
  present[[1L]]
}
make_source <- function(s) {
  z <- list(kind="workflow00_state",source=s,prepared_path=find_prepared(s))
  if (!is.null(status_path)) {
    n <- counts[[s]]$successfully_downloaded_results
    if (is.null(n) || length(n)!=1L || is.na(suppressWarnings(as.integer(n)))) stop(paste("Missing W00 count",s),call.=FALSE)
    z$expected_records <- as.integer(n)
  }
  z
}
catalogue <- list(
  schema="living-evidence-map-workflow01-source-catalogue-v1",
  description="Runtime W01 catalogue derived from the selected W00 sources",
  sources=setNames(lapply(src,make_source),src)
)
if (!is.null(manual_path)) {
  if (!file.exists(manual_path)) stop("Manual source config not found",call.=FALSE)
  manual <- fromJSON(manual_path,simplifyVector=FALSE)
  if (!identical(manual$schema,"living-evidence-map-workflow01-manual-sources-v1") ||
      is.null(manual$sources) || is.null(names(manual$sources))) {
    stop("Invalid manual source configuration",call.=FALSE)
  }
  for (s in names(manual$sources)) {
    if (!grepl("^[a-z0-9][a-z0-9_-]*$",s) || s %in% src) {
      stop(paste("Invalid or duplicate manual source",s),call.=FALSE)
    }
    registry <- manual$sources[[s]]$registry
    if (is.null(registry) || length(registry)!=1L || !nzchar(registry)) {
      stop(paste("Missing manual RIS registry for",s),call.=FALSE)
    }
    catalogue$sources[[s]] <- list(
      kind="manual_ris",registry=registry,
      prepared_path=file.path(prepared_root,s,"records.jsonl")
    )
  }
}
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(catalogue,auto_unbox=TRUE,pretty=TRUE,null="null"),output)
cat(sprintf("Generated W01 runtime catalogue for %d source(s)\n",length(catalogue$sources)))
