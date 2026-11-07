#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(paste("Missing",flag), call. = FALSE)
  args[[i + 1L]]
}
root <- get_arg("--artifact-root", "w00_artifacts")
out <- get_arg("--output-root", "current_sources")
manifest_out <- get_arg("--manifest", "outputs/w01_manual_handoff.json")
sources <- c(proquest = 528L, cab = 4951L, lilacs = 87L)
paths <- list()
counts <- list()
for (src in names(sources)) {
  dir <- file.path(root, paste0("w00-manual-", src))
  possible <- c(file.path(dir, "ingestion", "handoff", "records.jsonl"),
                file.path(dir, "handoff", "records.jsonl"))
  existing <- possible[file.exists(possible)]
  if (length(existing) != 1L) stop(sprintf("W00 artifact handoff missing or ambiguous for %s",src),call.=FALSE)
  receipt_path <- file.path(dir, "validation_only_receipt.json")
  if (!file.exists(receipt_path)) stop(sprintf("W00 validation receipt missing for %s",src),call.=FALSE)
  receipt <- fromJSON(receipt_path, simplifyVector=FALSE)
  if (!identical(receipt$status,"validated_only_no_zenodo_write") ||
      !identical(receipt$source,src) ||
      !identical(receipt$zenodo_write_performed,FALSE)) {
    stop(sprintf("Invalid W00 validation receipt for %s",src),call.=FALSE)
  }
  if (as.integer(receipt$unique_records_for_handover) != sources[[src]]) {
    stop(sprintf("Source %s count differs from verified W00 job result",src),call.=FALSE)
  }
  lines <- readLines(existing[[1L]],warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) != sources[[src]]) stop(sprintf("W00 %s file count mismatch",src),call.=FALSE)
  ids <- character(length(lines))
  for (i in seq_along(lines)) {
    x <- fromJSON(lines[[i]],simplifyVector=FALSE)
    if (!identical(x$source$provider,src)) stop(sprintf("Source mismatch %s line %d",src,i),call.=FALSE)
    id <- x$sidecar_identity$sidecar_record_id
    if (is.null(id) || !nzchar(id)) stop(sprintf("Missing source ID %s line %d",src,i),call.=FALSE)
    ids[[i]] <- id
  }
  if (anyDuplicated(ids)) stop(sprintf("Repeated source identifiers for %s",src),call.=FALSE)
  dest <- file.path(out,src,"records.jsonl")
  dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
  if (!file.copy(existing[[1L]],dest,overwrite=TRUE)) stop(sprintf("Copy failed for %s",src),call.=FALSE)
  paths[[src]] <- dest
  counts[[src]] <- length(lines)
  cat(sprintf("PASS W01 manual handoff %s: %d records\n",src,length(lines)))
}
result <- list(schema="living-evidence-map-workflow01-source-inputs-v1",
  status="validated_manual_only_not_full_w01",
  sources=paths,
  source_counts=counts,
  note="This manifest is a manual-source subset. Do not use it as the full W01 corpus; all 2015+ API-source inputs must first be restored and verified.")
dir.create(dirname(manifest_out),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(result,auto_unbox=TRUE,pretty=TRUE,null="null"),manifest_out)
cat("PASS: all three manual W00 artifacts prepared for W01; full W01 remains blocked on baseline inputs\n")
