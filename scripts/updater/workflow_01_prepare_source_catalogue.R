#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))
`%||%` <- function(x,y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

catalogue_path <- arg("--catalogue")
output_manifest <- arg("--output-manifest")
restore_script <- "scripts/updater/workflow_01_restore_manual_ris_from_zenodo.R"
if (is.null(catalogue_path) || is.null(output_manifest)) {
  stop("Required: --catalogue --output-manifest",call.=FALSE)
}
if (!file.exists(catalogue_path)) stop(sprintf("Catalogue not found: %s",catalogue_path),call.=FALSE)
if (!file.exists(restore_script)) stop("Manual RIS restore script not found",call.=FALSE)

x <- fromJSON(catalogue_path,simplifyVector=FALSE)
if (!identical(x$schema,"living-evidence-map-workflow01-source-catalogue-v1")) {
  stop("Unsupported Workflow 01 source catalogue schema",call.=FALSE)
}
# Preserve explicit review routing in downstream W01 manifests, without changing legacy inputs.
review_id <- x$review_id
sheet_id <- x$google_sheet_id
if (!is.null(review_id) || !is.null(sheet_id)) {
  if (is.null(review_id) || is.null(sheet_id) ||
      length(review_id)!=1L || length(sheet_id)!=1L ||
      !nzchar(as.character(review_id)) || !nzchar(as.character(sheet_id))) {
    stop("Review-specific catalogue requires both review_id and google_sheet_id",call.=FALSE)
  }
}
sources <- x$sources
if (is.null(sources) || !length(sources) || is.null(names(sources))) stop("Source catalogue contains no sources",call.=FALSE)

rscript <- file.path(R.home("bin"),"Rscript")
input_paths <- list()
manual_restored <- list()

for (src in names(sources)) {
  z <- sources[[src]]
  kind <- as.character(z$kind %||% "")
  prepared <- as.character(z$prepared_path %||% "")
  if (!nzchar(prepared)) stop(sprintf("Catalogue source %s lacks prepared_path",src),call.=FALSE)

  if (identical(kind,"manual_ris")) {
    registry <- as.character(z$registry %||% "")
    if (!nzchar(registry) || !file.exists(registry)) stop(sprintf("Manual RIS registry missing for %s: %s",src,registry),call.=FALSE)
    dir.create(dirname(prepared),recursive=TRUE,showWarnings=FALSE)
    status <- system2(
      rscript,
      c(restore_script,"--registry",registry,"--source",src,"--output",prepared)
    )
    if (status != 0L) stop(sprintf("Manual RIS restore failed for %s",src),call.=FALSE)
    manual_restored[[src]] <- registry
  } else if (!kind %in% c("workflow00_state","prepared_manual_ris")) {
    stop(sprintf("Unsupported catalogue source kind for %s: %s",src,kind),call.=FALSE)
  }

  if (!file.exists(prepared)) {
    stop(sprintf("Prepared source input missing for %s: %s",src,prepared),call.=FALSE)
  }
  # Verify the prepared file is exactly the one approved by the source catalogue.
  declared_sha <- as.character(z$sha256 %||% "")
  if (nzchar(declared_sha)) {
    if (!grepl("^[0-9a-fA-F]{64}$",declared_sha))
      stop(sprintf("Invalid catalogue SHA256 for %s",src),call.=FALSE)
    actual_sha <- digest::digest(file=prepared,algo="sha256",serialize=FALSE)
    if (!identical(tolower(declared_sha),tolower(actual_sha)))
      stop(sprintf("Prepared source checksum mismatch for %s",src),call.=FALSE)
  }

  expected_records <- suppressWarnings(as.integer(z$expected_records %||% NA_integer_))
  if (identical(kind,"workflow00_state") && !is.na(expected_records)) {
    lines <- readLines(prepared,warn=FALSE,encoding="UTF-8")
    actual_records <- sum(nzchar(trimws(lines)))
    if (identical(src,"openalex")) {
      audit_path <- file.path(dirname(prepared),"compatibility_audit.json")
      duplicate_path <- file.path(dirname(prepared),"duplicate_source_occurrences.jsonl")
      if (!file.exists(audit_path) || !file.exists(duplicate_path)) {
        stop("OpenAlex count reconciliation requires the adapter audit and duplicate-occurrence ledger",call.=FALSE)
      }
      audit <- fromJSON(audit_path,simplifyVector=FALSE)
      input <- audit$input
      raw_n <- as.integer(input$records_read %||% NA_integer_)
      unique_n <- as.integer(input$unique_records %||% NA_integer_)
      repeats_n <- as.integer(input$identical_duplicate_occurrences %||% NA_integer_)
      duplicate_lines <- readLines(duplicate_path,warn=FALSE,encoding="UTF-8")
      duplicate_lines <- duplicate_lines[nzchar(trimws(duplicate_lines))]
      if (anyNA(c(raw_n,unique_n,repeats_n)) ||
          raw_n!=expected_records || unique_n!=actual_records ||
          repeats_n!=length(duplicate_lines) || raw_n!=unique_n+repeats_n) {
        stop(sprintf("OpenAlex count reconciliation failed: W00=%d raw=%d unique=%d prepared=%d duplicates=%d audit_lines=%d",
                     expected_records,raw_n,unique_n,actual_records,repeats_n,length(duplicate_lines)),call.=FALSE)
      }
      cat(sprintf("PASS: OpenAlex source count: %d W00 occurrences = %d unique records + %d identical duplicates\\n",
                  raw_n,unique_n,repeats_n))
    } else if (actual_records != expected_records) {
      stop(sprintf(
        "Prepared source count mismatch for %s: expected %d from W00, found %d in %s",
        src,expected_records,actual_records,prepared
      ),call.=FALSE)
    }
  } else if (identical(kind,"prepared_manual_ris")) {
    lines <- readLines(prepared,warn=FALSE,encoding="UTF-8")
    actual_records <- sum(nzchar(trimws(lines)))
    if (is.na(expected_records) || expected_records < 0L || actual_records != expected_records)
      stop(sprintf("Prepared manual RIS count mismatch for %s: expected %s, found %d",src,as.character(expected_records),actual_records),call.=FALSE)
    parsed <- lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
    ids <- vapply(parsed,function(z)as.character(z$sidecar_identity$sidecar_record_id %||% ""),character(1))
    if(any(!nzchar(ids)) || anyDuplicated(ids)) stop("Missing/duplicated manual RIS manifestation identity for ",src,call.=FALSE)
    manual_restored[[src]] <- as.character(z$submission_id %||% "")
  } else if (file.info(prepared)$size <= 0) {
    stop(sprintf("Prepared source input is empty for %s without an authoritative zero-result count: %s",src,prepared),call.=FALSE)
  }
  # A review-specific source catalogue must supply immutable source checksums
  # wherever its authoritative prepared API adapter records them.
  if (!is.null(review_id) && identical(kind,"workflow00_state") && !nzchar(declared_sha))
    stop(sprintf("Review-specific API source %s has no verified SHA256",src),call.=FALSE)
  input_paths[[src]] <- prepared
}

manifest <- list(
  schema="living-evidence-map-workflow01-source-inputs-v1",
  status="validated",
  sources=input_paths,
  review_id=review_id,
  google_sheet_id=sheet_id,
  manual_ris_registries=manual_restored,
  generated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
dir.create(dirname(output_manifest),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),output_manifest)

cat(sprintf(
  "PASS: prepared Workflow 01 source-input manifest with %d sources (%d manual RIS)\n",
  length(input_paths),length(manual_restored)
))
