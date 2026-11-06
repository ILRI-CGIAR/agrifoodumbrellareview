#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

registry_path <- arg("--registry")
output_dir <- arg("--output-dir", "outputs/updater/workflow00_ris_integrity")
if (is.null(registry_path) || !file.exists(registry_path)) {
  stop("Required: --registry <registry.json>", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set", call. = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}

registry <- fromJSON(registry_path, simplifyVector = FALSE)
doi <- scalar(registry$staging$reserved_doi)
dep_id <- scalar(registry$staging$deposition_id)
if (is.null(dep_id) && !is.null(doi)) dep_id <- sub("^.*\\.", "", doi)
if (is.null(dep_id)) stop("Could not determine Zenodo draft/deposition ID", call. = FALSE)

files <- registry$input$files
if (is.null(files) || !length(files)) stop("Registry contains no input.files to verify", call. = FALSE)

expected <- lapply(files, function(x) list(
  filename = scalar(x$filename),
  bytes = if (is.null(x$bytes)) NULL else as.integer(x$bytes),
  sha256 = scalar(x$sha256)
))
if (any(vapply(expected, function(x) is.null(x$filename), logical(1)))) {
  stop("All registry input.files entries require filename", call. = FALSE)
}
if (any(vapply(expected, function(x) is.null(x$bytes) || is.null(x$sha256), logical(1)))) {
  stop("Integrity verification requires bytes and sha256 for every registered file", call. = FALSE)
}

auth <- function(req) req |> req_headers(Authorization = paste("Bearer", token))

# Inspect the modern draft file-entry list without modifying the draft.
files_url <- sprintf("https://zenodo.org/api/records/%s/draft/files", dep_id)
files_resp <- request(files_url) |>
  auth() |>
  req_timeout(60) |>
  req_error(is_error = function(resp) FALSE) |>
  req_perform()
files_status <- resp_status(files_resp)
cat(sprintf("DRAFT FILE LIST HTTP=%d\n", files_status))
draft_entries <- list()
if (files_status == 200L) {
  files_body <- resp_body_json(files_resp, simplifyVector = FALSE)
  draft_entries <- if (!is.null(files_body$entries)) files_body$entries else if (is.list(files_body) && is.null(names(files_body))) files_body else list()
  for (entry in draft_entries) {
    key <- scalar(entry$key) %||% scalar(entry$filename) %||% scalar(entry$name)
    status <- scalar(entry$status) %||% "unknown"
    size <- entry$size %||% entry$filesize %||% NULL
    checksum <- scalar(entry$checksum)
    cat(sprintf("DRAFT FILE key=%s status=%s size=%s checksum=%s\n",
                key %||% "<none>", status, as.character(size %||% NA), checksum %||% "<none>"))
  }
} else {
  body <- tryCatch(resp_body_string(files_resp), error = function(e) "")
  cat(sprintf("DRAFT FILE LIST ERROR %s\n", body))
}

perform <- function(req, timeout = 1800) {
  req |>
    req_timeout(timeout) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
}

results <- list()
download_dir <- file.path(output_dir, "downloads")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

for (x in expected) {
  encoded <- URLencode(x$filename, reserved = TRUE)
  url <- sprintf("https://zenodo.org/api/records/%s/draft/files/%s/content", dep_id, encoded)
  cat(sprintf("VERIFY DOWNLOAD %s\n", x$filename))
  resp <- perform(request(url) |> auth())
  status <- resp_status(resp)

  if (status != 200L) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "")
    results[[length(results) + 1L]] <- list(
      filename = x$filename,
      expected_bytes = x$bytes,
      actual_bytes = NULL,
      expected_sha256 = x$sha256,
      actual_sha256 = NULL,
      http_status = status,
      error = body,
      match = FALSE
    )
    cat(sprintf("VERIFY %s HTTP=%d match=FALSE\n", x$filename, status))
    next
  }

  out <- file.path(download_dir, x$filename)
  writeBin(resp_body_raw(resp), out)
  got_bytes <- unname(file.info(out)$size)
  got_sha <- digest(file = out, algo = "sha256", serialize = FALSE)
  ok <- identical(as.integer(got_bytes), as.integer(x$bytes)) && identical(got_sha, x$sha256)
  results[[length(results) + 1L]] <- list(
    filename = x$filename,
    expected_bytes = x$bytes,
    actual_bytes = got_bytes,
    expected_sha256 = x$sha256,
    actual_sha256 = got_sha,
    http_status = status,
    error = NULL,
    match = ok
  )
  cat(sprintf("VERIFY %s bytes=%s sha256=%s match=%s\n", x$filename, got_bytes, got_sha, ok))
}

all_ok <- all(vapply(results, function(x) isTRUE(x$match), logical(1)))
receipt <- list(
  status = if (all_ok) "pass" else "fail",
  zenodo_deposition_id = dep_id,
  draft_file_list_http_status = files_status,
  draft_file_entries = lapply(draft_entries, function(entry) list(
    key = scalar(entry$key) %||% scalar(entry$filename) %||% scalar(entry$name),
    status = scalar(entry$status),
    size = entry$size %||% entry$filesize %||% NULL,
    checksum = scalar(entry$checksum)
  )),
  files = results,
  zenodo_write_performed = FALSE,
  verified_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
)
writeLines(
  toJSON(receipt, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  file.path(output_dir, "integrity_receipt.json")
)

if (!all_ok) stop("One or more Zenodo RIS files failed size/SHA-256 verification", call. = FALSE)
cat(sprintf("PASS: all %d Zenodo RIS files match registered byte sizes and SHA-256 checksums\n", length(results)))
