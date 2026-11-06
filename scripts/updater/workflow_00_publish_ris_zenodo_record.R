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
output_dir <- arg("--output-dir", "outputs/updater/workflow00_manual_ris_publish")
if (is.null(registry_path) || !file.exists(registry_path)) {
  stop("Required: --registry <registry.json>", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set", call. = FALSE)

scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
or_else <- function(x, y) if (is.null(x)) y else x
auth <- function(req) req |> req_headers(Authorization = paste("Bearer", token))

perform <- function(req, expected, label, timeout = 300) {
  resp <- req |>
    req_timeout(timeout) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (!(status %in% expected)) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "")
    stop(sprintf("Zenodo %s returned HTTP %d: %s", label, status, body), call. = FALSE)
  }
  resp
}

local_registry <- fromJSON(registry_path, simplifyVector = FALSE)
reserved_doi <- scalar(local_registry$staging$reserved_doi)
dep_id <- scalar(local_registry$staging$deposition_id)
if (is.null(dep_id) && !is.null(reserved_doi)) dep_id <- sub("^.*\\.", "", reserved_doi)
if (is.null(dep_id)) stop("Could not determine Zenodo draft/deposition ID", call. = FALSE)

legacy_url <- paste0("https://zenodo.org/api/deposit/depositions/", dep_id)
legacy <- resp_body_json(
  perform(request(legacy_url) |> auth(), 200L, "legacy draft preflight", 60),
  simplifyVector = FALSE
)
if (isTRUE(legacy$submitted)) stop("Zenodo record is already submitted/published", call. = FALSE)

candidate_dois <- unique(na.omit(c(
  scalar(legacy$doi),
  scalar(legacy$metadata$doi),
  scalar(legacy$metadata$prereserve_doi$doi),
  scalar(legacy$prereserve_doi$doi)
)))
if (!is.null(reserved_doi) && !(reserved_doi %in% candidate_dois)) {
  stop(sprintf("Reserved DOI mismatch for draft %s", dep_id), call. = FALSE)
}

legacy_access <- scalar(legacy$metadata$access_right)
if (!identical(legacy_access, "restricted")) {
  stop(sprintf("Publication blocked: Zenodo access_right is %s, not restricted", or_else(legacy_access, "<missing>")), call. = FALSE)
}

rdm_url <- paste0("https://zenodo.org/api/records/", dep_id, "/draft")
rdm <- resp_body_json(
  perform(request(rdm_url) |> auth(), 200L, "RDM draft preflight", 60),
  simplifyVector = FALSE
)
files_url <- scalar(rdm$links$files)
if (is.null(files_url)) stop("RDM draft is missing files link", call. = FALSE)

file_body <- resp_body_json(
  perform(request(files_url) |> auth(), 200L, "draft file listing", 60),
  simplifyVector = FALSE
)
entries <- if (!is.null(file_body$entries)) file_body$entries else list()
key_of <- function(x) scalar(or_else(x$key, or_else(x$filename, x$name)))
status_of <- function(x) scalar(x$status)
keys <- vapply(entries, function(x) or_else(key_of(x), ""), character(1))
statuses <- vapply(entries, function(x) or_else(status_of(x), ""), character(1))

if (!("source_registry.json" %in% keys)) {
  stop("Publication blocked: source_registry.json is not present in the draft", call. = FALSE)
}
registry_url <- sprintf(
  "https://zenodo.org/api/records/%s/draft/files/%s/content",
  dep_id,
  URLencode("source_registry.json", reserved = TRUE)
)
archived_registry <- fromJSON(
  rawToChar(resp_body_raw(perform(request(registry_url) |> auth(), 200L, "archived source_registry.json", 120))),
  simplifyVector = FALSE
)

archived_source <- scalar(archived_registry$database$short_name)
local_source <- scalar(local_registry$database$short_name)
if (!is.null(local_source) && !identical(local_source, archived_source)) {
  stop(sprintf("Publication blocked: local source=%s but archived source=%s", local_source, archived_source), call. = FALSE)
}

raw_files <- archived_registry$input$files
if (is.null(raw_files) || !length(raw_files)) {
  stop("Publication blocked: archived source_registry.json has no raw input files", call. = FALSE)
}
raw_names <- vapply(raw_files, function(x) or_else(scalar(x$filename), ""), character(1))
if (any(!nzchar(raw_names))) stop("Publication blocked: archived raw file entry lacks filename", call. = FALSE)
if (anyDuplicated(raw_names)) stop("Publication blocked: duplicated raw filenames in archived registry", call. = FALSE)

for (x in raw_files) {
  nm <- scalar(x$filename)
  expected_bytes <- x$bytes
  expected_sha <- scalar(x$sha256)
  if (is.null(expected_bytes) || is.null(expected_sha)) {
    stop(sprintf("Publication blocked: archived registry lacks bytes/SHA-256 for %s", nm), call. = FALSE)
  }
  url <- sprintf(
    "https://zenodo.org/api/records/%s/draft/files/%s/content",
    dep_id,
    URLencode(nm, reserved = TRUE)
  )
  resp <- perform(request(url) |> auth(), 200L, paste0("raw file verification: ", nm), 1800)
  tmp <- tempfile(fileext = ".ris")
  writeBin(resp_body_raw(resp), tmp)
  got_bytes <- unname(file.info(tmp)$size)
  got_sha <- digest(file = tmp, algo = "sha256", serialize = FALSE)
  unlink(tmp)
  if (as.integer(got_bytes) != as.integer(expected_bytes) || !identical(got_sha, expected_sha)) {
    stop(sprintf("Publication blocked: raw file fingerprint mismatch for %s", nm), call. = FALSE)
  }
  cat(sprintf("RAW VERIFY %s bytes=%s sha256=%s PASS\n", nm, got_bytes, got_sha))
}

derived_names <- c(
  "source_registry.json",
  "records.jsonl",
  "manifest.json",
  "SHA256SUMS",
  "chunk_file_checksums.csv",
  "exact_duplicate_source_record_ids.csv"
)
expected_files <- c(raw_names, derived_names)

if (!setequal(keys, expected_files)) {
  stop(sprintf(
    "Publication blocked: draft file set mismatch. Missing=[%s] Unexpected=[%s]",
    paste(setdiff(expected_files, keys), collapse = ", "),
    paste(setdiff(keys, expected_files), collapse = ", ")
  ), call. = FALSE)
}
if (any(statuses != "completed")) {
  bad <- keys[statuses != "completed"]
  stop(sprintf("Publication blocked: non-completed file entries: %s", paste(bad, collapse = ", ")), call. = FALSE)
}

publish_url <- scalar(rdm$links$publish)
if (is.null(publish_url)) publish_url <- scalar(legacy$links$publish)
if (is.null(publish_url)) stop("Publication blocked: no Zenodo publish action link found", call. = FALSE)

cat(sprintf(
  "PUBLICATION PREFLIGHT PASS draft=%s source=%s raw_files=%d total_files=%d access=restricted\n",
  dep_id, archived_source, length(raw_names), length(expected_files)
))

pub <- perform(
  request(publish_url) |> req_method("POST") |> auth(),
  c(200L, 201L, 202L),
  "publish restricted draft",
  180
)

Sys.sleep(3)
published_url <- paste0("https://zenodo.org/api/records/", dep_id)
published <- resp_body_json(
  perform(request(published_url) |> auth(), 200L, "published record verification", 120),
  simplifyVector = FALSE
)
pub_doi <- or_else(scalar(published$doi), scalar(published$pids$doi$identifier))
if (!is.null(reserved_doi) && !is.null(pub_doi) && !identical(pub_doi, reserved_doi)) {
  stop(sprintf("Published DOI mismatch: expected %s, got %s", reserved_doi, pub_doi), call. = FALSE)
}

receipt <- list(
  status = "published_restricted",
  zenodo_deposition_id = dep_id,
  doi = pub_doi,
  source = archived_source,
  raw_files_verified = raw_names,
  total_files = length(expected_files),
  access_right = legacy_access,
  publish_http_status = resp_status(pub),
  published_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
)
writeLines(
  toJSON(receipt, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  file.path(output_dir, "publication_receipt.json")
)

cat(sprintf("PASS: published restricted Workflow 00 manual RIS record %s\n", or_else(pub_doi, dep_id)))
