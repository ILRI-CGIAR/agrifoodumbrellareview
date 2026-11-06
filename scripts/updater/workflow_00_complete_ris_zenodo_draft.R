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
output_dir <- arg("--output-dir", "outputs/updater/workflow00_manual_ris_zenodo")
validate_only <- tolower(arg("--validate-only", "false")) %in% c("true","1","yes")
if (is.null(registry_path) || !file.exists(registry_path)) {
  stop("Required: --registry <registry.json>", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set", call. = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}

registry <- fromJSON(registry_path, simplifyVector = FALSE)
if (!identical(scalar(registry$staging$method), "zenodo_draft")) {
  stop("Registry staging.method must be zenodo_draft", call. = FALSE)
}
reserved_doi <- scalar(registry$staging$reserved_doi)
dep_id <- scalar(registry$staging$deposition_id)
if (is.null(reserved_doi) && is.null(dep_id)) {
  stop("Registry must contain staging.reserved_doi or staging.deposition_id", call. = FALSE)
}
if (isTRUE(registry$staging$publish_after_validation)) {
  stop("Manual RIS draft completion currently requires publish_after_validation=false", call. = FALSE)
}

legacy_api <- "https://zenodo.org/api/deposit/depositions"
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

perform_maybe <- function(req, timeout = 60) {
  req |>
    req_timeout(timeout) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
}

get_deposition <- function(id) {
  resp <- perform(
    request(paste0(legacy_api, "/", id)) |> auth(),
    200L,
    paste0("deposition lookup ", id),
    60
  )
  resp_body_json(resp, simplifyVector = FALSE)
}

candidate_dois <- function(dep) {
  unique(na.omit(c(
    scalar(dep$doi),
    scalar(dep$metadata$doi),
    scalar(dep$metadata$prereserve_doi$doi),
    scalar(dep$prereserve_doi$doi)
  )))
}

find_draft_by_doi <- function(doi) {
  matches <- list()
  page <- 1L
  repeat {
    url <- paste0(legacy_api, "?status=draft&size=100&page=", page)
    resp <- perform(request(url) |> auth(), 200L, "draft listing", 60)
    items <- resp_body_json(resp, simplifyVector = FALSE)
    if (!length(items)) break
    for (dep in items) {
      if (doi %in% candidate_dois(dep)) matches[[length(matches) + 1L]] <- dep
    }
    if (length(items) < 100L) break
    page <- page + 1L
  }
  if (!length(matches)) stop(sprintf("No Zenodo draft found for reserved DOI %s", doi), call. = FALSE)
  if (length(matches) > 1L) stop(sprintf("Multiple Zenodo drafts found for reserved DOI %s", doi), call. = FALSE)
  matches[[1L]]
}

dep <- if (!is.null(dep_id)) get_deposition(dep_id) else find_draft_by_doi(reserved_doi)
dep_id <- as.character(dep$id)
if (!nzchar(dep_id)) stop("Zenodo draft response is missing deposition id", call. = FALSE)
if (isTRUE(dep$submitted)) {
  stop(sprintf("Zenodo deposition %s is already submitted/published; manual RIS completion only accepts an existing draft", dep_id), call. = FALSE)
}
if (!is.null(reserved_doi) && !(reserved_doi %in% candidate_dois(dep))) {
  stop(sprintf("Deposition %s does not match reserved DOI %s", dep_id, reserved_doi), call. = FALSE)
}

# Prefer the modern InvenioRDM draft API used by current Zenodo UI-created drafts.
rdm_draft_url <- paste0("https://zenodo.org/api/records/", dep_id, "/draft")
rdm_resp <- perform_maybe(request(rdm_draft_url) |> auth(), 60)
rdm_status <- resp_status(rdm_resp)
cat(sprintf("ZENODO RDM DRAFT STATUS %d for %s\n", rdm_status, dep_id))
rdm_draft <- NULL
if (rdm_status == 200L) {
  rdm_draft <- resp_body_json(rdm_resp, simplifyVector = FALSE)
} else if (is.null(scalar(dep$links$bucket))) {
  body <- tryCatch(resp_body_string(rdm_resp), error = function(e) "")
  stop(sprintf("Modern Zenodo draft API returned HTTP %d for UI-created draft %s: %s", rdm_status, dep_id, body), call. = FALSE)
}

extract_entries <- function(x) {
  if (is.null(x)) return(list())
  if (!is.null(x$entries)) return(x$entries)
  if (is.list(x) && is.null(names(x))) return(x)
  list()
}

file_name <- function(x) scalar(x$key) %||% scalar(x$filename) %||% scalar(x$name)
file_checksum <- function(x) scalar(x$checksum)
file_size <- function(x) x$size %||% x$filesize %||% NULL

api_mode <- NULL
files_url <- NULL
file_entries <- list()

if (!is.null(rdm_draft)) {
  files_url <- scalar(rdm_draft$links$files)
  if (!is.null(files_url)) {
    fr <- perform(request(files_url) |> auth(), 200L, "RDM draft file listing", 60)
    file_entries <- extract_entries(resp_body_json(fr, simplifyVector = FALSE))
    api_mode <- "rdm"
  }
}

if (is.null(api_mode)) {
  files_url <- scalar(dep$links$files) %||% paste0(legacy_api, "/", dep_id, "/files")
  fr <- perform(request(files_url) |> auth(), 200L, "legacy deposition file listing", 60)
  file_entries <- extract_entries(resp_body_json(fr, simplifyVector = FALSE))
  api_mode <- "legacy"
}

if (!length(file_entries)) stop(sprintf("Zenodo draft %s contains no files", dep_id), call. = FALSE)

zenodo_files <- lapply(file_entries, function(x) {
  nm <- file_name(x)
  download <- NULL
  if (identical(api_mode, "rdm")) {
    download <- scalar(x$links$content)
  } else {
    download <- scalar(x$links$download)
    if (is.null(download)) {
      bucket <- scalar(dep$links$bucket)
      if (!is.null(bucket) && !is.null(nm)) download <- paste0(bucket, "/", URLencode(nm, reserved = TRUE))
    }
  }
  list(
    name = nm,
    download = download,
    checksum = file_checksum(x),
    size = file_size(x),
    entry = x
  )
})
zenodo_files <- zenodo_files[vapply(zenodo_files, function(x) !is.null(x$name), logical(1))]
ris_files <- zenodo_files[grepl("\\.ris$", vapply(zenodo_files, `[[`, character(1), "name"), ignore.case = TRUE)]
if (!length(ris_files)) stop(sprintf("Zenodo draft %s contains no .ris files", dep_id), call. = FALSE)

ris_names <- vapply(ris_files, `[[`, character(1), "name")
if (anyDuplicated(ris_names)) stop("Zenodo draft contains duplicate RIS filenames", call. = FALSE)

registered <- registry$input$files
registered_names <- if (is.null(registered) || !length(registered)) character() else
  vapply(registered, function(x) scalar(x$filename) %||% "", character(1))

expected_chunk_count <- registry$input$expected_chunk_count
if (!is.null(expected_chunk_count) && as.integer(expected_chunk_count) != length(ris_names)) {
  stop(sprintf(
    "Zenodo draft contains %d RIS chunks but registry input.expected_chunk_count is %d",
    length(ris_names),
    as.integer(expected_chunk_count)
  ), call. = FALSE)
}

if (length(registered_names)) {
  if (!setequal(registered_names, ris_names)) {
    stop(sprintf(
      "RIS files in Zenodo draft do not match registry. Missing from draft: [%s]. Unregistered in draft: [%s].",
      paste(setdiff(registered_names, ris_names), collapse = ", "),
      paste(setdiff(ris_names, registered_names), collapse = ", ")
    ), call. = FALSE)
  }
} else {
  registry$input$files <- lapply(ris_names, function(nm) list(
    filename = nm,
    exported_at = NULL,
    notes = "Discovered from existing Zenodo draft by Workflow 00"
  ))
  registry$input$expected_chunk_count <- length(ris_names)
}
registry$staging$deposition_id <- dep_id

resolved_registry <- file.path(output_dir, "resolved_source_registry.json")

download_dir <- file.path(output_dir, "downloaded_ris")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
local_ris <- character(length(ris_files))

for (i in seq_along(ris_files)) {
  z <- ris_files[[i]]
  if (is.null(z$download)) stop(sprintf("No content download link for Zenodo file %s", z$name), call. = FALSE)
  out <- file.path(download_dir, z$name)
  cat(sprintf("ZENODO DOWNLOAD %s\n", z$name))
  resp <- perform(request(z$download) |> auth(), 200L, paste0("file download: ", z$name), 1800)
  writeBin(resp_body_raw(resp), out)
  if (!file.exists(out) || file.info(out)$size <= 0) stop(sprintf("Downloaded RIS file is empty: %s", z$name), call. = FALSE)
  local_ris[[i]] <- out
}

# Resolve and verify authoritative raw-file fingerprints before ingestion.
for (i in seq_along(local_ris)) {
  nm <- basename(local_ris[[i]])
  bytes <- unname(file.info(local_ris[[i]])$size)
  sha <- digest(file = local_ris[[i]], algo = "sha256", serialize = FALSE)
  reg_ix <- which(vapply(registry$input$files, function(x) identical(scalar(x$filename), nm), logical(1)))
  if (length(reg_ix) != 1L) stop(sprintf("Resolved registry does not contain exactly one entry for %s", nm), call. = FALSE)
  existing_bytes <- registry$input$files[[reg_ix]]$bytes
  existing_sha <- scalar(registry$input$files[[reg_ix]]$sha256)
  if (!is.null(existing_bytes) && as.integer(existing_bytes) != as.integer(bytes)) {
    stop(sprintf("Registered byte size mismatch for %s: expected %s, got %s", nm, existing_bytes, bytes), call. = FALSE)
  }
  if (!is.null(existing_sha) && !identical(existing_sha, sha)) {
    stop(sprintf("Registered SHA-256 mismatch for %s: expected %s, got %s", nm, existing_sha, sha), call. = FALSE)
  }
  registry$input$files[[reg_ix]]$bytes <- bytes
  registry$input$files[[reg_ix]]$sha256 <- sha
}

writeLines(
  toJSON(registry, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  resolved_registry
)

ingest_dir <- file.path(output_dir, "ingestion")
ingest_script <- "scripts/updater/workflow_00_ris_ingestion.R"
if (!file.exists(ingest_script)) stop(sprintf("RIS ingestion script not found: %s", ingest_script), call. = FALSE)

cmd_args <- c(
  ingest_script,
  as.vector(rbind("--ris", local_ris)),
  "--registry", resolved_registry,
  "--output-dir", ingest_dir
)
status <- system2("Rscript", args = vapply(cmd_args, shQuote, character(1)))
if (!identical(status, 0L)) stop(sprintf("RIS ingestion failed with exit status %s", status), call. = FALSE)

derived <- c(
  source_registry.json = file.path(ingest_dir, "registry", "source_registry.json"),
  records.jsonl = file.path(ingest_dir, "handoff", "records.jsonl"),
  manifest.json = file.path(ingest_dir, "manifest.json"),
  SHA256SUMS = file.path(ingest_dir, "SHA256SUMS"),
  chunk_file_checksums.csv = file.path(ingest_dir, "audit", "chunk_file_checksums.csv"),
  exact_duplicate_source_record_ids.csv = file.path(ingest_dir, "audit", "exact_duplicate_source_record_ids.csv")
)
missing <- names(derived)[!file.exists(derived)]
if (length(missing)) stop(sprintf("Expected derived file(s) missing: %s", paste(missing, collapse = ", ")), call. = FALSE)

manifest <- fromJSON(file.path(ingest_dir, "manifest.json"), simplifyVector = FALSE)

if (isTRUE(validate_only)) {
  validation_receipt <- list(
    status = "validated_only_no_zenodo_write",
    zenodo_deposition_id = dep_id,
    reserved_doi = reserved_doi,
    zenodo_api_mode = api_mode,
    source = scalar(registry$database$short_name),
    database = registry$database,
    ris_files = lapply(zenodo_files[grepl("\\.ris$", vapply(zenodo_files, `[[`, character(1), "name"), ignore.case = TRUE)], function(z) list(
      name = z$name,
      size = z$size,
      zenodo_checksum = z$checksum
    )),
    unique_records_for_handover = manifest$records$unique_records_for_handover,
    exact_duplicate_rows_removed = manifest$records$exact_duplicate_rows_removed,
    reported_search_results = manifest$records$reported_search_results,
    reported_results_match_unique_records = manifest$records$reported_results_match_unique_records,
    zenodo_write_performed = FALSE,
    completed_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  )
  writeLines(
    toJSON(validation_receipt, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
    file.path(output_dir, "validation_only_receipt.json")
  )
  cat(sprintf(
    "PASS VALIDATE-ONLY: existing Zenodo draft %s; %s unique records; no Zenodo write performed\n",
    dep_id,
    manifest$records$unique_records_for_handover
  ))
  quit(save = "no", status = 0L)
}

# Upload derived files back into the same existing draft. Never publish here.
uploaded <- list()

if (identical(api_mode, "rdm")) {
  # Refresh file entries to support safe replacement on a re-run.
  fr <- perform(request(files_url) |> auth(), 200L, "RDM draft file refresh", 60)
  current_entries <- extract_entries(resp_body_json(fr, simplifyVector = FALSE))

  for (nm in names(derived)) {
    p <- derived[[nm]]
    existing <- current_entries[vapply(current_entries, function(x) identical(file_name(x), nm), logical(1))]
    if (length(existing)) {
      self_url <- scalar(existing[[1L]]$links$self)
      if (is.null(self_url)) stop(sprintf("Cannot replace existing draft file %s: missing self link", nm), call. = FALSE)
      perform(request(self_url) |> req_method("DELETE") |> auth(), 204L, paste0("delete existing derived file: ", nm), 60)
    }

    init_resp <- perform(
      request(files_url) |>
        req_method("POST") |>
        auth() |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(list(list(key = nm)), auto_unbox = TRUE),
      c(200L, 201L),
      paste0("initialise derived file: ", nm),
      60
    )
    init_body <- resp_body_json(init_resp, simplifyVector = FALSE)
    init_entries <- extract_entries(init_body)
    if (!length(init_entries)) stop(sprintf("Zenodo did not return an initialised file entry for %s", nm), call. = FALSE)
    init_match <- init_entries[vapply(init_entries, function(x) identical(file_name(x), nm), logical(1))]
    if (length(init_match) != 1L) {
      stop(sprintf("Zenodo initialisation response did not contain exactly one entry for %s", nm), call. = FALSE)
    }
    ent <- init_match[[1L]]
    content_url <- scalar(ent$links$content)
    commit_url <- scalar(ent$links$commit)
    if (is.null(content_url) || is.null(commit_url)) {
      stop(sprintf("Initialised Zenodo file %s is missing content or commit link", nm), call. = FALSE)
    }

    cat(sprintf("ZENODO UPLOAD %s bytes=%s\n", nm, file.info(p)$size))
    perform(
      request(content_url) |>
        req_method("PUT") |>
        auth() |>
        req_headers("Content-Type" = "application/octet-stream", Expect = "") |>
        req_body_file(p),
      c(200L, 201L),
      paste0("file content upload: ", nm),
      1800
    )
    commit_resp <- perform(
      request(commit_url) |> req_method("POST") |> auth(),
      c(200L, 201L, 202L),
      paste0("file commit: ", nm),
      120
    )
    commit_body <- tryCatch(resp_body_json(commit_resp, simplifyVector = FALSE), error = function(e) list())
    uploaded[[nm]] <- list(
      bytes = unname(file.info(p)$size),
      sha256 = digest(file = p, algo = "sha256", serialize = FALSE),
      zenodo_checksum = scalar(commit_body$checksum)
    )
  }
} else {
  dep <- get_deposition(dep_id)
  if (isTRUE(dep$submitted)) stop("Zenodo draft was published during validation; refusing to modify it", call. = FALSE)
  bucket <- scalar(dep$links$bucket)
  if (is.null(bucket)) stop("Legacy Zenodo draft response is missing bucket URL after validation", call. = FALSE)

  for (nm in names(derived)) {
    p <- derived[[nm]]
    url <- paste0(bucket, "/", URLencode(nm, reserved = TRUE))
    cat(sprintf("ZENODO UPLOAD %s bytes=%s\n", nm, file.info(p)$size))
    resp <- perform(
      request(url) |>
        req_method("PUT") |>
        auth() |>
        req_headers(Expect = "") |>
        req_body_file(p),
      c(200L, 201L),
      paste0("derived file upload: ", nm),
      1800
    )
    body <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) list())
    uploaded[[nm]] <- list(
      bytes = unname(file.info(p)$size),
      sha256 = digest(file = p, algo = "sha256", serialize = FALSE),
      zenodo_checksum = scalar(body$checksum)
    )
  }
}

receipt <- list(
  status = "validated_and_uploaded_to_existing_draft",
  zenodo_deposition_id = dep_id,
  reserved_doi = reserved_doi,
  zenodo_api_mode = api_mode,
  source = scalar(registry$database$short_name),
  database = registry$database,
  ris_files = ris_names,
  unique_records_for_handover = manifest$records$unique_records_for_handover,
  exact_duplicate_rows_removed = manifest$records$exact_duplicate_rows_removed,
  reported_search_results = manifest$records$reported_search_results,
  reported_results_match_unique_records = manifest$records$reported_results_match_unique_records,
  derived_files_uploaded = uploaded,
  publication_action = "none",
  draft_left_unpublished = TRUE,
  completed_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
)
writeLines(
  toJSON(receipt, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  file.path(output_dir, "zenodo_draft_completion_receipt.json")
)

cat(sprintf(
  "PASS: completed existing Zenodo draft %s in place using %s API; %s unique records; draft remains unpublished\n",
  dep_id,
  api_mode,
  manifest$records$unique_records_for_handover
))
