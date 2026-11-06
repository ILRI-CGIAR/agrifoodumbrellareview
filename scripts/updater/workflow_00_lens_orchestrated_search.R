#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf('Missing value after %s', flag))
  args[[i + 1L]]
}
config_path <- arg('--config', 'config/lens_search.json')
canonical_path <- arg('--output', 'data/canonical/current/repair/records.jsonl')
manifest_path <- arg('--manifest', 'data/canonical/current/repair/manifest.json')
audit_dir <- arg('--audit-dir', 'fresh_rebuild')

api <- 'https://api.lens.org/scholarly/search'
page_size <- 500L
scroll_ttl <- '1m'
sentinel_lens_id <- '033-262-857-738-715'
require_sentinel <- tolower(arg('--require-sentinel', 'true')) %in% c('true','1','yes')
token <- Sys.getenv('LENS_API_TOKEN', unset = '')
if (!nzchar(token)) stop('LENS_API_TOKEN is required')

now_utc <- function() format(Sys.time(), tz = 'UTC', format = '%Y-%m-%dT%H:%M:%SZ')
`%||%` <- function(x, y) if (is.null(x)) y else x
first_non_null <- function(...) {
  xs <- list(...)
  for (x in xs) if (!is.null(x) && length(x) > 0L && !identical(x, '')) return(x)
  NULL
}
source_title <- function(raw) {
  src <- raw$source
  if (is.null(src)) return(NULL)
  if (is.list(src)) return(first_non_null(src$title, src$name))
  src
}
doi_from <- function(raw) {
  ids <- raw$external_ids
  if (is.list(ids) && length(ids)) {
    for (x in ids) {
      if (is.list(x) && identical(tolower(as.character(x$type %||% '')), 'doi') && !is.null(x$value)) {
        return(trimws(as.character(x$value)))
      }
    }
  }
  if (!is.null(raw$doi)) trimws(as.character(raw$doi)) else NULL
}

cfg <- fromJSON(config_path, simplifyVector = FALSE)
query_string <- cfg$api_query$query$bool$must[[1]]$query_string$query
if (!grepl('salmonid* OR Salmo OR Oncorhynchus OR "rainbow trout"', query_string, fixed = TRUE)) {
  stop('Refusing Workflow 00: species clause must contain salmonid*, Salmo, Oncorhynchus and "rainbow trout"')
}
base_query <- cfg$api_query$query
query_sha <- digest(query_string, algo = 'sha256', serialize = FALSE)
retrieved_at <- now_utc()

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(canonical_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)

lens_request <- function(payload) {
  last_error <- NULL
  for (attempt in seq_len(6L)) {
    req <- request(api) |>
      req_headers(
        Authorization = paste('Bearer', token),
        `Content-Type` = 'application/json',
        Accept = 'application/json'
      ) |>
      req_body_json(payload, auto_unbox = TRUE)
    resp <- tryCatch(req_perform(req), error = identity)
    if (!inherits(resp, 'error')) {
      status <- resp_status(resp)
      if (status < 400L) return(resp_body_json(resp, simplifyVector = FALSE))
      retryable <- status == 429L || status >= 500L
      last_error <- sprintf('Lens HTTP %s', status)
      if (!retryable) stop(last_error)
    } else {
      last_error <- conditionMessage(resp)
    }
    if (attempt < 6L) {
      delay <- min(60, 2^(attempt - 1L))
      message(sprintf('Lens request attempt %d failed (%s); retrying in %ss', attempt, last_error, delay))
      Sys.sleep(delay)
    }
  }
  stop(sprintf('Lens request failed after 6 attempts: %s', last_error))
}

payload <- list(query = base_query, size = page_size, scroll = scroll_ttl)
seen <- new.env(hash = TRUE, parent = emptyenv())
rows <- list()
raw_count <- 0L
duplicate_ids <- 0L
no_lens_id <- 0L
batches <- 0L
reported_total <- NULL

repeat {
  body <- lens_request(payload)
  batches <- batches + 1L
  if (is.null(reported_total)) {
    reported_total <- as.integer(body$total %||% 0L)
    message(sprintf('Lens reported total: %d', reported_total))
  }
  data <- body$data %||% list()
  raw_count <- raw_count + length(data)
  for (raw in data) {
    lid <- trimws(as.character(raw$lens_id %||% ''))
    if (!nzchar(lid)) {
      no_lens_id <- no_lens_id + 1L
      next
    }
    if (exists(lid, envir = seen, inherits = FALSE)) {
      duplicate_ids <- duplicate_ids + 1L
      next
    }
    assign(lid, TRUE, envir = seen)
    year <- first_non_null(raw$year_published, raw$date_published)
    rec <- list(
      identity = list(lens_id = lid, record_id = lid, record_id_type = 'lens_id'),
      source = list(provider = 'lens', source_format = 'lens_api_json'),
      lens = list(raw_payload = raw),
      canonical = list(
        record_id = lid,
        lens_id = lid,
        title = raw$title,
        abstract = raw$abstract,
        authors = raw$authors,
        year = year,
        source = source_title(raw),
        doi = doi_from(raw),
        keywords = raw$keywords,
        publication_type = raw$publication_type
      ),
      provenance = list(
        ingestion_workflow = 'workflow_00_full_lens_search',
        implementation_language = 'R',
        retrieved_at = retrieved_at,
        query_config = config_path,
        query_sha256 = query_sha,
        fresh_rebuild = TRUE
      )
    )
    rows[[length(rows) + 1L]] <- rec
  }
  message(sprintf('Fetched raw=%d; unique Lens IDs=%d', raw_count, length(rows)))
  if (length(data) == 0L || (!is.null(reported_total) && raw_count >= reported_total)) break
  scroll_id <- body$scroll_id %||% ''
  if (!nzchar(scroll_id)) {
    stop(sprintf('Lens reported %d records but returned no scroll_id after %d', reported_total, raw_count))
  }
  payload <- list(scroll_id = scroll_id, scroll = scroll_ttl)
}

if (!length(rows)) stop('Fresh Lens search returned zero records')
if (!is.null(reported_total) && raw_count < reported_total) {
  stop(sprintf('Incomplete Lens harvest: %d < reported total %d', raw_count, reported_total))
}
lens_ids <- vapply(rows, function(r) r$identity$lens_id, character(1))
if (anyDuplicated(lens_ids)) stop('Internal Lens-ID uniqueness failure')
sentinel_present <- sentinel_lens_id %in% lens_ids
if (require_sentinel && !sentinel_present) {
  stop(sprintf('Regression guard failed: known eligible Lens record %s was not returned by the corrected search', sentinel_lens_id))
}

con <- file(canonical_path, open = 'wt', encoding = 'UTF-8')
for (r in rows) writeLines(toJSON(r, auto_unbox = TRUE, null = 'null', na = 'null', digits = NA), con)
close(con)

manifest <- list(
  workflow = 'workflow_00_full_lens_search',
  implementation_language = 'R',
  status = 'success',
  created_at = retrieved_at,
  source = 'Lens Scholarly API',
  query_config = config_path,
  query_string = query_string,
  query_sha256 = query_sha,
  rainbow_trout_present_in_query = TRUE,
  regression_sentinel_lens_id = sentinel_lens_id,
  regression_sentinel_required = require_sentinel,
  regression_sentinel_present = sentinel_present,
  lens_reported_total = reported_total,
  raw_records_retrieved = raw_count,
  unique_records_written = length(rows),
  duplicate_lens_ids_suppressed = duplicate_ids,
  records_without_lens_id = no_lens_id,
  batches = batches,
  canonical_path = canonical_path,
  pipeline_stage = 'lens_search_complete',
  fresh_state = list(
    deduplication_present = FALSE,
    publication_status_present = FALSE,
    screening_present = FALSE,
    screening_history_present = FALSE
  )
)
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null'), manifest_path)
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null'), file.path(audit_dir, 'search_manifest.json'))
message(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null'))
message(sprintf('PASS: Lens harvest complete; sentinel_required=%s sentinel_present=%s.', require_sentinel, sentinel_present))
