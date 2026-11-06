#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag))
  args[[i + 1L]]
}

query <- arg("--query")
filter_query <- trimws(arg("--filter", ""))
max_records_arg <- arg("--max-records", "100")
page_size <- as.integer(arg("--page-size", "100"))
output_dir <- arg("--output-dir", "outputs/updater/openalex_ingestion_test")
base_url <- arg("--base-url", "https://api.openalex.org/")

if (is.null(query) || !nzchar(trimws(query))) stop("--query is required")
full_harvest <- identical(tolower(max_records_arg), "all")
if (!full_harvest) {
  max_records <- suppressWarnings(as.integer(max_records_arg))
  if (is.na(max_records) || max_records < 1L) stop("--max-records must be >= 1 or 'all'")
} else {
  max_records <- .Machine$integer.max
}
if (is.na(page_size) || page_size < 1L || page_size > 100L) stop("--page-size must be between 1 and 100")

api_key <- Sys.getenv("OPENALEX_API_KEY", unset = "")
if (!nzchar(api_key)) api_key <- Sys.getenv("OPENALEX_API_TOKEN", unset = "")
if (!nzchar(api_key)) stop("Neither OPENALEX_API_KEY nor OPENALEX_API_TOKEN is available")

`%||%` <- function(x, y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")

root <- normalizePath(output_dir, mustWork = FALSE)
raw_dir <- file.path(root, "raw")
headers_dir <- file.path(root, "headers")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(headers_dir, recursive = TRUE, showWarnings = FALSE)

manifest_path <- file.path(root, "manifest.json")
checkpoint_path <- file.path(root, "checkpoint.json")
validation_path <- file.path(root, "validation.json")

write_json <- function(x, path) {
  tmp <- paste0(path, ".tmp")
  writeLines(toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA),
             tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop(sprintf("Could not atomically write %s", path))
}

scalar_text <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  y <- as.character(x[[1L]])
  if (!nzchar(y)) default else y
}

request_page <- function(cursor, count) {
  last_error <- NULL
  for (attempt in seq_len(6L)) {
    req <- request(base_url) |>
      req_headers(
        Accept = "application/json",
        Authorization = paste("Bearer", api_key),
        `User-Agent` = "LivingEvidenceMap OpenAlex ingestion"
      ) |>
      req_url_query(
        oql = query,
        filter = if (nzchar(filter_query)) filter_query else NULL,
        `per-page` = count,
        cursor = cursor
      ) |>
      req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(req_perform(req), error = identity)
    if (!inherits(resp, "error")) {
      status <- resp_status(resp)
      if (status < 400L) return(resp)

      body <- tryCatch(resp_body_string(resp), error = function(e) "")
      last_error <- sprintf("OpenAlex HTTP %d: %s", status, substr(body, 1L, 1200L))
      retryable <- status == 429L || status >= 500L
      if (!retryable) stop(last_error)
    } else {
      last_error <- conditionMessage(resp)
    }

    if (attempt < 6L) {
      wait <- min(60, 2^(attempt - 1L))
      message(sprintf("OpenAlex request attempt %d failed (%s); retrying in %ss",
                      attempt, last_error, wait))
      Sys.sleep(wait)
    }
  }
  stop(sprintf("OpenAlex request failed after 6 attempts: %s", last_error))
}

started_at <- now_utc()
page <- 1L
cursor <- "*"
retrieved <- 0L
reported_total <- NA_integer_
reported_total_changed <- FALSE
page_summaries <- list()
cumulative_cost_usd <- 0

manifest <- list(
  workflow = "00c_openalex_ingestion",
  implementation_language = "R",
  status = "running",
  started_at = started_at,
  endpoint = base_url,
  query_parameter = "oql",
  query = query,
  filter = if (nzchar(filter_query)) filter_query else NULL,
  max_records_requested = if (full_harvest) "all" else max_records,
  requested_page_size = page_size,
  source = "OpenAlex Works API",
  search_scope = c("title", "abstract"),
  lens_search_scope_reference = c("title", "abstract", "keywords"),
  methodological_difference = "OpenAlex OQL title/abstract search excludes full text. Lens additionally searched keywords, so field scope is still not identical.",
  raw_response_preservation = TRUE,
  checkpoint_after_every_page = TRUE,
  canonicalisation_performed = FALSE,
  canonical_json_modified = FALSE,
  downstream_processing_performed = FALSE
)
write_json(manifest, manifest_path)

repeat {
  requested_count <- page_size
  if (!full_harvest) requested_count <- min(page_size, max_records - retrieved)
  if (requested_count <= 0L) break

  message(sprintf("Requesting OpenAlex page %d: cursor=%s count=%d",
                  page, if (page == 1L) "*" else "<next_cursor>", requested_count))
  resp <- request_page(cursor, requested_count)
  body_text <- resp_body_string(resp)
  headers <- resp_headers(resp)
  headers_json <- as.list(unclass(headers))

  raw_path <- file.path(raw_dir, sprintf("response_%06d.json", page))
  con <- file(raw_path, open = "wb")
  writeBin(charToRaw(body_text), con)
  close(con)

  header_path <- file.path(headers_dir, sprintf("response_%06d_headers.json", page))
  write_json(headers_json, header_path)

  parsed <- tryCatch(
    fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop(sprintf("OpenAlex returned invalid JSON on page %d: %s",
                                     page, conditionMessage(e)))
  )

  meta <- parsed[["meta"]]
  results <- parsed[["results"]]
  if (is.null(meta) || !is.list(meta)) stop(sprintf("Missing meta object on page %d", page))
  if (is.null(results)) results <- list()
  if (!is.list(results)) stop(sprintf("Unexpected results structure on page %d", page))

  this_total <- suppressWarnings(as.integer(meta[["count"]]))
  if (is.na(reported_total)) {
    reported_total <- this_total
  } else if (!is.na(this_total) && !identical(this_total, reported_total)) {
    reported_total_changed <- TRUE
    message(sprintf("NOTE: OpenAlex reported total changed during harvest: first=%d current=%d",
                    reported_total, this_total))
  }

  n_results <- length(results)
  next_cursor <- scalar_text(meta[["next_cursor"]], NA_character_)
  page_cost <- suppressWarnings(as.numeric(meta[["cost_usd"]] %||% 0))
  if (is.na(page_cost)) page_cost <- 0
  cumulative_cost_usd <- cumulative_cost_usd + page_cost

  page_summary <- list(
    page = page,
    requested_count = requested_count,
    returned_results = n_results,
    total_results_reported = this_total,
    cost_usd = page_cost,
    cumulative_cost_usd = cumulative_cost_usd,
    raw_file = file.path("raw", basename(raw_path)),
    headers_file = file.path("headers", basename(header_path)),
    rate_limit = scalar_text(headers[["x-ratelimit-limit"]], NULL),
    rate_limit_remaining = scalar_text(headers[["x-ratelimit-remaining"]], NULL),
    rate_limit_credits_used = scalar_text(headers[["x-ratelimit-credits-used"]], NULL),
    rate_limit_reset = scalar_text(headers[["x-ratelimit-reset"]], NULL)
  )
  page_summaries[[length(page_summaries) + 1L]] <- page_summary

  retrieved <- retrieved + n_results
  checkpoint <- list(
    workflow = "00c_openalex_ingestion",
    status = "running",
    updated_at = now_utc(),
    query = query,
    total_results_reported_first_page = reported_total,
    pages_completed = page,
    entries_retrieved = retrieved,
    next_cursor = if (!is.na(next_cursor) && nzchar(next_cursor)) next_cursor else NULL,
    cumulative_cost_usd = cumulative_cost_usd,
    max_records_requested = if (full_harvest) "all" else max_records
  )
  write_json(checkpoint, checkpoint_path)

  message(sprintf("OpenAlex page %d returned %d works; cumulative=%d; first-page total=%s; cumulative cost=$%.6f",
                  page, n_results, retrieved,
                  if (is.na(reported_total)) "unknown" else as.character(reported_total),
                  cumulative_cost_usd))

  if (n_results == 0L) break
  if (!full_harvest && retrieved >= max_records) break
  if (full_harvest && !is.na(reported_total) && retrieved >= reported_total) break
  if (is.na(next_cursor) || !nzchar(next_cursor)) break

  cursor <- next_cursor
  page <- page + 1L
}

raw_files <- sort(list.files(raw_dir, pattern = "^response_[0-9]{6}\\.json$", full.names = TRUE))
ids <- character()
recount <- 0L
for (rf in raw_files) {
  x <- fromJSON(rf, simplifyVector = FALSE)
  rs <- x[["results"]] %||% list()
  recount <- recount + length(rs)
  page_ids <- vapply(rs, function(w) scalar_text(w$id, ""), character(1))
  if (any(!nzchar(page_ids))) stop(sprintf("Missing OpenAlex work ID in %s", basename(rf)))
  ids <- c(ids, page_ids)
}

duplicate_ids <- unique(ids[duplicated(ids)])
count_matches_pages <- identical(recount, retrieved)
unique_id_count <- length(unique(ids))
expected_complete <- full_harvest && !is.na(reported_total)
count_matches_reported_total <- if (expected_complete) identical(recount, reported_total) else NA

validation <- list(
  workflow = "00c_openalex_ingestion",
  validated_at = now_utc(),
  raw_page_files = length(raw_files),
  records_recounted_from_raw = recount,
  records_tracked_during_harvest = retrieved,
  count_matches_page_accumulator = count_matches_pages,
  openalex_ids_present = length(ids),
  unique_openalex_ids = unique_id_count,
  duplicate_openalex_ids_n = length(duplicate_ids),
  duplicate_openalex_ids = duplicate_ids,
  full_harvest_requested = full_harvest,
  first_page_total_reported_by_openalex = reported_total,
  count_matches_first_page_total = count_matches_reported_total,
  reported_total_changed_during_harvest = reported_total_changed,
  duplicate_policy = "Raw API responses are preserved unchanged; exact repeated OpenAlex Work IDs are recorded here and collapsed deterministically downstream before identifier-delta comparison.",
  canonical_json_modified = FALSE
)
write_json(validation, validation_path)

if (!count_matches_pages) stop("Validation failure: raw-page recount does not match running retrieved count")
if (length(duplicate_ids) > 0L) {
  message(sprintf("NOTE: %d repeated OpenAlex Work IDs occurred across live cursor pages; raw responses remain unchanged and exact-ID repeats will be collapsed downstream.", length(duplicate_ids)))
}
# Raw pages may contain repeated Work IDs under live cursor pagination.
# Accept only when the unique-ID count exactly equals the reported total;
# preserve the raw-row discrepancy, IDs and unchanged API responses in validation.
duplicate_only_count_difference <- expected_complete &&
  !isTRUE(count_matches_reported_total) &&
  identical(as.integer(unique_id_count), as.integer(reported_total)) &&
  length(duplicate_ids) > 0L
validation$duplicate_only_count_difference <- duplicate_only_count_difference
write_json(validation, validation_path)
if (isTRUE(duplicate_only_count_difference)) {
  message(sprintf("NOTE: %d raw OpenAlex rows contain %d repeated Work IDs; %d distinct IDs match first-page reported total. Raw responses and repeated IDs retained for audit.",
                  recount, length(duplicate_ids), unique_id_count))
}
if (expected_complete && !isTRUE(count_matches_reported_total) && !isTRUE(reported_total_changed) && !isTRUE(duplicate_only_count_difference)) {
  stop(sprintf("Validation failure: full harvest retrieved %d records but OpenAlex first page reported %d without observed live-index count drift",
               recount, reported_total))
}
if (expected_complete && !isTRUE(count_matches_reported_total) && isTRUE(reported_total_changed)) {
  message(sprintf("NOTE: full-harvest raw count (%d) differs from first-page count (%d), accepted because OpenAlex reported live-index count drift during pagination.",
                  recount, reported_total))
}

manifest$status <- "success"
manifest$completed_at <- now_utc()
manifest$total_results_reported_by_openalex_first_page <- reported_total
manifest$entries_retrieved <- retrieved
manifest$pages_retrieved <- length(page_summaries)
manifest$cumulative_cost_usd <- cumulative_cost_usd
manifest$validation = validation
manifest$pages <- page_summaries
manifest$output <- list(
  raw_responses = "raw/response_*.json",
  response_headers = "headers/response_*_headers.json",
  checkpoint = "checkpoint.json",
  validation = "validation.json",
  manifest = "manifest.json"
)
write_json(manifest, manifest_path)

checkpoint$status <- "success"
checkpoint$completed_at <- now_utc()
checkpoint$next_cursor <- NULL
write_json(checkpoint, checkpoint_path)

message(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"))
message(sprintf("PASS: OpenAlex ingestion complete; %d raw works preserved across %d page(s); cost=$%.6f.",
                retrieved, length(page_summaries), cumulative_cost_usd))
