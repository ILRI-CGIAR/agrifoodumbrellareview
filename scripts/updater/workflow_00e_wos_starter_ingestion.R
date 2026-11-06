#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

query <- arg("--query")
modified_time_span <- trimws(arg("--modified-time-span", ""))
output_dir <- arg("--output-dir", "outputs/updater/wos_starter")
page_size <- as.integer(arg("--page-size", "50"))
start_page <- as.integer(arg("--start-page", "1"))
end_page_arg <- arg("--end-page", NULL)
count_only <- identical(tolower(arg("--count-only", "false")), "true")
reported_total_arg <- arg("--reported-total", NULL)
base_url <- arg("--base-url", "https://api.clarivate.com/apis/wos-starter/v1/documents")

if (is.null(query) || !nzchar(trimws(query))) stop("--query is required", call. = FALSE)
if (is.na(page_size) || page_size < 1L || page_size > 50L) stop("--page-size must be 1..50 for WoS Starter", call. = FALSE)
if (is.na(start_page) || start_page < 1L) stop("--start-page must be >= 1", call. = FALSE)

api_key <- Sys.getenv("WOS_STARTER_API", unset = "")
if (!nzchar(api_key)) stop("WOS_STARTER_API is required", call. = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  y <- as.character(x[[1L]])
  if (!nzchar(y)) default else y
}
as_int <- function(x, default = NA_integer_) {
  y <- suppressWarnings(as.integer(scalar(x, NA_character_)))
  if (is.na(y)) default else y
}

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
  writeLines(toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA), tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) stop(sprintf("Could not atomically write %s", path), call. = FALSE)
}

request_page <- function(page) {
  last_error <- NULL
  for (attempt in seq_len(6L)) {
    req <- request(base_url) |>
      req_headers(
        Accept = "application/json",
        `X-ApiKey` = api_key,
        `User-Agent` = "LivingEvidenceMap WoS Starter ingestion"
      ) |>
      req_url_query(q = query, db = "WOS", limit = page_size, page = page,
                    modifiedTimeSpan = if (nzchar(modified_time_span)) modified_time_span else NULL) |>
      req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(req_perform(req), error = identity)
    if (!inherits(resp, "error")) {
      status <- resp_status(resp)
      if (status < 400L) return(resp)
      body <- tryCatch(resp_body_string(resp), error = function(e) "")
      last_error <- sprintf("WoS Starter HTTP %d: %s", status, substr(body, 1L, 1200L))
      if (!(status == 429L || status >= 500L)) stop(last_error, call. = FALSE)
    } else {
      last_error <- conditionMessage(resp)
    }
    if (attempt < 6L) Sys.sleep(min(60, 2^(attempt - 1L)))
  }
  stop(sprintf("WoS Starter request failed after 6 attempts: %s", last_error), call. = FALSE)
}

parse_response <- function(resp, page) {
  body <- resp_body_string(resp)
  parsed <- tryCatch(fromJSON(body, simplifyVector = FALSE),
                     error = function(e) stop(sprintf("Invalid WoS JSON on page %d: %s", page, conditionMessage(e)), call. = FALSE))
  hits <- parsed$hits %||% list()
  meta <- parsed$metadata %||% list()
  list(body = body, parsed = parsed, hits = hits, total = as_int(meta$total), headers = resp_headers(resp))
}

started_at <- now_utc()

database_scope <- list(
  platform = "Web of Science",
  api_product = "Web of Science Starter API",
  endpoint = base_url,
  database_parameter = "WOS",
  databases_searched = list(
    list(
      code = "WOS",
      name = "Web of Science Core Collection"
    )
  ),
  selection_method = "Explicit db=WOS API parameter",
  recorded_at_utc = started_at,
  institutional_coverage_note = paste(
    "The content, editions and historical coverage available within the Web of Science Core Collection",
    "are determined by the institutional/API entitlement active at query time and may change."
  ),
  core_collection_editions = "Not exposed or independently verified by this Starter API request"
)

first <- NULL
if (is.null(reported_total_arg)) {
  first <- parse_response(request_page(1L), 1L)
  reported_total <- first$total
  if (is.na(reported_total)) stop("WoS Starter response did not report metadata.total", call. = FALSE)
} else {
  reported_total <- suppressWarnings(as.integer(reported_total_arg))
  if (is.na(reported_total) || reported_total < 0L) stop("--reported-total must be a non-negative integer", call. = FALSE)
}
total_pages <- max(1L, ceiling(reported_total / page_size))

if (count_only) {
  write_json(list(
    workflow = "00e_wos_starter_ingestion",
    implementation_language = "R",
    status = "count_success",
    counted_at = now_utc(),
    endpoint = base_url,
    database = "WOS",
    database_scope = database_scope,
    query = query,
    modified_time_span = if (nzchar(modified_time_span)) modified_time_span else NULL,
    search_scope = c("title", "abstract", "author_keywords"),
    keywords_plus_included = FALSE,
    total_results = reported_total,
    page_size = page_size,
    total_pages = total_pages,
    canonical_json_modified = FALSE
  ), manifest_path)
  cat(sprintf("WOS_TOTAL=%d\nWOS_TOTAL_PAGES=%d\n", reported_total, total_pages))
  quit(status = 0L)
}

end_page <- if (is.null(end_page_arg)) total_pages else as.integer(end_page_arg)
if (is.na(end_page) || end_page < start_page) stop("--end-page must be >= --start-page", call. = FALSE)
if (end_page > total_pages) end_page <- total_pages

manifest <- list(
  workflow = "00e_wos_starter_ingestion",
  implementation_language = "R",
  status = "running",
  started_at = started_at,
  endpoint = base_url,
  database = "WOS",
  database_scope = database_scope,
  query = query,
  modified_time_span = if (nzchar(modified_time_span)) modified_time_span else NULL,
  search_scope = c("title", "abstract", "author_keywords"),
  keywords_plus_included = FALSE,
  reported_total = reported_total,
  page_size = page_size,
  total_pages = total_pages,
  chunk_start_page = start_page,
  chunk_end_page = end_page,
  raw_response_preservation = TRUE,
  checkpoint_after_every_page = TRUE,
  canonicalisation_performed = FALSE,
  canonical_json_modified = FALSE,
  downstream_processing_performed = FALSE
)
write_json(manifest, manifest_path)

uids <- character()
retrieved <- 0L
page_summaries <- list()

for (page in seq.int(start_page, end_page)) {
  message(sprintf("Requesting WoS Starter page %d / %d", page, total_pages))
  res <- if (page == 1L && !is.null(first)) first else parse_response(request_page(page), page)
  if (!is.na(res$total) && res$total != reported_total) {
    stop(sprintf("WoS total changed during harvest: first=%d page_%d=%d", reported_total, page, res$total), call. = FALSE)
  }

  n <- length(res$hits)
  expected_n <- if (page < total_pages) page_size else reported_total - page_size * (total_pages - 1L)
  if (n != expected_n) {
    stop(sprintf("Unexpected record count on page %d: expected=%d returned=%d", page, expected_n, n), call. = FALSE)
  }

  raw_path <- file.path(raw_dir, sprintf("response_%06d.json", page))
  con <- file(raw_path, "wb"); writeBin(charToRaw(res$body), con); close(con)
  write_json(as.list(unclass(res$headers)), file.path(headers_dir, sprintf("response_%06d_headers.json", page)))

  page_uids <- vapply(res$hits, function(h) scalar(h$uid, ""), character(1))
  if (any(!nzchar(page_uids))) stop(sprintf("Missing WoS UID on page %d", page), call. = FALSE)
  if (anyDuplicated(page_uids)) stop(sprintf("Duplicate WoS UID within page %d", page), call. = FALSE)
  uids <- c(uids, page_uids)
  retrieved <- retrieved + n

  page_summaries[[length(page_summaries) + 1L]] <- list(
    page = page,
    returned_records = n,
    reported_total = res$total,
    raw_file = file.path("raw", basename(raw_path)),
    rate_limit_remaining = scalar(res$headers[["x-ratelimit-remaining"]], NULL),
    rate_limit_limit = scalar(res$headers[["x-ratelimit-limit"]], NULL)
  )

  write_json(list(
    workflow = "00e_wos_starter_ingestion",
    status = "running",
    updated_at = now_utc(),
    reported_total = reported_total,
    total_pages = total_pages,
    chunk_start_page = start_page,
    chunk_end_page = end_page,
    last_completed_page = page,
    pages_completed_in_chunk = page - start_page + 1L,
    records_retrieved_in_chunk = retrieved,
    canonical_json_modified = FALSE
  ), checkpoint_path)

  if (page < end_page) Sys.sleep(0.22)
}

dup_uids <- unique(uids[duplicated(uids)])
validation <- list(
  workflow = "00e_wos_starter_ingestion",
  validated_at = now_utc(),
  reported_total = reported_total,
  total_pages = total_pages,
  chunk_start_page = start_page,
  chunk_end_page = end_page,
  pages_retrieved = end_page - start_page + 1L,
  records_retrieved = retrieved,
  unique_wos_uids = length(unique(uids)),
  duplicate_wos_uids_n = length(dup_uids),
  canonical_json_modified = FALSE
)
write_json(validation, validation_path)
if (length(dup_uids)) stop(sprintf("Validation failure: %d duplicate WoS UIDs within chunk", length(dup_uids)), call. = FALSE)

manifest$status <- "success"
manifest$completed_at <- now_utc()
manifest$records_retrieved <- retrieved
manifest$pages_retrieved <- end_page - start_page + 1L
manifest$validation <- validation
manifest$pages <- page_summaries
manifest$output <- list(
  raw_responses = "raw/response_*.json",
  response_headers = "headers/response_*_headers.json",
  checkpoint = "checkpoint.json",
  validation = "validation.json",
  manifest = "manifest.json"
)
write_json(manifest, manifest_path)
write_json(list(
  workflow = "00e_wos_starter_ingestion",
  status = "success",
  completed_at = now_utc(),
  chunk_start_page = start_page,
  chunk_end_page = end_page,
  records_retrieved = retrieved,
  canonical_json_modified = FALSE
), checkpoint_path)

message(sprintf("PASS: WoS Starter chunk complete; pages %d-%d, %d records.", start_page, end_page, retrieved))
