#!/usr/bin/env Rscript

suppressPackageStartupMessages({
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
coalesce_null <- function(x, y) if (is.null(x)) y else x
clean <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  y <- as.character(x[[1L]])
  if (is.na(y) || !nzchar(trimws(y))) NA_character_ else y
}
read_jsonl <- function(path) {
  con <- file(path, "rt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  out <- list()
  repeat {
    x <- readLines(con, n = 1000L, warn = FALSE)
    if (!length(x)) break
    x <- x[nzchar(trimws(x))]
    if (length(x)) out <- c(out, lapply(x, fromJSON, simplifyVector = FALSE))
  }
  out
}

input_path <- arg("--input")
audit_path <- arg("--audit")
output_dir <- arg("--output-dir")
status_path <- arg("--status")
if (any(vapply(list(input_path, audit_path, output_dir, status_path), is.null, logical(1)))) {
  stop("Required: --input --audit --output-dir --status", call. = FALSE)
}
if (!file.exists(input_path)) stop("Input canonical JSONL not found", call. = FALSE)
if (!file.exists(audit_path)) stop("Audit file not found", call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

audit <- read_jsonl(audit_path)
conflict_audit <- Filter(function(x) length(coalesce_null(x$quarantined, list())) > 0L, audit)
raw_conflict_ids <- vapply(conflict_audit, function(x) clean(x$record_id), character(1))
if (any(is.na(raw_conflict_ids))) stop("Quarantined audit entry missing stable record_id", call. = FALSE)
conflict_ids <- unique(raw_conflict_ids)

if (!length(conflict_ids)) {
  status <- list(
    schema = "living-evidence-map-workflow02-human-review-status-v1",
    workflow = "02",
    blocking_cases = 0L,
    requires_human_review = FALSE,
    audit_sha256 = digest(file = audit_path, algo = "sha256", serialize = FALSE)
  )
  writeLines(toJSON(status, auto_unbox = TRUE, pretty = TRUE), status_path, useBytes = TRUE)
  cat("PASS: no Workflow 02 quarantined conflicts require human review\n")
  quit(status = 0L)
}

wanted <- setNames(rep(TRUE, length(conflict_ids)), conflict_ids)
context <- list()
con <- file(input_path, "rt", encoding = "UTF-8")
repeat {
  x <- readLines(con, n = 1000L, warn = FALSE)
  if (!length(x)) break
  x <- x[nzchar(trimws(x))]
  for (line in x) {
    rec <- fromJSON(line, simplifyVector = FALSE)
    rid <- clean(rec$identity$record_id)
    if (!is.na(rid) && rid %in% names(wanted)) {
      context[[rid]] <- list(
        record_id = rid,
        doi = clean(rec$canonical$doi),
        title = clean(rec$canonical$title),
        abstract = clean(rec$canonical$abstract)
      )
    }
  }
}
close(con)

rows <- list()
json_rows <- list()
k <- 0L
for (a in conflict_audit) {
  rid <- clean(a$record_id)
  ctx <- coalesce_null(context[[rid]], list(record_id = rid, doi = clean(a$doi), title = NA_character_, abstract = NA_character_))
  for (q in coalesce_null(a$quarantined, list())) {
    k <- k + 1L
    provider <- clean(q$provider)
    field <- clean(q$field)
    reason <- clean(q$reason)
    sim <- suppressWarnings(as.numeric(coalesce_null(q$title_similarity, NA_real_)))
    returned_doi <- clean(q$returned_doi)
    eid <- clean(q$eid)
    provider_title <- NA_character_
    if (identical(provider, "europe_pmc")) provider_title <- clean(a$europe_pmc$title)
    if (identical(provider, "scopus")) {
      sc_provider <- if (identical(field, "author_keywords") && !is.null(a$scopus_keywords)) a$scopus_keywords else a$scopus
      provider_title <- clean(sc_provider$title)
    }

    rows[[k]] <- data.frame(
      record_id = rid,
      doi = clean(a$doi),
      canonical_title = clean(ctx$title),
      provider = provider,
      field = field,
      reason = reason,
      provider_title = provider_title,
      title_similarity = sim,
      returned_doi = returned_doi,
      eid = eid,
      human_decision = "",
      human_note = "",
      stringsAsFactors = FALSE
    )
    json_rows[[k]] <- list(
      record_id = rid,
      doi = clean(a$doi),
      canonical = ctx,
      conflict = q,
      provider_response = if (identical(provider, "europe_pmc")) {
        a$europe_pmc
      } else if (identical(provider, "scopus")) {
        if (identical(field, "author_keywords") && !is.null(a$scopus_keywords)) a$scopus_keywords else a$scopus
      } else NULL
    )
  }
}

review_csv <- file.path(output_dir, "workflow02_human_review.csv")
review_jsonl <- file.path(output_dir, "workflow02_human_review.jsonl")
write.csv(do.call(rbind, rows), review_csv, row.names = FALSE, na = "")
con <- file(review_jsonl, "wt", encoding = "UTF-8")
for (x in json_rows) writeLines(toJSON(x, auto_unbox = TRUE, null = "null", na = "null"), con)
close(con)

manifest <- list(
  schema = "living-evidence-map-workflow02-human-review-package-v1",
  workflow = "02",
  blocking_records = length(conflict_ids),
  blocking_conflicts = length(json_rows),
  record_ids = sort(conflict_ids),
  source_audit_sha256 = digest(file = audit_path, algo = "sha256", serialize = FALSE),
  source_input_sha256 = digest(file = input_path, algo = "sha256", serialize = FALSE),
  review_csv = list(filename = basename(review_csv), sha256 = digest(file = review_csv, algo = "sha256", serialize = FALSE)),
  review_jsonl = list(filename = basename(review_jsonl), sha256 = digest(file = review_jsonl, algo = "sha256", serialize = FALSE))
)
manifest_path <- file.path(output_dir, "workflow02_human_review_manifest.json")
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null"), manifest_path, useBytes = TRUE)

status <- list(
  schema = "living-evidence-map-workflow02-human-review-status-v1",
  workflow = "02",
  blocking_cases = length(conflict_ids),
  blocking_conflicts = length(json_rows),
  requires_human_review = TRUE,
  manifest_sha256 = digest(file = manifest_path, algo = "sha256", serialize = FALSE)
)
writeLines(toJSON(status, auto_unbox = TRUE, pretty = TRUE), status_path, useBytes = TRUE)

cat(sprintf("HUMAN_REVIEW_REQUIRED: %d records (%d quarantined conflicts)\n", length(conflict_ids), length(json_rows)))
