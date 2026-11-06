#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

input_dir <- arg("--input-dir", "inputs/wos_starter")
output_dir <- arg("--output-dir", "outputs/updater/wos_starter_sidecar_adapter")
if (!dir.exists(input_dir)) stop(sprintf("Input directory does not exist: %s", input_dir), call. = FALSE)
resolve_source_dir <- function(root) {
  direct <- file.path(root, "raw")
  child <- file.path(root, "source_child", "raw")
  if (dir.exists(direct)) return(root)
  if (dir.exists(child)) return(file.path(root, "source_child"))
  raw_dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  raw_dirs <- raw_dirs[basename(raw_dirs) == "raw"]
  if (length(raw_dirs) == 1L) return(dirname(raw_dirs[[1L]]))
  stop(sprintf("Could not uniquely locate authoritative raw directory under %s", root), call. = FALSE)
}
input_dir <- resolve_source_dir(input_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
scalar <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
vec <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  y <- unname(as.character(unlist(x, use.names = FALSE)))
  y <- trimws(y)
  y <- y[nzchar(y)]
  if (!length(y)) NULL else unique(y)
}

extract_authors <- function(names_obj) {
  aa <- names_obj$authors %||% list()
  if (!length(aa)) return(NULL)
  lapply(aa, function(a) list(
    display_name = scalar(a$displayName),
    wos_standard = scalar(a$wosStandard),
    researcher_id = scalar(a$researcherId)
  ))
}

extract_citation_count <- function(citations) {
  if (is.null(citations) || !length(citations)) return(NULL)
  vals <- vapply(citations, function(z) {
    db <- scalar(z$db)
    n <- suppressWarnings(as.integer(scalar(z$count)))
    if (identical(db, "WOS") && !is.na(n)) n else NA_integer_
  }, integer(1))
  vals <- vals[!is.na(vals)]
  if (!length(vals)) NULL else vals[[1L]]
}

raw_files <- sort(list.files(file.path(input_dir, "raw"), pattern = "^response_[0-9]{6}\\.json$", full.names = TRUE))
if (!length(raw_files)) stop("No WoS Starter raw JSON files found", call. = FALSE)

hits <- list()
for (rf in raw_files) {
  x <- fromJSON(rf, simplifyVector = FALSE)
  hs <- x$hits %||% list()
  if (!is.list(hs)) stop(sprintf("Unexpected hits structure in %s", rf), call. = FALSE)
  hits <- c(hits, hs)
}
if (!length(hits)) stop("WoS Starter sidecar adapter received zero records", call. = FALSE)

adapted_at <- now_utc()
out <- vector("list", length(hits))
rows <- vector("list", length(hits))

for (i in seq_along(hits)) {
  h <- hits[[i]]
  uid <- scalar(h$uid)
  if (is.null(uid)) stop(sprintf("WoS record %d has no UID", i), call. = FALSE)

  ids <- h$identifiers %||% list()
  source <- h$source %||% list()
  keywords <- h$keywords %||% list()

  doi <- scalar(ids$doi)
  title <- scalar(h$title)
  authors <- extract_authors(h$names %||% list())
  author_keywords <- vec(keywords$authorKeywords)
  publication_type <- vec(h$types)
  source_types <- vec(h$sourceTypes)
  year <- scalar(source$publishYear)
  source_title <- scalar(source$sourceTitle)
  volume <- scalar(source$volume)
  issue <- scalar(source$issue)
  pages <- source$pages %||% list()
  page_range <- scalar(pages$range)
  if (is.null(page_range)) {
    b <- scalar(pages$begin); e <- scalar(pages$end)
    page_range <- if (!is.null(b) && !is.null(e)) paste0(b, "-", e) else b %||% e
  }

  sidecar_id <- paste0("wos:", uid)
  out[[i]] <- list(
    sidecar_identity = list(
      sidecar_record_id = sidecar_id,
      wos_uid = uid,
      doi = doi
    ),
    source = list(
      provider = "wos_starter",
      source_format = "wos_starter_api_json",
      source_collection = "Web of Science Core Collection",
      database_code = "WOS",
      search_scope = c("title", "abstract", "author_keywords"),
      keywords_plus_included = FALSE
    ),
    wos = list(raw_payload = h),
    mapped_fields = list(
      title = title,
      abstract = NULL,
      authors = authors,
      year = year,
      source = source_title,
      doi = doi,
      author_keywords = author_keywords,
      publication_type = publication_type,
      volume = volume,
      issue = issue,
      pages = page_range,
      issn = scalar(ids$issn),
      eissn = scalar(ids$eissn),
      pmid = scalar(ids$pmid),
      source_types = source_types,
      times_cited_wos = extract_citation_count(h$citations)
    ),
    provenance = list(
      adapter_workflow = "workflow_01w_wos_starter_sidecar_adapter",
      implementation_language = "R",
      adapted_at = adapted_at,
      source_stage = "wos_starter_ti_ab_ak_search",
      canonical_json_modified = FALSE,
      downstream_workflows_modified = FALSE
    )
  )

  rows[[i]] <- data.frame(
    sidecar_record_id = sidecar_id,
    wos_uid = uid,
    doi = doi %||% NA_character_,
    title = title %||% NA_character_,
    year = year %||% NA_character_,
    source = source_title %||% NA_character_,
    authors_present = !is.null(authors) && length(authors) > 0L,
    author_keywords_present = !is.null(author_keywords) && length(author_keywords) > 0L,
    publication_type_present = !is.null(publication_type) && length(publication_type) > 0L,
    abstract_present = FALSE,
    stringsAsFactors = FALSE
  )
}

ids <- vapply(out, function(z) z$sidecar_identity$sidecar_record_id, character(1))
if (anyDuplicated(ids)) stop("Duplicate WoS sidecar_record_id values found", call. = FALSE)

jsonl_path <- file.path(output_dir, "wos_starter_sidecar_records.jsonl")
con <- file(jsonl_path, "wt", encoding = "UTF-8")
for (r in out) writeLines(toJSON(r, auto_unbox = TRUE, null = "null", na = "null", digits = NA), con)
close(con)

coverage <- do.call(rbind, rows)
write.csv(coverage, file.path(output_dir, "field_coverage_records.csv"), row.names = FALSE, na = "")

present <- function(x) !is.na(x) & nzchar(as.character(x))
pct <- function(x) round(100 * mean(x), 1)

audit <- list(
  workflow = "workflow_01w_wos_starter_sidecar_adapter",
  status = "success",
  created_at = adapted_at,
  input = list(
    source = "Web of Science Starter API",
    records_read = length(hits),
    raw_files = length(raw_files)
  ),
  output = list(
    sidecar_jsonl = "wos_starter_sidecar_records.jsonl",
    field_coverage_csv = "field_coverage_records.csv",
    canonical_json_modified = FALSE,
    canonical_branch_written = FALSE,
    downstream_workflows_modified = FALSE
  ),
  identifier_checks = list(
    unique_sidecar_ids = length(unique(ids)),
    duplicate_sidecar_ids = anyDuplicated(ids),
    wos_uid_present_n = sum(present(coverage$wos_uid)),
    doi_present_n = sum(present(coverage$doi)),
    doi_missing_n = sum(!present(coverage$doi))
  ),
  mapped_field_coverage_percent = list(
    title = pct(present(coverage$title)),
    abstract = 0,
    authors = pct(coverage$authors_present),
    author_keywords = pct(coverage$author_keywords_present),
    year = pct(present(coverage$year)),
    source = pct(present(coverage$source)),
    doi = pct(present(coverage$doi)),
    publication_type = pct(coverage$publication_type_present)
  ),
  canonical_schema_compatibility = list(
    canonical_bibliographic_fields = c("title","abstract","authors","year","source","doi","author_keywords","publication_type","volume","issue","pages"),
    safely_mappable_now = c("title","authors","year","source","doi","author_keywords","publication_type","volume","issue","pages"),
    unavailable_from_starter = c("abstract","funding","affiliations"),
    source_specific_identity = "wos_uid",
    retained_sidecar_only = c("issn","eissn","pmid","source_types","times_cited_wos","raw_payload"),
    note = "WoS Starter is compatible with the existing source-manifestation model. Missing abstracts can be handled by Workflow 01 enrichment; no canonical schema change is required."
  )
)
writeLines(toJSON(audit, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
           file.path(output_dir, "compatibility_audit.json"))

message(sprintf("PASS: wrote %d WoS Starter sidecar records; canonical JSON untouched.", length(out)))
