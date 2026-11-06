#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf('Missing value after %s', flag))
  args[[i + 1L]]
}

input_dir <- arg('--input-dir', 'inputs/scopus_ingestion')
output_dir <- arg('--output-dir', 'outputs/updater/scopus_sidecar_adapter')
if (!dir.exists(input_dir)) stop(sprintf('Input directory does not exist: %s', input_dir))
resolve_source_dir <- function(root) {
  direct <- file.path(root, 'raw')
  child <- file.path(root, 'source_child', 'raw')
  if (dir.exists(direct)) return(root)
  if (dir.exists(child)) return(file.path(root, 'source_child'))
  raw_dirs <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  raw_dirs <- raw_dirs[basename(raw_dirs) == 'raw']
  if (length(raw_dirs) == 1L) return(dirname(raw_dirs[[1L]]))
  stop(sprintf('Could not uniquely locate authoritative raw directory under %s', root))
}
input_dir <- resolve_source_dir(input_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(), tz = 'UTC', format = '%Y-%m-%dT%H:%M:%SZ')
trim_null <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
extract_scopus_id <- function(entry) {
  id <- trim_null(entry[['dc:identifier']])
  if (is.null(id)) return(NULL)
  sub('^SCOPUS_ID:', '', id)
}
extract_year <- function(entry) {
  d <- trim_null(entry[['prism:coverDate']])
  if (is.null(d)) return(NULL)
  m <- regexpr('^[0-9]{4}', d)
  if (m[1] == -1) NULL else regmatches(d, m)
}
normalise_affiliations <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  lapply(x, function(a) list(
    name = a[['affilname']] %||% NULL,
    city = a[['affiliation-city']] %||% NULL,
    country = a[['affiliation-country']] %||% NULL
  ))
}

raw_files <- sort(list.files(file.path(input_dir, 'raw'), pattern = '\\.json$', full.names = TRUE))
if (!length(raw_files)) stop(sprintf('No raw Scopus JSON files found under %s/raw', input_dir))

entries <- list()
for (f in raw_files) {
  x <- fromJSON(f, simplifyVector = FALSE)
  sr <- x[['search-results']]
  if (is.null(sr) || !is.list(sr)) stop(sprintf('Missing search-results in %s', f))
  es <- sr[['entry']] %||% list()
  entries <- c(entries, es)
}
if (!length(entries)) stop('Scopus sidecar adapter received zero entries')

retrieved_at <- now_utc()
records <- vector('list', length(entries))
coverage_rows <- vector('list', length(entries))

for (i in seq_along(entries)) {
  e <- entries[[i]]
  eid <- trim_null(e[['eid']])
  sid <- extract_scopus_id(e)
  doi <- trim_null(e[['prism:doi']])
  title <- trim_null(e[['dc:title']])
  first_author <- trim_null(e[['dc:creator']])
  source_title <- trim_null(e[['prism:publicationName']])
  cover_date <- trim_null(e[['prism:coverDate']])
  year <- extract_year(e)
  publication_type <- trim_null(e[['subtypeDescription']])
  affiliations <- normalise_affiliations(e[['affiliation']])

  if (is.null(eid) && is.null(sid)) stop(sprintf('Record %d has neither Scopus EID nor Scopus ID', i))

  sidecar_id <- if (!is.null(eid)) paste0('scopus:', eid) else paste0('scopus_id:', sid)
  records[[i]] <- list(
    sidecar_identity = list(
      sidecar_record_id = sidecar_id,
      scopus_eid = eid,
      scopus_id = sid,
      doi = doi
    ),
    source = list(
      provider = 'scopus',
      source_format = 'scopus_search_api_standard_json',
      api_view = 'STANDARD'
    ),
    scopus = list(raw_payload = e),
    mapped_fields = list(
      title = title,
      abstract = NULL,
      authors = if (is.null(first_author)) NULL else list(list(display_name = first_author, role = 'first_author_only')),
      first_author = first_author,
      year = year,
      publication_date = cover_date,
      source = source_title,
      doi = doi,
      keywords = NULL,
      author_keywords = NULL,
      publication_type = publication_type,
      affiliations = affiliations
    ),
    provenance = list(
      adapter_workflow = 'workflow_01s_scopus_sidecar_adapter',
      implementation_language = 'R',
      adapted_at = retrieved_at,
      source_stage = 'scopus_search_standard',
      canonical_json_modified = FALSE,
      downstream_workflows_modified = FALSE
    )
  )

  coverage_rows[[i]] <- data.frame(
    sidecar_record_id = sidecar_id,
    scopus_eid = eid %||% NA_character_,
    scopus_id = sid %||% NA_character_,
    doi = doi %||% NA_character_,
    title = title %||% NA_character_,
    first_author = first_author %||% NA_character_,
    year = year %||% NA_character_,
    source = source_title %||% NA_character_,
    publication_type = publication_type %||% NA_character_,
    affiliations_present = !is.null(affiliations),
    abstract_available = FALSE,
    full_authors_available = FALSE,
    keywords_available = FALSE,
    stringsAsFactors = FALSE
  )
}

sidecar_ids <- vapply(records, function(r) r$sidecar_identity$sidecar_record_id, character(1))
if (anyDuplicated(sidecar_ids)) stop('Duplicate Scopus sidecar_record_id values found in sample')

jsonl_path <- file.path(output_dir, 'scopus_sidecar_records.jsonl')
con <- file(jsonl_path, open = 'wt', encoding = 'UTF-8')
for (r in records) writeLines(toJSON(r, auto_unbox = TRUE, null = 'null', na = 'null', digits = NA), con)
close(con)

coverage <- do.call(rbind, coverage_rows)
write.csv(coverage, file.path(output_dir, 'field_coverage_records.csv'), row.names = FALSE, na = '')

present_pct <- function(x) round(100 * mean(!is.na(x) & nzchar(as.character(x))), 1)
manifest <- list(
  workflow = 'workflow_01s_scopus_sidecar_adapter',
  implementation_language = 'R',
  status = 'success',
  created_at = retrieved_at,
  input = list(
    source = 'Scopus Search API STANDARD artefact',
    raw_files = basename(raw_files),
    records_read = length(entries)
  ),
  output = list(
    sidecar_jsonl = 'scopus_sidecar_records.jsonl',
    field_coverage_csv = 'field_coverage_records.csv',
    canonical_json_modified = FALSE,
    canonical_branch_written = FALSE,
    downstream_workflows_modified = FALSE
  ),
  identifier_checks = list(
    unique_sidecar_ids = length(unique(sidecar_ids)),
    duplicate_sidecar_ids = anyDuplicated(sidecar_ids),
    eid_present_n = sum(!is.na(coverage$scopus_eid) & nzchar(coverage$scopus_eid)),
    scopus_id_present_n = sum(!is.na(coverage$scopus_id) & nzchar(coverage$scopus_id)),
    doi_present_n = sum(!is.na(coverage$doi) & nzchar(coverage$doi)),
    doi_missing_n = sum(is.na(coverage$doi) | !nzchar(coverage$doi))
  ),
  mapped_field_coverage_percent = list(
    title = present_pct(coverage$title),
    first_author = present_pct(coverage$first_author),
    year = present_pct(coverage$year),
    source = present_pct(coverage$source),
    doi = present_pct(coverage$doi),
    publication_type = present_pct(coverage$publication_type),
    affiliations = round(100 * mean(coverage$affiliations_present), 1),
    abstract = 0,
    full_authors = 0,
    keywords = 0
  ),
  workflow01_schema_compatibility = list(
    canonical_bibliographic_fields = c('title', 'abstract', 'authors', 'year', 'source', 'doi', 'author_keywords', 'publication_type'),
    safely_mappable_now = c('title', 'year', 'source', 'doi', 'publication_type'),
    partially_mappable_now = c('authors:first_author_only'),
    unavailable_in_standard_search_sample = c('abstract', 'full_authors', 'author_keywords'),
    source_specific_identity = c('scopus_eid','scopus_id'),
    canonical_materialisation_deferred = TRUE,
    note = 'Diagnostic source sidecar only. Canonical materialisation is performed later by the source-agnostic Workflow 01 canonical builder.'
  )
)
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null'), file.path(output_dir, 'compatibility_audit.json'))

message(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = 'null', na = 'null'))
message(sprintf('PASS: wrote %d Scopus source-manifestation sidecar records; canonical materialisation deferred.', length(records)))
