#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)

arg_one <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}
arg_many <- function(flag) {
  pos <- which(args == flag)
  if (!length(pos)) return(character())
  vapply(pos, function(i) {
    if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
    args[[i + 1L]]
  }, character(1))
}

ris_paths <- arg_many("--ris")
registry_path <- arg_one("--registry")
output_dir <- arg_one("--output-dir", "outputs/updater/workflow00_ris")

if (!length(ris_paths) || is.null(registry_path)) {
  stop("Required: one or more --ris <file.ris> arguments and --registry <registry.json>", call. = FALSE)
}
if (any(!file.exists(ris_paths))) {
  stop(sprintf("RIS file(s) not found: %s", paste(ris_paths[!file.exists(ris_paths)], collapse = ", ")), call. = FALSE)
}
if (!file.exists(registry_path)) stop(sprintf("Registry file not found: %s", registry_path), call. = FALSE)
if (anyDuplicated(basename(ris_paths))) stop("Supplied RIS filenames must be unique", call. = FALSE)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x
scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
clean_text <- function(x) {
  x <- scalar(x)
  if (is.null(x)) return(NULL)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) NULL else x
}
as_values <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(unlist(x, use.names = FALSE)))
  y <- y[!is.na(y) & nzchar(y)]
  if (!length(y)) NULL else unname(y)
}
norm_doi <- function(x) {
  x <- clean_text(x)
  if (is.null(x)) return(NULL)
  x <- tolower(x)
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x, perl = TRUE)
  x <- sub("^doi:\\s*", "", x, perl = TRUE)
  x <- sub("[?#].*$", "", x, perl = TRUE)
  x <- sub("[.,;:]+$", "", x, perl = TRUE)
  if (!nzchar(x)) NULL else x
}
first_present <- function(r, tags) {
  for (tag in tags) {
    x <- r[[tag]]
    if (!is.null(x) && length(x)) return(x)
  }
  NULL
}
year_from <- function(x) {
  x <- scalar(x)
  if (is.null(x)) return(NULL)
  m <- regexpr("(19|20)[0-9]{2}", x, perl = TRUE)
  if (m[[1L]] < 0L) return(NULL)
  as.integer(regmatches(x, m))
}
parse_sn <- function(x) {
  vals <- as_values(x)
  if (is.null(vals)) return(list(issn = NULL, isbn = NULL, raw = NULL))
  tokens <- unlist(strsplit(vals, "[[:space:];,]+", perl = TRUE), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  clean <- gsub("[^0-9Xx]", "", tokens)
  issn <- tokens[grepl("^[0-9]{4}-?[0-9]{3}[0-9Xx]$", tokens)]
  isbn_ix <- nchar(clean) %in% c(10L, 13L) & grepl("^[0-9Xx-]+$", tokens)
  list(
    issn = if (length(issn)) unique(issn) else NULL,
    isbn = if (any(isbn_ix)) unique(tokens[isbn_ix]) else NULL,
    raw = vals
  )
}
sha256_file <- function(path) digest(file = path, algo = "sha256", serialize = FALSE)
record_hash <- function(r) digest(toJSON(r, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
                                  algo = "sha256", serialize = FALSE)

registry <- fromJSON(registry_path, simplifyVector = FALSE)
required_top <- c("schema_version", "acquisition", "database", "search", "field_semantics", "input")
missing_top <- required_top[!vapply(required_top, function(nm) !is.null(registry[[nm]]), logical(1))]
if (length(missing_top)) stop(sprintf("Registry missing required field(s): %s", paste(missing_top, collapse = ", ")), call. = FALSE)
if (!identical(registry$schema_version, "workflow00-source-registry-v1")) stop("Unsupported registry schema_version", call. = FALSE)
if (!identical(scalar(registry$acquisition$method), "ris_upload")) stop("Registry acquisition.method must be ris_upload", call. = FALSE)
if (!identical(scalar(registry$acquisition$source_format), "RIS")) stop("Registry acquisition.source_format must be RIS", call. = FALSE)

database_name <- scalar(registry$database$name)
provider_slug <- scalar(registry$database$short_name)
if (is.null(database_name)) stop("Registry database.name is required", call. = FALSE)
if (is.null(provider_slug) || !grepl("^[a-z0-9][a-z0-9_-]*$", provider_slug)) {
  stop("Registry database.short_name must be a stable lower-case source code", call. = FALSE)
}

kw_semantics <- scalar(registry$field_semantics$ris_KW)
allowed_kw_semantics <- c("author_keywords", "indexing_terms", "mixed", "unknown")
if (is.null(kw_semantics) || !(kw_semantics %in% allowed_kw_semantics)) {
  stop("Registry field_semantics.ris_KW must be author_keywords, indexing_terms, mixed, or unknown", call. = FALSE)
}

registry_files <- registry$input$files
if (is.null(registry_files) || !length(registry_files)) stop("Registry input.files must contain at least one file", call. = FALSE)
registered_names <- vapply(registry_files, function(x) scalar(x$filename) %||% "", character(1))
if (any(!nzchar(registered_names))) stop("Every registry input.files entry requires filename", call. = FALSE)
if (anyDuplicated(registered_names)) stop("Registry input.files contains duplicate filenames", call. = FALSE)

supplied_names <- basename(ris_paths)
if (!setequal(registered_names, supplied_names)) {
  missing <- setdiff(registered_names, supplied_names)
  extra <- setdiff(supplied_names, registered_names)
  stop(sprintf("RIS inputs do not match registry. Missing: [%s]. Extra: [%s].",
               paste(missing, collapse = ", "), paste(extra, collapse = ", ")), call. = FALSE)
}
ris_paths <- ris_paths[match(registered_names, supplied_names)]

expected_chunks <- registry$input$expected_chunk_count
if (!is.null(expected_chunks) && as.integer(expected_chunks) != length(ris_paths)) {
  stop(sprintf("Expected %d RIS chunks but received %d", as.integer(expected_chunks), length(ris_paths)), call. = FALSE)
}

audit_dir <- file.path(output_dir, "audit")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

file_sha <- vapply(ris_paths, sha256_file, character(1))
file_checksum_audit <- data.frame(
  filename = basename(ris_paths),
  bytes = unname(file.info(ris_paths)$size),
  sha256 = unname(file_sha),
  stringsAsFactors = FALSE
)
write.csv(file_checksum_audit, file.path(audit_dir, "chunk_file_checksums.csv"), row.names = FALSE)

dup_sha <- unique(file_sha[duplicated(file_sha) | duplicated(file_sha, fromLast = TRUE)])
if (length(dup_sha)) {
  dup_rows <- do.call(rbind, lapply(dup_sha, function(h) {
    files <- basename(ris_paths[file_sha == h])
    data.frame(
      sha256 = h,
      duplicated_filenames = paste(files, collapse = ";"),
      duplicate_file_count = length(files),
      stringsAsFactors = FALSE
    )
  }))
  write.csv(dup_rows, file.path(audit_dir, "duplicate_chunk_checksums.csv"), row.names = FALSE)
  msg <- vapply(seq_along(dup_sha), function(i) paste(basename(ris_paths[file_sha == dup_sha[[i]]]), collapse = " = "), character(1))
  stop(sprintf("Duplicate RIS chunk content detected by SHA-256: %s. Check audit/chunk_file_checksums.csv and audit/duplicate_chunk_checksums.csv.",
               paste(msg, collapse = "; ")), call. = FALSE)
}

parse_ris <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\\ufeff", "", lines)
  tag_re <- "^([A-Z0-9]{2})[[:space:]]*-[[:space:]]?(.*)$"
  records <- list()
  current <- NULL
  last_tag <- NULL

  append_tag <- function(rec, tag, value) {
    if (is.null(rec[[tag]])) rec[[tag]] <- list(value)
    else rec[[tag]][[length(rec[[tag]]) + 1L]] <- value
    rec
  }

  for (line in lines) {
    m <- regexec(tag_re, line, perl = TRUE)
    hit <- regmatches(line, m)[[1L]]
    if (length(hit)) {
      tag <- hit[[2L]]
      value <- hit[[3L]]
      if (tag == "TY") {
        if (!is.null(current)) stop(sprintf("%s: TY before previous record ended with ER", basename(path)), call. = FALSE)
        current <- list()
        current <- append_tag(current, tag, value)
      } else if (tag == "ER") {
        if (is.null(current)) stop(sprintf("%s: ER outside an RIS record", basename(path)), call. = FALSE)
        records[[length(records) + 1L]] <- current
        current <- NULL
      } else if (!is.null(current)) {
        current <- append_tag(current, tag, value)
      }
      last_tag <- tag
    } else if (!is.null(current) && !is.null(last_tag) && nzchar(trimws(line))) {
      # Some database RIS exports wrap long field values onto continuation
      # lines without preserving leading indentation. A non-tag line inside
      # an open TY...ER record therefore continues the preceding RIS field.
      cont <- trimws(line)
      vals <- current[[last_tag]]
      if (is.null(vals) || !length(vals)) current <- append_tag(current, last_tag, cont)
      else current[[last_tag]][[length(vals)]] <- paste(vals[[length(vals)]], cont)
    } else if (nzchar(trimws(line)) && is.null(current)) {
      stop(sprintf("%s: non-empty text outside an RIS record: %s", basename(path), line), call. = FALSE)
    }
  }
  if (!is.null(current)) stop(sprintf("%s: file ended before ER terminator", basename(path)), call. = FALSE)
  records
}

make_source_record_id <- function(r) {
  candidates <- c(
    scalar(first_present(r, c("AN", "ID", "UT"))),
    norm_doi(first_present(r, c("DO"))),
    scalar(first_present(r, c("UR")))
  )
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]
  if (length(candidates)) return(paste0(provider_slug, ":", candidates[[1L]]))
  payload <- paste(
    clean_text(first_present(r, c("TI", "T1", "CT"))),
    clean_text(first_present(r, c("PY", "Y1", "DA"))),
    paste(as_values(first_present(r, c("AU", "A1"))) %||% character(), collapse = ";"),
    sep = "|"
  )
  paste0(provider_slug, ":ris_sha256:", substr(digest(payload, algo = "sha256", serialize = FALSE), 1L, 24L))
}

normalise_record <- function(r) {
  title <- clean_text(first_present(r, c("TI", "T1", "CT", "BT")))
  abstract <- clean_text(first_present(r, c("AB", "N2")))
  authors <- as_values(c(r$AU %||% list(), r$A1 %||% list()))
  doi <- norm_doi(first_present(r, c("DO")))
  journal <- clean_text(first_present(r, c("JO", "JF", "JA", "T2")))
  pub_date <- clean_text(first_present(r, c("DA", "Y1", "PY")))
  year <- year_from(first_present(r, c("PY", "Y1", "DA")))
  ris_keywords <- as_values(r$KW)
  author_keywords <- if (identical(kw_semantics, "author_keywords")) ris_keywords else NULL
  indexing_terms <- if (kw_semantics %in% c("indexing_terms", "mixed", "unknown")) ris_keywords else NULL
  publication_type <- unique(c(as_values(r$TY) %||% character(), as_values(r$M3) %||% character()))
  if (!length(publication_type)) publication_type <- NULL
  language <- as_values(r$LA)
  sn <- parse_sn(r$SN)
  volume <- clean_text(r$VL)
  issue <- clean_text(r$IS)
  pages <- clean_text(first_present(r, c("SP", "EP")))
  source_record_id <- make_source_record_id(r)

  list(
    source = list(
      provider = provider_slug,
      source_format = "ris",
      source_collection = database_name
    ),
    sidecar_identity = list(
      sidecar_record_id = source_record_id,
      source_record_id = source_record_id,
      doi = doi
    ),
    mapped_fields = list(
      title = title,
      abstract = abstract,
      authors = authors,
      structured_authors = NULL,
      year = year,
      publication_date = pub_date,
      source = journal,
      doi = doi,
      author_keywords = author_keywords,
      indexing_terms = indexing_terms,
      publication_type = publication_type,
      volume = volume,
      issue = issue,
      pages = pages,
      issn = sn$issn,
      isbn = sn$isbn,
      eissn = NULL,
      issn_l = NULL,
      language = language,
      affiliations = NULL,
      institutions = NULL
    ),
    source_specific = list(
      ris_keyword_semantics = kw_semantics,
      database_export_label = clean_text(r$DB),
      serial_number_raw = sn$raw,
      publisher = clean_text(r$PB),
      publication_place = clean_text(r$PP),
      record_url = clean_text(r$UR),
      country_of_publication = clean_text(r$C4),
      elocation = clean_text(r$C6)
    ),
    provenance = list(
      acquisition_method = "ris_upload",
      raw_payload_location = "Workflow 00 restricted Zenodo search archive",
      raw_payload_duplicated_in_canonical_jsonl = FALSE
    )
  )
}

chunk_records <- vector("list", length(ris_paths))
chunk_audit <- vector("list", length(ris_paths))
all_rows <- list()

for (j in seq_along(ris_paths)) {
  path <- ris_paths[[j]]
  raw <- parse_ris(path)
  if (!length(raw)) stop(sprintf("%s contains zero complete RIS records", basename(path)), call. = FALSE)
  ids <- vapply(raw, make_source_record_id, character(1))
  hashes <- vapply(raw, record_hash, character(1))

  chunk_records[[j]] <- raw
  chunk_audit[[j]] <- list(
    filename = basename(path),
    bytes = unname(file.info(path)$size),
    sha256 = file_sha[[j]],
    parsed_records = length(raw),
    unique_source_record_ids = length(unique(ids)),
    duplicate_source_record_ids_within_chunk = length(ids) - length(unique(ids))
  )

  for (i in seq_along(raw)) {
    all_rows[[length(all_rows) + 1L]] <- list(
      chunk = basename(path),
      source_record_id = ids[[i]],
      payload_sha256 = hashes[[i]],
      raw = raw[[i]]
    )
  }
}

all_ids <- vapply(all_rows, function(x) x$source_record_id, character(1))
id_groups <- split(seq_along(all_rows), all_ids)
duplicate_groups <- id_groups[lengths(id_groups) > 1L]

conflicts <- list()
exact_duplicates <- list()
for (id in names(duplicate_groups)) {
  ix <- duplicate_groups[[id]]
  hashes <- unique(vapply(all_rows[ix], function(x) x$payload_sha256, character(1)))
  chunks <- unique(vapply(all_rows[ix], function(x) x$chunk, character(1)))
  if (length(hashes) > 1L) {
    conflicts[[length(conflicts) + 1L]] <- data.frame(
      source_record_id = id,
      occurrences = length(ix),
      chunks = paste(chunks, collapse = ";"),
      distinct_payload_hashes = length(hashes),
      stringsAsFactors = FALSE
    )
  } else {
    exact_duplicates[[length(exact_duplicates) + 1L]] <- data.frame(
      source_record_id = id,
      occurrences = length(ix),
      chunks = paste(chunks, collapse = ";"),
      duplicate_rows_removed = length(ix) - 1L,
      stringsAsFactors = FALSE
    )
  }
}

raw_dir <- file.path(output_dir, "raw")
registry_dir <- file.path(output_dir, "registry")
handoff_dir <- file.path(output_dir, "handoff")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(registry_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(handoff_dir, recursive = TRUE, showWarnings = FALSE)

if (length(conflicts)) {
  conflict_df <- do.call(rbind, conflicts)
  write.csv(conflict_df, file.path(audit_dir, "conflicting_source_record_ids.csv"), row.names = FALSE)
  stop(sprintf("Conflicting payloads found for %d repeated source_record_id values; refusing to combine chunks", nrow(conflict_df)), call. = FALSE)
}

exact_df <- if (length(exact_duplicates)) do.call(rbind, exact_duplicates) else
  data.frame(source_record_id = character(), occurrences = integer(), chunks = character(),
             duplicate_rows_removed = integer(), stringsAsFactors = FALSE)
write.csv(exact_df, file.path(audit_dir, "exact_duplicate_source_record_ids.csv"), row.names = FALSE)

keep_ix <- vapply(id_groups, function(ix) ix[[1L]], integer(1))
keep_ix <- sort(unname(keep_ix))
unique_rows <- all_rows[keep_ix]
records <- lapply(unique_rows, function(x) normalise_record(x$raw))

for (j in seq_along(ris_paths)) {
  dest <- file.path(raw_dir, basename(ris_paths[[j]]))
  if (!file.copy(ris_paths[[j]], dest, overwrite = TRUE)) stop(sprintf("Failed to stage %s", basename(ris_paths[[j]])), call. = FALSE)
}
registry_copy <- file.path(registry_dir, "source_registry.json")
if (!file.copy(registry_path, registry_copy, overwrite = TRUE)) stop("Failed to stage source registry", call. = FALSE)

handoff_path <- file.path(handoff_dir, "records.jsonl")
con <- file(handoff_path, "wt", encoding = "UTF-8")
for (r in records) writeLines(toJSON(r, auto_unbox = TRUE, null = "null", na = "null", digits = NA), con, useBytes = TRUE)
close(con)

parsed_total <- length(all_rows)
unique_total <- length(records)
duplicates_removed <- parsed_total - unique_total
reported <- registry$search$reported_results
count_match <- if (is.null(reported)) NULL else identical(as.integer(reported), unique_total)

file_manifest <- lapply(seq_along(ris_paths), function(j) {
  staged <- file.path(raw_dir, basename(ris_paths[[j]]))
  list(
    filename = basename(ris_paths[[j]]),
    path = file.path("raw", basename(ris_paths[[j]])),
    bytes = unname(file.info(staged)$size),
    sha256 = sha256_file(staged),
    parsed_records = chunk_audit[[j]]$parsed_records,
    unique_source_record_ids = chunk_audit[[j]]$unique_source_record_ids,
    duplicate_source_record_ids_within_chunk = chunk_audit[[j]]$duplicate_source_record_ids_within_chunk
  )
})

status <- if (isFALSE(count_match)) "record_count_mismatch" else "success"
manifest <- list(
  schema = "living-evidence-map-workflow00-ris-harvest-v2",
  status = status,
  source = provider_slug,
  database = registry$database,
  search = registry$search,
  acquisition = registry$acquisition,
  chunks = list(
    expected = expected_chunks %||% NULL,
    received = length(ris_paths),
    files = file_manifest,
    duplicate_file_checksums = 0L
  ),
  records = list(
    parsed_across_chunks = parsed_total,
    exact_duplicate_rows_removed = duplicates_removed,
    unique_records_for_handover = unique_total,
    conflicting_source_record_ids = 0L,
    reported_search_results = reported %||% NULL,
    reported_results_match_unique_records = count_match
  ),
  handoff_contract = "Workflow 00 -> Workflow 01 existing source + source_record_id manifestation identity",
  files = list(
    registry = list(path = file.path("registry", "source_registry.json"),
                    bytes = unname(file.info(registry_copy)$size),
                    sha256 = sha256_file(registry_copy)),
    handoff_jsonl = list(path = file.path("handoff", "records.jsonl"),
                         bytes = unname(file.info(handoff_path)$size),
                         sha256 = sha256_file(handoff_path)),
    chunk_checksum_audit = list(path = file.path("audit", "chunk_file_checksums.csv"),
                                bytes = unname(file.info(file.path(audit_dir, "chunk_file_checksums.csv"))$size),
                                sha256 = sha256_file(file.path(audit_dir, "chunk_file_checksums.csv"))),
    duplicate_audit = list(path = file.path("audit", "exact_duplicate_source_record_ids.csv"),
                           bytes = unname(file.info(file.path(audit_dir, "exact_duplicate_source_record_ids.csv"))$size),
                           sha256 = sha256_file(file.path(audit_dir, "exact_duplicate_source_record_ids.csv")))
  ),
  generated_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
)

manifest_path <- file.path(output_dir, "manifest.json")
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), manifest_path)

checksum_paths <- c(ris_paths = file.path(raw_dir, basename(ris_paths)),
                    registry = registry_copy,
                    handoff = handoff_path,
                    chunk_checksum_audit = file.path(audit_dir, "chunk_file_checksums.csv"),
                    duplicate_audit = file.path(audit_dir, "exact_duplicate_source_record_ids.csv"),
                    manifest = manifest_path)
checksum_lines <- vapply(checksum_paths, function(p) {
  rel <- substring(p, nchar(output_dir) + 2L)
  sprintf("%s  %s", sha256_file(p), rel)
}, character(1))
writeLines(checksum_lines, file.path(output_dir, "SHA256SUMS"))

if (isFALSE(count_match)) {
  stop(sprintf("Unique RIS record count (%d) does not match registry search.reported_results (%d). Audit outputs were written.",
               unique_total, as.integer(reported)), call. = FALSE)
}

message(sprintf("PASS: %d RIS chunk(s), %d parsed rows, %d exact duplicate row(s) removed, %d unique records for source=%s",
                length(ris_paths), parsed_total, duplicates_removed, unique_total, provider_slug))
message(sprintf("W00 handover: %s", handoff_path))
