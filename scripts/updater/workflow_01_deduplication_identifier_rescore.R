#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(stringi)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

input_dir <- arg("--input-dir")
output_dir <- arg("--output-dir")
source_run_id <- arg("--source-run-id", "35585544686")
start_row <- as.integer(arg("--start-row", "1"))
end_row_arg <- arg("--end-row", NULL)

if (is.null(input_dir) || is.null(output_dir)) {
  stop("Required: --input-dir --output-dir", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

timestamp_utc <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
progress <- function(stage, completed = NULL, total = NULL, extra = NULL) {
  msg <- paste0("[", timestamp_utc(), "] ", stage)
  if (!is.null(completed)) {
    msg <- paste0(msg, ": ", completed)
    if (!is.null(total)) msg <- paste0(msg, " / ", total)
  }
  if (!is.null(extra) && nzchar(extra)) msg <- paste0(msg, " | ", extra)
  cat(msg, "\n")
  flush.console()
}
checkpoint <- function(stage, completed = NULL, total = NULL, extra = list()) {
  x <- c(list(
    workflow = "01_deduplication_identifier_rescore",
    updated_at = timestamp_utc(),
    stage = stage,
    completed = completed,
    total = total,
    source_benchmark_run_id = source_run_id
  ), extra)
  writeLines(
    toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
    file.path(output_dir, "checkpoint_progress.json")
  )
}

scored_path <- file.path(input_dir, "scored_sample.csv")
if (!file.exists(scored_path)) {
  hits <- list.files(input_dir, pattern = "^scored_sample\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(hits)) stop("scored_sample.csv not found in benchmark artefact", call. = FALSE)
  scored_path <- hits[[1L]]
}

progress("reading saved scored benchmark sample")
x <- fread(scored_path, na.strings = c("", "NA"))
if (!nrow(x)) {
  required0 <- c("title_i","title_j","classification","rule","record_i","record_j")
  missing0 <- setdiff(required0,names(x))
  if(length(missing0)) stop(sprintf("Empty scored state missing required columns: %s",paste(missing0,collapse=", ")),call.=FALSE)
  x[, `:=`(
    rescored_classification=classification,
    rescored_rule=rule,
    review_route=character(.N),
    manual_review_needed=logical(.N),
    decision_changed=logical(.N)
  )]
  fwrite(x,file.path(output_dir,"rescored_sample.csv"))
  summary <- list(
    workflow="01_deduplication_identifier_rescore",status="success",
    source_benchmark_run_id=source_run_id,sample_n=0L,source_full_n=0L,
    start_row=1L,end_row=0L,weak_auto_pairs_checked=0L,decisions_changed_to_review=0L,
    original_classification_counts=list(),rescored_classification_counts=list(),
    changed_by_original_rule=list(),remaining_automatic_by_rule=list(),
    historic_human_adjudications_loaded=FALSE,candidate_generation_repeated=FALSE,
    source_candidate_artifact_reused=TRUE
  )
  writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
             file.path(output_dir,"summary.json"))
  cat("PASS: zero scored duplicate candidates; wrote empty rescored state\n")
  quit(save="no",status=0L)
}
required <- c("title_i","title_j","classification","rule","record_i","record_j")
missing <- setdiff(required, names(x))
if (length(missing)) stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
full_n <- nrow(x)
end_row <- if (is.null(end_row_arg)) full_n else as.integer(end_row_arg)
if (is.na(start_row) || is.na(end_row) || start_row < 1L || end_row < start_row || end_row > full_n) {
  stop(sprintf("Invalid row range: %s-%s for %s rows", start_row, end_row, full_n), call. = FALSE)
}
x[, source_row_index := seq_len(.N)]
if (start_row != 1L || end_row != full_n) {
  x <- x[source_row_index >= start_row & source_row_index <= end_row]
}
progress("saved sample loaded", nrow(x), full_n,
         sprintf("processing source rows %d-%d", start_row, end_row))
checkpoint("sample_loaded", nrow(x), full_n,
           list(start_row=start_row, end_row=end_row, chunk_rows=nrow(x)))

meta_path <- file.path(input_dir, "normalised_metadata.csv")
if (!file.exists(meta_path)) {
  hits <- list.files(input_dir, pattern = "^normalised_metadata\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(hits)) stop("normalised_metadata.csv not found in benchmark artefact", call. = FALSE)
  meta_path <- hits[[1L]]
}
meta <- fread(meta_path, na.strings = c("", "NA"))
if (!"idx" %in% names(meta)) stop("normalised_metadata.csv lacks idx", call. = FALSE)
setkey(meta, idx)

labels_path <- file.path("data", "validation", "workflow02_v2_human_labels_2026-09-21.csv")
if (!file.exists(labels_path)) stop("Human validation label file not found", call. = FALSE)
labels <- fread(labels_path)
if (!all(c("record_i","record_j","truth") %in% names(labels))) stop("Human validation labels lack required columns", call. = FALSE)
labels[, pair_key := paste(pmin(record_i, record_j), pmax(record_i, record_j), sep = "::")]
if (anyDuplicated(labels$pair_key)) stop("Duplicate pair keys in human validation labels", call. = FALSE)
progress("validation labels loaded", nrow(labels), nrow(labels))

normalise_visible_title <- function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
  s <- stri_trans_nfkc(s)
  stri_trans_tolower(s)
}

canonical_label <- function(x) {
  x <- tolower(trimws(x))
  x <- stri_replace_all_regex(x, "\\s+", " ")
  if (identical(x, "supplementary file")) return("supplementary_file")
  if (identical(x, "additional file")) return("additional_file")
  map <- c(
    "pt" = "part", "part" = "part",
    "study" = "study",
    "experiment" = "experiment",
    "trial" = "trial",
    "report" = "report",
    "no" = "number", "number" = "number",
    "interview" = "interview",
    "episode" = "episode",
    "chapter" = "chapter",
    "section" = "section",
    "series" = "series",
    "vol" = "volume", "volume" = "volume",
    "issue" = "issue",
    "supplement" = "supplementary_file", "supplementary" = "supplementary_file",
    "file" = "file", "additional" = "additional_file",
    "appendix" = "appendix",
    "phase" = "phase"
  )
  unname(map[x])
}

labelled_ids <- function(s) {
  s <- normalise_visible_title(s)
  if (is.na(s)) return(list())
  pattern <- paste0(
    "(?i)(?<!\\p{L})",
    "(supplementary\\s+file|additional\\s+file|part|pt|study|experiment|trial|report|no|number|interview|episode|chapter|section|series|vol|volume|issue|supplement|supplementary|file|appendix|phase)",
    "\\s*[\\.:#-]?\\s*",
    "([0-9]+(?:\\.[0-9]+)?|[ivxlcdm]+)",
    "(?!\\p{L})"
  )
  m <- stri_match_all_regex(s, pattern)[[1L]]
  if (is.matrix(m) && nrow(m) && !all(is.na(m))) {
    out <- list()
    for (i in seq_len(nrow(m))) {
      lab <- canonical_label(m[i,2])
      val <- toupper(m[i,3])
      if (is.na(lab) || is.na(val)) next
      out[[lab]] <- sort(unique(c(out[[lab]] %||% character(), val)))
    }
    return(out)
  }
  list()
}

years_in_title <- function(s) {
  s <- normalise_visible_title(s)
  if (is.na(s)) return(character())
  m <- stri_extract_all_regex(s, "(?<![0-9])(?:18|19|20|21)[0-9]{2}(?![0-9])")[[1L]]
  sort(unique(m[!is.na(m)]))
}

roman_ids <- function(s) {
  s <- normalise_visible_title(s)
  if (is.na(s)) return(character())
  # Exclude standalone I because it is frequently a pronoun; labelled Phase I/Part I
  # is already captured by labelled_ids().
  m <- stri_extract_all_regex(s, "(?i)(?<!\\p{L})(?:ii|iii|iv|v|vi|vii|viii|ix|x|xi|xii|xiii|xiv|xv|xvi|xvii|xviii|xix|xx)(?!\\p{L})")[[1L]]
  sort(unique(toupper(m[!is.na(m)])))
}

generic_numbers <- function(s) {
  s <- normalise_visible_title(s)
  if (is.na(s)) return(character())
  m <- stri_extract_all_regex(s, "(?<![\\p{L}0-9])[0-9]+(?:\\.[0-9]+)?(?![\\p{L}0-9])")[[1L]]
  sort(unique(m[!is.na(m)]))
}

set_differs <- function(a, b) {
  length(a) > 0L && length(b) > 0L && !identical(sort(unique(a)), sort(unique(b)))
}

identifier_conflict <- function(a, b) {
  reasons <- character()

  la <- labelled_ids(a)
  lb <- labelled_ids(b)
  common_labels <- intersect(names(la), names(lb))
  if (length(common_labels)) {
    for (lab in common_labels) {
      if (set_differs(la[[lab]], lb[[lab]])) {
        reasons <- c(reasons, paste0("label:", lab, "=", paste(la[[lab]], collapse = ","), " vs ", paste(lb[[lab]], collapse = ",")))
      }
    }
  }

  ya <- years_in_title(a); yb <- years_in_title(b)
  if (set_differs(ya, yb)) {
    reasons <- c(reasons, paste0("year=", paste(ya, collapse = ","), " vs ", paste(yb, collapse = ",")))
  }

  ra <- roman_ids(a); rb <- roman_ids(b)
  if (set_differs(ra, rb)) {
    reasons <- c(reasons, paste0("roman=", paste(ra, collapse = ","), " vs ", paste(rb, collapse = ",")))
  }

  na <- generic_numbers(a); nb <- generic_numbers(b)
  if (set_differs(na, nb)) {
    reasons <- c(reasons, paste0("numeric=", paste(na, collapse = ","), " vs ", paste(nb, collapse = ",")))
  }

  list(
    conflict = length(reasons) > 0L,
    reasons = paste(unique(reasons), collapse = "; ")
  )
}

# Regression checks from the observed false-positive patterns.
stopifnot(identifier_conflict("Mike and Drenda Bayliss Interview 08", "Mike and Drenda Bayliss Interview 03")$conflict)
stopifnot(identifier_conflict("Annual Report No. 60, 2015", "Annual Report No. 61, 2016")$conflict)
stopifnot(identifier_conflict("Studies on Viral Diseases of Japanese Fishes-III", "Studies on Viral Diseases of Japanese Fishes-V")$conflict)
stopifnot(identifier_conflict("Experiment Part 1", "Experiment Part 2")$conflict)
stopifnot(!identifier_conflict("A 3-dimensional positioning study", "A 3-dimensional positioning study")$conflict)
stopifnot(identifier_conflict("Supplementary file 1.xlsx", "Supplementary file 8.xlsx")$conflict)
stopifnot(identifier_conflict("Additional file 7: study data", "Additional file 2: study data")$conflict)


first_author_from_norm <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  strsplit(x, "|", fixed = TRUE)[[1L]][[1L]]
}

is_preprint_manifestation <- function(doi, journal) {
  d <- ifelse(is.na(doi), "", tolower(doi))
  j <- ifelse(is.na(journal), "", tolower(journal))
  grepl("^10\\.21203/rs\\.3\\.", d) ||
    grepl("^10\\.1101/", d) ||
    grepl("^10\\.2139/ssrn\\.", d) ||
    grepl("^10\\.20944/", d) ||
    grepl("biorxiv|medrxiv|researchsquare|socialscienceresearchnetwork|ssrn|arxiv|peerjpreprints|preprintsorg", j)
}

title_length_norm <- function(s) {
  if (is.na(s) || !nzchar(s)) return(0L)
  nchar(stri_replace_all_regex(stri_trans_tolower(stri_trans_nfkc(s)), "[\\p{P}\\p{S}\\p{Z}\\s]+", ""), type = "chars")
}



journal_contains <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(FALSE)
  grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)
}

is_generic_exact_title <- function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(FALSE)
  z <- stri_trans_tolower(stri_trans_nfkc(s))
  z <- stri_replace_all_regex(z, "[\\p{P}\\p{S}\\p{Z}\\s]+", "")
  z %in% c("occurrencedownload", "index", "bookreviews")
}

pair_meta <- function(i, j) {
  a <- meta[.(i)]
  b <- meta[.(j)]
  if (!nrow(a) || !nrow(b)) return(NULL)
  fa <- first_author_from_norm(a$author_norm[[1L]])
  fb <- first_author_from_norm(b$author_norm[[1L]])
  yd <- if (!is.na(a$year[[1L]]) && !is.na(b$year[[1L]])) abs(a$year[[1L]] - b$year[[1L]]) else NA_integer_
  list(
    abstract_missing_i = is.na(a$abstract_hash[[1L]]) || !nzchar(a$abstract_hash[[1L]]),
    abstract_missing_j = is.na(b$abstract_hash[[1L]]) || !nzchar(b$abstract_hash[[1L]]),
    first_author_match = !is.na(fa) && !is.na(fb) && identical(fa, fb),
    exact_author_match = !is.na(a$author_norm[[1L]]) && !is.na(b$author_norm[[1L]]) &&
      nzchar(a$author_norm[[1L]]) && nzchar(b$author_norm[[1L]]) &&
      identical(a$author_norm[[1L]], b$author_norm[[1L]]),
    journal_match = !is.na(a$journal_norm[[1L]]) && !is.na(b$journal_norm[[1L]]) &&
      nzchar(a$journal_norm[[1L]]) && nzchar(b$journal_norm[[1L]]) &&
      identical(a$journal_norm[[1L]], b$journal_norm[[1L]]),
    journal_containment = journal_contains(a$journal_norm[[1L]], b$journal_norm[[1L]]),
    year_diff = yd,
    doi_i_present = !is.na(a$doi_norm[[1L]]) && nzchar(a$doi_norm[[1L]]),
    doi_j_present = !is.na(b$doi_norm[[1L]]) && nzchar(b$doi_norm[[1L]]),
    preprint_i = is_preprint_manifestation(a$doi_norm[[1L]], a$journal_norm[[1L]]),
    preprint_j = is_preprint_manifestation(b$doi_norm[[1L]], b$journal_norm[[1L]])
  )
}

weak_auto_rules <- c(
  "title_containment_strong_abstract",
  "title_similarity_0.97_strong_abstract",
  "title_similarity_0.95_exact_author_year"
)

x[, identifier_conflict := FALSE]
x[, identifier_conflict_reason := ""]
x[, rescored_classification := classification]
x[, rescored_rule := rule]

eligible <- which(x$classification == "duplicate" & x$rule %in% weak_auto_rules)
progress("checking structured title identifiers", 0L, length(eligible))

for (n in seq_along(eligible)) {
  i <- eligible[[n]]
  z <- identifier_conflict(x$title_i[[i]], x$title_j[[i]])
  if (isTRUE(z$conflict)) {
    x$identifier_conflict[[i]] <- TRUE
    x$identifier_conflict_reason[[i]] <- z$reasons
    x$rescored_classification[[i]] <- "review"
    x$rescored_rule[[i]] <- "structured_title_identifier_conflict"
  }
  if (n == 1L || n %% 25L == 0L || n == length(eligible)) {
    progress("structured title identifiers checked", n, length(eligible))
    checkpoint("identifier_check", n, length(eligible), list(changed_so_far = sum(x$identifier_conflict)))
  }
}

# Join pair metadata once rather than performing two keyed table lookups for
# every candidate pair. This changes execution only, not any decision rule.
mi <- meta[x$record_i]
mj <- meta[x$record_j]
first_author_i <- vapply(mi$author_norm, first_author_from_norm, character(1))
first_author_j <- vapply(mj$author_norm, first_author_from_norm, character(1))

x[, abstract_missing_i := is.na(mi$abstract_hash) | !nzchar(mi$abstract_hash)]
x[, abstract_missing_j := is.na(mj$abstract_hash) | !nzchar(mj$abstract_hash)]
x[, first_author_match_vec := !is.na(first_author_i) & !is.na(first_author_j) & first_author_i == first_author_j]
x[, exact_author_match_vec := !is.na(mi$author_norm) & !is.na(mj$author_norm) &
     nzchar(mi$author_norm) & nzchar(mj$author_norm) & mi$author_norm == mj$author_norm]
x[, journal_match_vec := !is.na(mi$journal_norm) & !is.na(mj$journal_norm) &
     nzchar(mi$journal_norm) & nzchar(mj$journal_norm) & mi$journal_norm == mj$journal_norm]
x[, journal_containment_vec := mapply(journal_contains, mi$journal_norm, mj$journal_norm, USE.NAMES=FALSE)]
x[, year_diff_vec := fifelse(!is.na(mi$year) & !is.na(mj$year), abs(mi$year - mj$year), NA_integer_)]
x[, doi_i_present_vec := !is.na(mi$doi_norm) & nzchar(mi$doi_norm)]
x[, doi_j_present_vec := !is.na(mj$doi_norm) & nzchar(mj$doi_norm)]
x[, preprint_i_vec := mapply(is_preprint_manifestation, mi$doi_norm, mi$journal_norm, USE.NAMES=FALSE)]
x[, preprint_j_vec := mapply(is_preprint_manifestation, mj$doi_norm, mj$journal_norm, USE.NAMES=FALSE)]

# Structured-title conflicts only influence the method where a later promotion
# could otherwise fire. Build a deliberately broad superset of those rows.
title_len_i <- vapply(x$title_i, title_length_norm, integer(1))
title_len_j <- vapply(x$title_j, title_length_norm, integer(1))
tlen_vec <- pmin(title_len_i, title_len_j)
one_missing_vec <- xor(x$abstract_missing_i, x$abstract_missing_j)
preprint_pair_vec <- x$preprint_i_vec | x$preprint_j_vec
yd_ok_1_vec <- is.na(x$year_diff_vec) | x$year_diff_vec <= 1L
yd_ok_2_vec <- is.na(x$year_diff_vec) | x$year_diff_vec <= 2L

# Optional fields are absent in some saved scored artefacts. In the reference
# row-wise implementation those branches evaluate as non-firing; represent that
# explicitly here so a length-zero vector cannot collapse the whole mask.
strong_abs_vec <- if ("strong_abstract" %in% names(x)) x$strong_abstract %in% TRUE else rep(FALSE, nrow(x))
same_family_vec <- if ("same_doi_family" %in% names(x)) x$same_doi_family %in% TRUE else rep(FALSE, nrow(x))

promotion_candidate <- (
  (x$title_containment %in% TRUE & tlen_vec >= 30L) |
  (one_missing_vec & x$exact_title %in% TRUE & tlen_vec >= 30L & yd_ok_1_vec) |
  (x$exact_abstract %in% TRUE & tlen_vec >= 30L & yd_ok_1_vec & !is.na(x$title_similarity) & x$title_similarity >= 0.95) |
  (strong_abs_vec & tlen_vec >= 30L) |
  (preprint_pair_vec & x$exact_title %in% TRUE & tlen_vec >= 20L & yd_ok_2_vec) |
  (preprint_pair_vec & !is.na(x$ordered_coverage) & x$ordered_coverage >= 0.98 &
     !is.na(x$shingle_containment) & x$shingle_containment >= 0.90) |
  (x$first_author_match_vec & !is.na(x$title_similarity) & x$title_similarity >= 0.93) |
  (x$exact_title %in% TRUE & tlen_vec >= 10L & x$first_author_match_vec & x$journal_match_vec) |
  (x$exact_title %in% TRUE & tlen_vec >= 20L & !x$doi_i_present_vec & !x$doi_j_present_vec &
     !is.na(x$year_diff_vec) & x$year_diff_vec == 0L & x$journal_containment_vec) |
  (!(x$exact_title %in% TRUE) & !is.na(x$title_similarity) & x$title_similarity >= 0.995 &
     tlen_vec >= 30L & x$first_author_match_vec) |
  (same_family_vec & !is.na(x$title_similarity) & x$title_similarity >= 0.97 & tlen_vec >= 30L)
)
conflict_candidates <- unique(c(eligible, which(promotion_candidate)))
progress("checking promotion-relevant structured title identifiers", 0L, length(conflict_candidates))
for (n in seq_along(conflict_candidates)) {
  i <- conflict_candidates[[n]]
  if (!isTRUE(x$identifier_conflict[[i]])) {
    z <- identifier_conflict(x$title_i[[i]], x$title_j[[i]])
    if (isTRUE(z$conflict)) {
      x$identifier_conflict[[i]] <- TRUE
      x$identifier_conflict_reason[[i]] <- z$reasons
    }
  }
  if (n == 1L || n %% 1000L == 0L || n == length(conflict_candidates)) {
    progress("promotion-relevant structured title identifiers checked", n, length(conflict_candidates))
    checkpoint("promotion_identifier_check", n, length(conflict_candidates),
               list(changed_so_far=sum(x$identifier_conflict)))
  }
}

# Second-stage high-confidence promotions learned from the human-reviewed queue.
# Human labels are never used to make decisions; they are used only below for evaluation.
#
# IMPORTANT: the original implementation applied these rules row-by-row across the
# complete scored candidate table. That was semantically correct but prohibitively
# slow at living-update scale because repeated single-cell writes caused excessive
# copying. All rule inputs are already precomputed above, so apply the exact same
# ordered rule set vectorially. Rule precedence and identifier-conflict overrides
# are preserved.
x[, promotion_reason := ""]
x[, one_abstract_missing := FALSE]
x[, first_author_match := FALSE]
x[, journal_match := FALSE]
x[, preprint_pair := FALSE]

non_conflict <- !x$identifier_conflict
non_conflict_idx <- which(non_conflict)
if (length(non_conflict_idx)) {
  # Use explicit integer row indices to preserve the old row-wise assignment
  # semantics exactly, including FALSE values when source metadata are missing.
  set(x, i = non_conflict_idx, j = "one_abstract_missing",
      value = one_missing_vec[non_conflict_idx])
  set(x, i = non_conflict_idx, j = "first_author_match",
      value = x$first_author_match_vec[non_conflict_idx])
  set(x, i = non_conflict_idx, j = "journal_match",
      value = x$journal_match_vec[non_conflict_idx])
  set(x, i = non_conflict_idx, j = "preprint_pair",
      value = preprint_pair_vec[non_conflict_idx])
}

# Narrow identifier-conflict overrides are evaluated first, exactly as in the
# original row-wise implementation. The second rule only applies where the first
# did not fire.
conflict_idx <- which(x$identifier_conflict)
if (length(conflict_idx)) {
  one_doi_present_vec <- xor(x$doi_i_present_vec, x$doi_j_present_vec)
  both_doi_present_vec <- x$doi_i_present_vec & x$doi_j_present_vec
  exact_pub_year_vec <- !is.na(x$year_diff_vec) & x$year_diff_vec == 0L

  m1 <- x$identifier_conflict &
    one_doi_present_vec &
    x$exact_author_match_vec &
    exact_pub_year_vec &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.99

  if (any(m1)) {
    x[m1, `:=`(
      rescored_classification = "duplicate",
      rescored_rule = "identifier_conflict_single_doi_exact_author_year",
      promotion_reason = "identifier_conflict_single_doi_exact_author_year"
    )]
  }

  m2 <- x$identifier_conflict &
    !m1 &
    both_doi_present_vec &
    x$exact_author_match_vec &
    exact_pub_year_vec &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.98 &
    !is.na(x$ordered_coverage) &
    x$ordered_coverage >= 0.98 &
    !is.na(x$shingle_containment) &
    x$shingle_containment >= 0.90

  if (any(m2)) {
    x[m2, `:=`(
      rescored_classification = "duplicate",
      rescored_rule = "identifier_conflict_strong_content_exact_author_year",
      promotion_reason = "identifier_conflict_strong_content_exact_author_year"
    )]
  }
}

# Non-conflict promotion rules are first-match-wins. Rows already classified as
# duplicate are intentionally left unchanged, matching the original final guard
# `if (promote && rescored_classification != "duplicate")`.
eligible_for_promotion <- non_conflict & x$rescored_classification != "duplicate"

generic_i_vec <- vapply(x$title_i, is_generic_exact_title, logical(1))
generic_j_vec <- vapply(x$title_j, is_generic_exact_title, logical(1))

apply_promotion <- function(mask, reason) {
  fire <- eligible_for_promotion & x$promotion_reason == "" & mask
  if (any(fire)) {
    x[fire, `:=`(
      rescored_classification = "duplicate",
      rescored_rule = reason,
      promotion_reason = reason
    )]
  }
  invisible(NULL)
}

apply_promotion(
  x$title_containment %in% TRUE &
    tlen_vec >= 30L,
  "long_title_containment_no_identifier_conflict"
)

apply_promotion(
  one_missing_vec &
    x$exact_title %in% TRUE &
    tlen_vec >= 30L &
    yd_ok_1_vec &
    (x$first_author_match_vec | x$journal_match_vec | preprint_pair_vec),
  "exact_title_missing_abstract_corroborated"
)

apply_promotion(
  x$exact_abstract %in% TRUE &
    tlen_vec >= 30L &
    yd_ok_1_vec &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.95,
  "exact_abstract_high_title_agreement"
)

apply_promotion(
  strong_abs_vec &
    tlen_vec >= 30L &
    (
      x$exact_title %in% TRUE |
      x$title_containment %in% TRUE |
      (!is.na(x$title_similarity) & x$title_similarity >= 0.97)
    ),
  "strong_abstract_high_title_agreement"
)

apply_promotion(
  preprint_pair_vec &
    x$exact_title %in% TRUE &
    tlen_vec >= 20L &
    yd_ok_2_vec,
  "preprint_exact_title_compatible_year"
)

apply_promotion(
  preprint_pair_vec &
    !is.na(x$ordered_coverage) &
    x$ordered_coverage >= 0.98 &
    !is.na(x$shingle_containment) &
    x$shingle_containment >= 0.90,
  "preprint_strong_content_manifestation"
)

apply_promotion(
  x$first_author_match_vec &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.93 &
    !is.na(x$ordered_coverage) &
    x$ordered_coverage >= 0.92 &
    !is.na(x$shingle_containment) &
    x$shingle_containment >= 0.69,
  "strong_content_first_author_title_agreement"
)

apply_promotion(
  x$exact_title %in% TRUE &
    tlen_vec >= 10L &
    x$first_author_match_vec &
    x$journal_match_vec &
    !generic_i_vec &
    !generic_j_vec,
  "exact_short_title_author_journal"
)

apply_promotion(
  x$exact_title %in% TRUE &
    tlen_vec >= 20L &
    !x$doi_i_present_vec &
    !x$doi_j_present_vec &
    !is.na(x$year_diff_vec) &
    x$year_diff_vec == 0L &
    x$journal_containment_vec &
    !generic_i_vec &
    !generic_j_vec,
  "exact_title_year_journal_containment_no_doi"
)

apply_promotion(
  !(x$exact_title %in% TRUE) &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.995 &
    tlen_vec >= 30L &
    x$first_author_match_vec,
  "near_exact_title_first_author"
)

apply_promotion(
  same_family_vec &
    !is.na(x$title_similarity) &
    x$title_similarity >= 0.97 &
    tlen_vec >= 30L,
  "doi_family_high_title_agreement"
)

progress(
  "promotion and rescore rules applied",
  nrow(x),
  nrow(x),
  paste("promotions:", sum(nzchar(x$promotion_reason)))
)
checkpoint(
  "promotion_rescore",
  nrow(x),
  nrow(x),
  list(duplicates_so_far = sum(x$rescored_classification == "duplicate", na.rm = TRUE))
)

# Candidate generation is deliberately broad. Deduplication routing depends
# only on duplicate-identity evidence. Relevance/exclusion status is not used here.
x[, review_route := fifelse(
  rescored_classification == "duplicate", "automatic_duplicate",
  fifelse(rescored_classification == "review", "manual_review", "non_duplicate")
)]
x[, manual_review_needed := review_route == "manual_review"]

x[, decision_changed := classification != rescored_classification | rule != rescored_rule]

fwrite(x, file.path(output_dir, "rescored_sample.csv"))
fwrite(x[decision_changed == TRUE], file.path(output_dir, "changed_decisions.csv"))
fwrite(
  x[classification == "duplicate" & rule %in% weak_auto_rules,
    .(record_i, record_j, title_i, title_j, original_rule = rule,
      identifier_conflict, identifier_conflict_reason,
      rescored_classification, rescored_rule)],
  file.path(output_dir, "weak_rule_audit.csv")
)

changed_by_rule <- x[decision_changed == TRUE, .N, by = rule][order(rule)]
remaining_auto_by_rule <- x[rescored_classification == "duplicate", .N, by = rescored_rule][order(rescored_rule)]


x[, pair_key := paste(pmin(record_i, record_j), pmax(record_i, record_j), sep = "::")]
eval <- merge(
  labels,
  x[, .(pair_key, classification, rule, rescored_classification, rescored_rule,
        decision_changed, identifier_conflict, one_abstract_missing,
        first_author_match, journal_match, preprint_pair,
        review_route, manual_review_needed)],
  by = "pair_key",
  all.x = TRUE,
  sort = FALSE
)
eval[, labelled_pair_found := !is.na(rescored_classification)]
eval[, predicted_duplicate := rescored_classification == "duplicate"]
eval[, truth_duplicate := truth == "duplicate"]
eval[, outcome := fifelse(!labelled_pair_found, "not_in_saved_sample",
                   fifelse(truth_duplicate & predicted_duplicate, "TP",
                   fifelse(!truth_duplicate & predicted_duplicate, "FP",
                   fifelse(truth_duplicate & !predicted_duplicate, "FN", "TN"))))]
fwrite(eval, file.path(output_dir, "human_validation_results.csv"))

tp <- eval[outcome == "TP", .N]
fp <- eval[outcome == "FP", .N]
fn <- eval[outcome == "FN", .N]
tn <- eval[outcome == "TN", .N]
precision <- if ((tp + fp) > 0L) tp / (tp + fp) else NA_real_
recall <- if ((tp + fn) > 0L) tp / (tp + fn) else NA_real_
specificity <- if ((tn + fp) > 0L) tn / (tn + fp) else NA_real_
validation_summary <- list(
  labelled_pairs = nrow(labels),
  labelled_pairs_found = sum(eval$labelled_pair_found),
  tp = tp, fp = fp, fn = fn, tn = tn,
  precision = precision,
  recall = recall,
  specificity = specificity,
  changed_labelled_pairs = sum(eval$decision_changed %in% TRUE, na.rm = TRUE)
)
progress("human validation scored", validation_summary$labelled_pairs_found, validation_summary$labelled_pairs,
         paste("TP", tp, "FP", fp, "FN", fn, "TN", tn))

summary <- list(
  workflow = "01_deduplication_identifier_rescore",
  status = "success",
  source_benchmark_run_id = source_run_id,
  sample_n = nrow(x),
  source_full_n = full_n,
  start_row = start_row,
  end_row = end_row,
  weak_auto_pairs_checked = length(eligible),
  decisions_changed_to_review = sum(x$decision_changed),
  original_classification_counts = as.list(table(x$classification)),
  rescored_classification_counts = as.list(table(x$rescored_classification)),
  changed_by_original_rule = if (nrow(changed_by_rule)) split(changed_by_rule$N, changed_by_rule$rule) else list(),
  remaining_automatic_by_rule = if (nrow(remaining_auto_by_rule)) split(remaining_auto_by_rule$N, remaining_auto_by_rule$rescored_rule) else list(),
  historic_human_adjudications_loaded = FALSE,
  candidate_generation_repeated = FALSE,
  source_candidate_artifact_reused = TRUE
)
writeLines(
  toJSON(summary, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  file.path(output_dir, "summary.json")
)
progress("rescore complete", nrow(x), nrow(x), paste("decisions changed:", sum(x$decision_changed)))
checkpoint("complete", nrow(x), nrow(x), list(status = "success", decisions_changed = sum(x$decision_changed)))
cat(toJSON(summary, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), "\n")
