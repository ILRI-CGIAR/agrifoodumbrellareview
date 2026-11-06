#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(digest)
  library(stringdist)
  library(jsonlite)
  library(stringi)
  library(xml2)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

input_dir <- arg("--input-dir")
upstream_dir <- arg("--upstream-dir")
output_dir <- arg("--output-dir")
target_n <- as.integer(arg("--target-n", "10000"))
sample_key <- arg("--sample-key", "workflow02-v2-expanded-audit-v1")

if (is.null(input_dir) || is.null(upstream_dir) || is.null(output_dir)) {
  stop("Required: --input-dir --upstream-dir --output-dir", call. = FALSE)
}
if (is.na(target_n) || target_n < 2000L) stop("--target-n must be >= 2000", call. = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

find_one <- function(name) {
  p <- file.path(input_dir, name)
  if (file.exists(p)) return(p)
  hits <- list.files(input_dir, pattern = paste0("^", gsub("\\.", "\\\\.", name), "$"),
                     recursive = TRUE, full.names = TRUE)
  if (!length(hits)) stop(sprintf("%s not found in saved benchmark artefact", name), call. = FALSE)
  hits[[1L]]
}

meta <- fread(find_one("normalised_metadata.csv"), na.strings = c("", "NA"))
all_pairs <- fread(find_one("all_candidate_pairs.csv"), na.strings = c("", "NA"))
if (!nrow(all_pairs)) {
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  empty <- data.table(
    record_i=integer(),record_j=integer(),source_i=character(),source_j=character(),
    title_i=character(),title_j=character(),doi_i=character(),doi_j=character(),
    blocks=character(),title_similarity=numeric(),title_containment=logical(),
    exact_title=logical(),exact_abstract=logical(),ordered_coverage=numeric(),
    shingle_containment=numeric(),lcs_tokens=integer(),classification=character(),
    rule=character(),stratum=character()
  )
  fwrite(empty,file.path(output_dir,"scored_sample.csv"))
  fwrite(meta,file.path(output_dir,"normalised_metadata.csv"))
  fwrite(data.table(record_i=integer(),record_j=integer(),blocks=character(),stratum=character()),
         file.path(output_dir,"expanded_scoring_sample.csv"))
  summary <- list(
    status="success",source_candidate_artifact_reused=TRUE,candidate_generation_repeated=FALSE,
    total_candidate_pairs=0L,original_calibration_pairs=0L,expanded_pairs=0L,
    scoring_sample_n=0L,stratum_counts=list(),base_classification_counts=list(),base_rule_counts=list()
  )
  writeLines(jsonlite::toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
             file.path(output_dir,"expanded_summary.json"))
  cat("PASS: zero incremental duplicate candidate pairs; wrote empty scored state\n")
  quit(save="no",status=0L)
}
# Incremental mode preserves prior decisions and never needs a historic
# calibration/scored-sample artefact. Keep an empty schema-compatible table.
original <- data.table(record_i=integer(),record_j=integer(),blocks=character())

scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- as.character(x[[1L]])
  if (!nzchar(trimws(y))) NULL else y
}
strip_markup_text <- function(x) {
  x <- scalar(x)
  if (is.null(x)) return(NULL)
  wrapped <- paste0("<div>", x, "</div>")
  doc <- tryCatch(
    suppressWarnings(read_html(wrapped, options=c("RECOVER","NOERROR","NOWARNING"))),
    error=function(e) NULL
  )
  if (!is.null(doc)) {
    node <- xml_find_first(doc, ".//div")
    if (!inherits(node, "xml_missing")) x <- xml_text(node)
  }
  x
}
norm_words <- function(x) {
  x <- strip_markup_text(x)
  if (is.null(x)) return(NULL)
  x <- stri_trans_tolower(stri_trans_nfkc(x))
  x <- stri_replace_all_regex(x, "[\\p{P}\\p{S}\\p{Z}\\s]+", " ")
  x <- trimws(stri_replace_all_regex(x, "\\s+", " "))
  if (!nzchar(x)) NULL else x
}
source_kind <- function(r) {
  if (is.list(r$lens)) return("lens")
  p <- scalar((r$source %||% list())$provider)
  if (is.null(p)) stop("Source provider is missing",call.=FALSE)
  if (identical(p,"agricola_via_europe_pmc")) return("agricola")
  if (identical(p,"wos_starter")) return("wos")
  if (!grepl("^[a-z0-9][a-z0-9_-]*$",p)) stop(sprintf("Invalid source provider slug: %s",p),call.=FALSE)
  p
}
source_record_id <- function(r) {
  if (source_kind(r)=="lens") return(as.character((r$identity %||% list())$lens_id %||% (r$identity %||% list())$record_id %||% ""))
  as.character((r$sidecar_identity %||% list())$sidecar_record_id %||% "")
}
record_abstract <- function(r) {
  if (source_kind(r)=="lens") return(scalar((r$canonical %||% list())$abstract %||% ((r$lens %||% list())$raw_payload %||% list())$abstract))
  scalar((r$mapped_fields %||% list())$abstract)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

read_abstract_map <- function(path) {
  con <- file(path, "rt", encoding="UTF-8")
  on.exit(close(con), add=TRUE)
  rows <- list()
  repeat {
    lines <- readLines(con, n=500L, warn=FALSE)
    if (!length(lines)) break
    for (line in lines) {
      if (!nzchar(trimws(line))) next
      r <- fromJSON(line, simplifyVector=FALSE)
      src <- source_kind(r)
      rid <- source_record_id(r)
      abs <- norm_words(record_abstract(r))
      rows[[length(rows)+1L]] <- data.table(
        source=src, source_record_id=rid,
        abstract_norm=if (is.null(abs)) NA_character_ else abs
      )
    }
  }
  rbindlist(rows, use.names=TRUE, fill=TRUE)
}

source_manifest_path <- file.path(upstream_dir,"source_inputs.json")
if (file.exists(source_manifest_path)) {
  sm <- fromJSON(source_manifest_path,simplifyVector=FALSE)
  if (!identical(sm$schema,"living-evidence-map-workflow01-source-inputs-v1")) {
    stop("Unsupported upstream source-input manifest schema",call.=FALSE)
  }
  if (is.null(sm$sources) || !length(sm$sources) || is.null(names(sm$sources))) {
    stop("Upstream source-input manifest contains no sources",call.=FALSE)
  }
  upstream_paths <- vapply(sm$sources,function(z)as.character(z[[1L]]),character(1))
} else {
  upstream_paths <- list.files(
    upstream_dir,
    pattern="_records_for_deduplication\\.jsonl$",
    full.names=TRUE
  )
  names(upstream_paths) <- sub("_records_for_deduplication\\.jsonl$","",basename(upstream_paths))
}
if (!length(upstream_paths)) stop("No upstream source files found",call.=FALSE)
if (any(!file.exists(upstream_paths))) {
  stop(sprintf("Upstream source file(s) missing: %s",paste(names(upstream_paths)[!file.exists(upstream_paths)],collapse=", ")),call.=FALSE)
}
abs_maps <- lapply(upstream_paths, read_abstract_map)
abs_map <- unique(rbindlist(abs_maps, use.names=TRUE, fill=TRUE),
                  by=c("source","source_record_id"))
setkey(abs_map, source, source_record_id)
meta <- abs_map[meta, on=.(source,source_record_id)]
if ("i.abstract_norm" %in% names(meta)) {
  if ("abstract_norm" %in% names(meta)) {
    meta[, abstract_norm := fifelse(!is.na(abstract_norm), abstract_norm, i.abstract_norm)]
    meta[, i.abstract_norm := NULL]
  } else {
    setnames(meta, "i.abstract_norm", "abstract_norm")
  }
}

stopifnot(all(c("idx","title","title_norm","doi_norm","doi_family","abstract_hash",
                "author_norm","year","abstract_norm") %in% names(meta)))
stopifnot(all(c("record_i","record_j","blocks") %in% names(all_pairs)))

pair_key <- function(i,j) paste(pmin(i,j), pmax(i,j), sep="::")
original[, pair_key := pair_key(record_i, record_j)]
all_pairs[, pair_key := pair_key(record_i, record_j)]
all_pairs <- all_pairs[!duplicated(pair_key)]

# Retain every original calibration pair, then add deterministic stratified new pairs.
need <- max(0L, target_n - nrow(original))
pool <- all_pairs[!pair_key %in% original$pair_key]
pool[, primary_block := tstrsplit(blocks, ";", fixed = TRUE, keep = 1L)]
pool[, sample_hash := vapply(seq_len(.N), function(i)
  digest(paste(sample_key, record_i[[i]], record_j[[i]], sep="|"),
         algo="sha256", serialize=FALSE), character(1))]

if (need > 0L) {
  nb <- uniqueN(pool$primary_block)
  per_block <- max(50L, ceiling(need / max(1L, nb)))
  setorder(pool, primary_block, sample_hash)
  extra <- pool[, head(.SD, per_block), by=primary_block]

  # Some strata can contain fewer than the nominal per-block quota.
  # Top up deterministically from the remaining candidate pool so the
  # expanded audit reaches the requested size without changing provenance
  # or regenerating candidates.
  if (nrow(extra) < need) {
    selected_keys <- pair_key(extra$record_i, extra$record_j)
    remainder <- pool[!pair_key(record_i, record_j) %in% selected_keys]
    setorder(remainder, sample_hash)
    topup_n <- min(need - nrow(extra), nrow(remainder))
    if (topup_n > 0L) extra <- rbind(extra, remainder[seq_len(topup_n)], fill=TRUE)
  }

  if (nrow(extra) > need) {
    setorder(extra, sample_hash)
    extra <- extra[seq_len(need)]
  }
} else {
  extra <- pool[0]
}

score_sample <- rbind(
  original[, .(record_i, record_j, blocks, pair_key, stratum="original_calibration")],
  extra[, .(record_i, record_j, blocks, pair_key, stratum="expanded_stratified")],
  fill=TRUE
)
if (nrow(score_sample) != min(target_n, nrow(all_pairs))) {
  stop(sprintf("Expanded sample size mismatch: got %d expected %d",
               nrow(score_sample), min(target_n, nrow(all_pairs))), call.=FALSE)
}

setkey(meta, idx)
tokens <- function(x) if (is.na(x) || !nzchar(x)) character() else strsplit(x, " ", fixed=TRUE)[[1L]]
shingles5 <- function(t) if (length(t)<5L) character() else unique(vapply(seq_len(length(t)-4L), function(i) paste(t[i:(i+4L)], collapse=" "), character(1)))
lcs_len <- function(a,b) {
  if (!length(a) || !length(b)) return(0L)
  if (length(a)>length(b)) {tmp<-a;a<-b;b<-tmp}
  prev <- integer(length(b)+1L); cur <- integer(length(b)+1L)
  for (i in seq_along(a)) {
    cur[] <- 0L
    for (j in seq_along(b)) cur[[j+1L]] <- if (identical(a[[i]],b[[j]])) prev[[j]]+1L else max(prev[[j+1L]],cur[[j]])
    tmp<-prev;prev<-cur;cur<-tmp
  }
  prev[[length(b)+1L]]
}

# Cache tokenisation and 5-word shingles once per record. The same abstract can
# participate in many candidate pairs, so recomputing these structures per pair
# is pure implementation overhead.
abstract_cache <- new.env(hash=TRUE, parent=emptyenv())
cached_abstract <- function(idx, abstract_norm) {
  key <- as.character(idx)
  if (exists(key, envir=abstract_cache, inherits=FALSE)) {
    return(get(key, envir=abstract_cache, inherits=FALSE))
  }
  t <- tokens(abstract_norm)
  z <- list(tokens=t, shingles=shingles5(t), n=length(t))
  assign(key, z, envir=abstract_cache)
  z
}

abstract_metrics <- function(idx_a,a,idx_b,b) {
  ca <- cached_abstract(idx_a,a); cb <- cached_abstract(idx_b,b)
  shorter <- min(ca$n,cb$n)
  if (!shorter) return(list(lcs=0L,ordered=NA_real_,shingle=NA_real_,strong=FALSE))
  sh <- if (!length(ca$shingles)||!length(cb$shingles)) NA_real_ else
    length(intersect(ca$shingles,cb$shingles))/min(length(ca$shingles),length(cb$shingles))

  # Every existing rule that uses ordered coverage/LCS also requires shingle
  # containment >= 0.60 (usually >= 0.69 or >= 0.90). Therefore shingle < 0.60
  # proves that LCS cannot affect any classification or rescore decision.
  if (is.na(sh) || sh < 0.60) {
    return(list(lcs=NA_integer_,ordered=NA_real_,shingle=sh,strong=FALSE))
  }

  l <- lcs_len(ca$tokens,cb$tokens); oc <- l/shorter
  strong <- (l>=80L && oc>=0.90 && sh>=0.60) ||
            (l>=60L&&l<=79L&&oc>=0.95&&sh>=0.75)
  list(lcs=l,ordered=oc,shingle=sh,strong=strong)
}

score_one <- function(i,j,blocks) {
  a <- meta[.(i)]; b <- meta[.(j)]
  same_doi <- !is.na(a$doi_norm)&&!is.na(b$doi_norm)&&identical(a$doi_norm,b$doi_norm)
  same_family <- !is.na(a$doi_family)&&!is.na(b$doi_family)&&identical(a$doi_family,b$doi_family)
  diff_doi <- !is.na(a$doi_norm)&&!is.na(b$doi_norm)&&!identical(a$doi_norm,b$doi_norm)
  exact_title <- !is.na(a$title_norm)&&!is.na(b$title_norm)&&identical(a$title_norm,b$title_norm)
  tsim <- if (!is.na(a$title_norm)&&!is.na(b$title_norm)) as.numeric(stringsim(a$title_norm,b$title_norm,method="jw",p=0.1)) else NA_real_
  containment <- FALSE
  if (!is.na(a$title_norm)&&!is.na(b$title_norm)) {
    short <- if(nchar(a$title_norm)<=nchar(b$title_norm)) a$title_norm else b$title_norm
    long <- if(nchar(a$title_norm)<=nchar(b$title_norm)) b$title_norm else a$title_norm
    containment <- nchar(short)>=30L && grepl(short,long,fixed=TRUE)
  }
  exact_abs <- !is.na(a$abstract_hash)&&!is.na(b$abstract_hash)&&identical(a$abstract_hash,b$abstract_hash)
  exact_author <- !is.na(a$author_norm)&&!is.na(b$author_norm)&&identical(a$author_norm,b$author_norm)
  exact_year <- !is.na(a$year)&&!is.na(b$year)&&identical(a$year,b$year)
  yd <- if (!is.na(a$year)&&!is.na(b$year)) abs(a$year-b$year) else NA_integer_
  brA <- grepl("(^|;)bramer_A(;|$)",blocks)
  brB <- grepl("(^|;)bramer_B(;|$)",blocks)

  # Preserve the reference method and rule precedence exactly. Abstract metrics
  # are still available to the downstream rescorer even when an earlier base
  # rule ultimately determines the base classification.
  need_abs <- !exact_abs && !is.na(tsim) && (tsim>=0.90||containment||exact_title)
  am <- if (need_abs && !is.na(a$abstract_norm)&&!is.na(b$abstract_norm))
          abstract_metrics(i,a$abstract_norm,j,b$abstract_norm)
        else list(lcs=NA_integer_,ordered=NA_real_,shingle=NA_real_,strong=FALSE)

  auto <- NULL
  if(brA)auto<-"bramer_A"
  else if(brB)auto<-"bramer_B"
  else if(same_doi&&exact_title)auto<-"exact_doi_exact_title"
  else if(exact_title&&exact_abs)auto<-"exact_title_exact_abstract"
  else if(same_doi&&containment)auto<-"exact_doi_title_containment"
  else if(same_doi&&!is.na(tsim)&&tsim>=0.985)auto<-"exact_doi_title_similarity_0.985"
  else if(same_doi&&exact_abs)auto<-"exact_doi_exact_abstract"
  else if(exact_title&&am$strong)auto<-"exact_title_strong_abstract"
  else if(containment&&am$strong)auto<-"title_containment_strong_abstract"
  else if(!is.na(tsim)&&tsim>=0.97&&am$strong)auto<-"title_similarity_0.97_strong_abstract"
  else if(!is.na(tsim)&&tsim>=0.95&&exact_author&&exact_year)auto<-"title_similarity_0.95_exact_author_year"

  cls <- "unresolved"; rule <- "candidate_only"
  if(!is.null(auto)) {
    if(diff_doi&&!same_family&&!grepl("^exact_doi_",auto)) {cls<-"review";rule<-"material_conflict_different_doi"}
    else {cls<-"duplicate";rule<-auto}
  } else if(exact_abs) {cls<-"review";rule<-"exact_abstract_insufficient_metadata"}
  else if(same_family&&diff_doi&&!is.na(tsim)&&tsim>=0.97) {cls<-"review";rule<-"doi_family_title_similarity_0.97"}
  else if(exact_title&&!is.na(yd)&&yd<=1L) {cls<-"review";rule<-"exact_title_year_within_1"}
  else if(am$strong&&diff_doi&&!same_family) {cls<-"review";rule<-"strong_content_material_doi_conflict"}

  data.table(record_i=i,record_j=j,source_i=a$source,source_j=b$source,
             title_i=a$title,title_j=b$title,doi_i=a$doi_norm,doi_j=b$doi_norm,
             blocks=blocks,title_similarity=tsim,title_containment=containment,
             exact_title=exact_title,exact_abstract=exact_abs,
             ordered_coverage=am$ordered,shingle_containment=am$shingle,lcs_tokens=am$lcs,
             classification=cls,rule=rule)
}

out <- vector("list", nrow(score_sample))
checkpoint_path <- file.path(output_dir, "scored_checkpoint.csv")
if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

for (k in seq_len(nrow(score_sample))) {
  out[[k]] <- score_one(score_sample$record_i[[k]], score_sample$record_j[[k]], score_sample$blocks[[k]])

  # Persist completed scoring in bounded chunks. This makes long full-corpus
  # runs recoverable from an uploaded artefact if the runner is interrupted.
  if (k %% 5000L == 0L || k == nrow(score_sample)) {
    from <- max(1L, k - 4999L)
    chunk <- rbindlist(out[from:k], fill=TRUE)
    chunk[, stratum := score_sample$stratum[from:k]]
    fwrite(chunk, checkpoint_path, append = file.exists(checkpoint_path),
           col.names = !file.exists(checkpoint_path))
  }

  if (k %% 500L == 0L || k == nrow(score_sample)) {
    cat(sprintf("Scored %d / %d pairs\n", k, nrow(score_sample)))
    flush.console()
  }
}
scored <- fread(checkpoint_path, na.strings=c("", "NA"))

# Preserve the exact original 2,000 scored rows to guarantee comparability.
orig_keys <- original$pair_key
scored[, pair_key := pair_key(record_i,record_j)]
setkey(scored,pair_key); setkey(original,pair_key)
common <- intersect(orig_keys, scored$pair_key)
if (length(common) != nrow(original)) stop("Original calibration pairs missing from expanded sample", call.=FALSE)

fwrite(scored[, !"pair_key"], file.path(output_dir,"scored_sample.csv"))
fwrite(meta, file.path(output_dir,"normalised_metadata.csv"))
fwrite(score_sample[, .(record_i,record_j,blocks,stratum)], file.path(output_dir,"expanded_scoring_sample.csv"))

summary <- list(
  status="success",
  source_candidate_artifact_reused=TRUE,
  candidate_generation_repeated=FALSE,
  total_candidate_pairs=nrow(all_pairs),
  original_calibration_pairs=nrow(original),
  expanded_pairs=nrow(extra),
  scoring_sample_n=nrow(scored),
  stratum_counts=as.list(table(scored$stratum)),
  base_classification_counts=as.list(table(scored$classification)),
  base_rule_counts=as.list(table(scored$rule))
)
writeLines(jsonlite::toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"expanded_summary.json"))
cat(jsonlite::toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),"\n")
