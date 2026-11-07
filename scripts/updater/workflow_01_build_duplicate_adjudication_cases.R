#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
or_else <- function(x,y) if (is.null(x) || !length(x)) y else x
scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  z <- suppressWarnings(as.character(unlist(x,use.names=FALSE)))
  z <- z[!is.na(z)]
  if (!length(z)) return(NA_character_)
  z <- trimws(z)
  z <- z[nzchar(z)]
  if (!length(z)) NA_character_ else z[[1L]]
}
collapse_text <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  z <- unlist(x,use.names=FALSE)
  z <- as.character(z[!is.na(z)])
  z <- trimws(z)
  z <- unique(z[nzchar(z)])
  if (!length(z)) NA_character_ else paste(z,collapse="; ")
}

review_path <- arg("--review-pairs")
metadata_path <- arg("--metadata")
union_dir <- arg("--union-dir")
output_path <- arg("--output")
if (any(vapply(list(review_path,metadata_path,union_dir,output_path),is.null,logical(1)))) {
  stop("Required: --review-pairs --metadata --union-dir --output",call.=FALSE)
}

review <- fread(review_path,na.strings=c("","NA"))
meta <- fread(metadata_path,na.strings=c("","NA"))
required_review <- c("record_i","record_j","pair_key","rescored_rule","title_similarity","blocks")
required_meta <- c("idx","source","source_record_id","title","doi_norm","author_norm","year",
                   "journal_norm","volume_norm","issue_norm","pages_norm")
if (length(setdiff(required_review,names(review)))) stop("Review queue lacks required columns",call.=FALSE)
if (length(setdiff(required_meta,names(meta)))) stop("Metadata lacks required columns",call.=FALSE)
setkey(meta,idx)

wanted_idx <- sort(unique(c(review$record_i,review$record_j)))
if (any(!wanted_idx %in% meta$idx)) stop("Review queue references metadata indices that do not exist",call.=FALSE)
wanted_meta <- meta[.(wanted_idx)]
wanted_keys <- paste(wanted_meta$source,wanted_meta$source_record_id,sep="::")
names(wanted_keys) <- as.character(wanted_meta$idx)

source_record_id <- function(r, expected_source) {
  if (expected_source == "lens") {
    return(scalar(or_else((r$identity %||% list())$lens_id,
                          (r$canonical %||% list())$record_id)))
  }
  scalar((r$sidecar_identity %||% list())$sidecar_record_id)
}
`%||%` <- function(x,y) if (is.null(x)) y else x

authors_text <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  if (is.character(x)) return(collapse_text(x))
  vals <- vapply(x,function(a) {
    if (is.character(a) && length(a)==1L) return(a)
    if (!is.list(a)) return(NA_character_)
    z <- scalar(a$display_name)
    if (!is.na(z)) return(z)
    z <- scalar(a$fullName)
    if (!is.na(z)) return(z)
    first <- scalar(a$first_name); last <- scalar(a$last_name)
    nm <- trimws(paste(ifelse(is.na(first),"",first),ifelse(is.na(last),"",last)))
    if (nzchar(nm)) nm else NA_character_
  },character(1))
  collapse_text(vals)
}

# Source adapters may store raw_payload as a scalar reference rather than an
# embedded object. Only named JSON objects support field-level extraction.
field_object <- function(x) if (is.list(x) && !is.data.frame(x)) x else list()

extract_record <- function(r, expected_source, fallback) {
  mapped <- field_object(if (expected_source=="lens") r$canonical else r$mapped_fields)
  container <- field_object(if (expected_source=="lens") r$lens else r[[expected_source]])
  raw <- field_object(container[["raw_payload", exact = TRUE]])
  enrich <- field_object(r$abstract_enrichment)

  abstract_candidates <- c(
    scalar(mapped$abstract),
    scalar(enrich$abstract),
    scalar(enrich$recovered_abstract),
    scalar(enrich$abstract_text),
    scalar(enrich$replacement_abstract),
    scalar(raw$abstract),
    scalar(raw$abstractText),
    scalar(raw$description)
  )
  abstract_candidates <- abstract_candidates[!is.na(abstract_candidates) & nzchar(abstract_candidates)]
  abstract <- if (length(abstract_candidates)) abstract_candidates[[1L]] else scalar(fallback$abstract_norm)

  keywords <- mapped$keywords
  if (is.null(keywords) && expected_source=="lens") keywords <- raw$keywords
  if (is.list(keywords) && !is.character(keywords)) {
    keywords <- unlist(lapply(keywords,function(k) {
      if (is.character(k)) k else if (is.list(k)) or_else(k$keyword,or_else(k$display_name,k$name)) else NULL
    }),use.names=FALSE)
  }

  list(
    index=as.integer(fallback$idx),
    source=expected_source,
    source_record_id=as.character(fallback$source_record_id),
    title=if (!is.na(scalar(mapped$title))) scalar(mapped$title) else as.character(fallback$title),
    abstract=abstract,
    keywords=collapse_text(keywords),
    journal=if (!is.na(scalar(mapped$source))) scalar(mapped$source) else scalar(fallback$journal_norm),
    year=if (!is.na(scalar(mapped$year))) scalar(mapped$year) else as.character(fallback$year),
    authors=if (!is.na(authors_text(mapped$authors))) authors_text(mapped$authors) else scalar(fallback$author_norm),
    volume=if (!is.na(scalar(mapped$volume))) scalar(mapped$volume) else scalar(fallback$volume_norm),
    issue=if (!is.na(scalar(mapped$issue))) scalar(mapped$issue) else scalar(fallback$issue_norm),
    pages=if (!is.na(scalar(mapped$pages))) scalar(mapped$pages) else scalar(fallback$pages_norm),
    doi=if (!is.na(scalar(mapped$doi))) scalar(mapped$doi) else scalar(fallback$doi_norm)
  )
}

records <- new.env(parent=emptyenv())
for (src in sort(unique(wanted_meta$source))) {
  path <- file.path(union_dir,sprintf("%s_records_for_deduplication.jsonl",src))
  if (!file.exists(path)) stop(sprintf("Missing union JSONL for %s",src),call.=FALSE)
  needed_ids <- wanted_meta[source==src,source_record_id]
  needed_set <- setNames(rep(TRUE,length(needed_ids)),needed_ids)
  con <- file(path,"rt",encoding="UTF-8")
  on.exit(close(con),add=TRUE)
  repeat {
    lines <- readLines(con,n=1000L,warn=FALSE)
    if (!length(lines)) break
    for (line in lines) {
      if (!nzchar(trimws(line))) next
      r <- fromJSON(line,simplifyVector=FALSE)
      rid <- source_record_id(r,src)
      if (is.na(rid) || !nzchar(rid) || !(rid %in% names(needed_set))) next
      m <- wanted_meta[source==src & source_record_id==rid]
      if (nrow(m)!=1L) stop(sprintf("Metadata mapping not unique for %s::%s",src,rid),call.=FALSE)
      assign(paste(src,rid,sep="::"),extract_record(r,src,m),envir=records)
    }
  }
  close(con)
  on.exit(NULL,add=FALSE)
}

missing_keys <- wanted_keys[!vapply(wanted_keys,exists,logical(1),envir=records,inherits=FALSE)]
if (length(missing_keys)) stop(sprintf("Could not recover %d required bibliographic records",length(missing_keys)),call.=FALSE)

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
out <- file(output_path,"wt",encoding="UTF-8")
on.exit(close(out),add=TRUE)

for (i in seq_len(nrow(review))) {
  q <- review[i]
  mi <- meta[.(q$record_i)]
  mj <- meta[.(q$record_j)]
  ri <- get(paste(mi$source,mi$source_record_id,sep="::"),envir=records,inherits=FALSE)
  rj <- get(paste(mj$source,mj$source_record_id,sep="::"),envir=records,inherits=FALSE)
  identity <- function(src,id) paste0(nchar(src,type="bytes"),":",src,nchar(id,type="bytes"),":",id)
  stable <- paste(sort(c(identity(as.character(mi$source),as.character(mi$source_record_id)),
                         identity(as.character(mj$source),as.character(mj$source_record_id)))),collapse="|")
  case_id <- paste0("hr-",substr(digest(stable,algo="sha256",serialize=FALSE),1,20))
  if(any(c("source_id_i","source_record_id_i","source_id_j","source_record_id_j") %in% names(q))) {
    if(!all(c("source_id_i","source_record_id_i","source_id_j","source_record_id_j") %in% names(q)))
      stop("Review pair has incomplete immutable IDs",call.=FALSE)
    if(!identical(as.character(q$source_id_i),as.character(mi$source)) ||
       !identical(as.character(q$source_record_id_i),as.character(mi$source_record_id)) ||
       !identical(as.character(q$source_id_j),as.character(mj$source)) ||
       !identical(as.character(q$source_record_id_j),as.character(mj$source_record_id)))
      stop("Review pair source identity differs from metadata",call.=FALSE)
  }

  z <- list(
    schema="living-evidence-map-workflow01-duplicate-adjudication-case-v1",
    review_case_id=case_id,
    pair_key=as.character(q$pair_key),
    record_i=ri,
    record_j=rj,
    deterministic_evidence=list(
      blocks=as.character(q$blocks),
      title_similarity=as.numeric(q$title_similarity),
      title_containment=if ("title_containment" %in% names(q)) as.logical(q$title_containment) else NULL,
      exact_title=if ("exact_title" %in% names(q)) as.logical(q$exact_title) else NULL,
      exact_abstract=if ("exact_abstract" %in% names(q)) as.logical(q$exact_abstract) else NULL,
      ordered_coverage=if ("ordered_coverage" %in% names(q)) as.numeric(q$ordered_coverage) else NULL,
      shingle_containment=if ("shingle_containment" %in% names(q)) as.numeric(q$shingle_containment) else NULL,
      classifier_decision=as.character(q$rescored_classification),
      classifier_rule=as.character(q$rescored_rule),
      identifier_conflict=if ("identifier_conflict" %in% names(q)) as.logical(q$identifier_conflict) else NULL,
      identifier_conflict_reason=if ("identifier_conflict_reason" %in% names(q)) scalar(q$identifier_conflict_reason) else NULL,
      preprint_pair=if ("preprint_pair" %in% names(q)) as.logical(q$preprint_pair) else NULL
    )
  )
  writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),out,useBytes=TRUE)
}
close(out)
on.exit(NULL,add=FALSE)

manifest <- list(
  schema="living-evidence-map-workflow01-adjudication-input-manifest-v1",
  cases=nrow(review),
  unique_bibliographic_records=length(wanted_keys),
  source_counts=as.list(table(wanted_meta$source)),
  review_pairs_sha256=digest(file=review_path,algo="sha256",serialize=FALSE),
  metadata_sha256=digest(file=metadata_path,algo="sha256",serialize=FALSE),
  output_sha256=digest(file=output_path,algo="sha256",serialize=FALSE)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           paste0(output_path,".manifest.json"))
cat(sprintf("PASS: built %d full-evidence duplicate-adjudication cases from %d bibliographic records\n",
            nrow(review),length(wanted_keys)))
