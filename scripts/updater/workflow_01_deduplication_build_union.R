#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
old_dir <- arg("--old-dir")
expected_prior <- suppressWarnings(as.integer(arg("--expected-prior-manifestations",NA_character_)))
output_dir <- arg("--output-dir")

input_manifest_path <- arg("--input-manifest",NULL)
input_pos <- which(args == "--input")
inputs <- if (length(input_pos)) vapply(input_pos,function(i) {
  if (i == length(args)) stop("Missing value after --input",call.=FALSE)
  args[[i+1L]]
},character(1)) else character()

if (!is.null(input_manifest_path)) {
  if (!file.exists(input_manifest_path)) stop(sprintf("Input manifest not found: %s",input_manifest_path),call.=FALSE)
  im <- fromJSON(input_manifest_path,simplifyVector=FALSE)
  if (!identical(im$schema,"living-evidence-map-workflow01-source-inputs-v1")) stop("Unsupported source-input manifest schema",call.=FALSE)
  if (is.null(im$sources) || !length(im$sources) || is.null(names(im$sources))) stop("Source-input manifest contains no sources",call.=FALSE)
  current_paths <- vapply(im$sources,function(z)as.character(z[[1L]]),character(1))
} else if (length(inputs)) {
  if (any(!grepl("^[^=]+=",inputs))) stop("Each --input must be source=path",call.=FALSE)
  source_names <- sub("=.*$","",inputs)
  current_paths <- sub("^[^=]+=","",inputs)
  names(current_paths) <- source_names
  if (anyDuplicated(source_names)) stop("Each source may be supplied only once",call.=FALSE)
} else {
  current_paths <- c(
    lens=arg("--current-lens"),
    scopus=arg("--current-scopus"),
    openalex=arg("--current-openalex"),
    agricola=arg("--current-agricola"),
    wos=arg("--current-wos")
  )
  if (any(vapply(current_paths,is.null,logical(1)))) {
    stop("Provide --input-manifest, repeated --input source=path arguments, or all legacy --current-* source paths",call.=FALSE)
  }
}

if (is.null(old_dir) || is.null(output_dir)) stop("Required: --old-dir --output-dir",call.=FALSE)
if (!length(current_paths)) stop("At least one current source input is required",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
scalar <- function(x) {
  if (is.null(x)||!length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
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
  src <- source_kind(r)
  if (src=="lens") {
    return(as.character((r$identity %||% list())$lens_id %||% (r$identity %||% list())$record_id %||% ""))
  }
  as.character((r$sidecar_identity %||% list())$sidecar_record_id %||% "")
}
read_lines_nonempty <- function(path) {
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x[nzchar(trimws(x))]
}
ids_for_lines <- function(lines, expected_source) {
  ids <- character(length(lines))
  for (i in seq_along(lines)) {
    r <- fromJSON(lines[[i]],simplifyVector=FALSE)
    src <- source_kind(r)
    if (!identical(src,expected_source)) stop(sprintf("Expected %s but found %s at line %d",expected_source,src,i),call.=FALSE)
    rid <- source_record_id(r)
    if (!nzchar(rid)) stop(sprintf("%s line %d lacks source ID",expected_source,i),call.=FALSE)
    ids[[i]] <- rid
  }
  ids
}

legacy_required_sources <- c("lens","scopus","openalex","agricola","wos")

summary_rows <- list()
old_total <- 0L
appended_total <- 0L

for (src in names(current_paths)) {
  old_lines <- character()
  old_ids <- character()
  old_filename <- paste0(src,"_records_for_deduplication.jsonl")
  hits <- list.files(old_dir,pattern=paste0("^",old_filename,"$"),recursive=TRUE,full.names=TRUE)
  if (length(hits)>1L) stop(sprintf("Expected at most one preserved %s file, found %d",src,length(hits)),call.=FALSE)
  if (length(hits)==1L) {
    old_lines <- read_lines_nonempty(hits[[1L]])
    old_ids <- ids_for_lines(old_lines,src)
    if (anyDuplicated(old_ids)) stop(sprintf("Duplicate preserved %s source IDs",src),call.=FALSE)
  } else if (src %in% legacy_required_sources) {
    stop(sprintf("Required preserved source file missing for legacy source %s",src),call.=FALSE)
  }

  cur_lines <- read_lines_nonempty(current_paths[[src]])
  cur_ids <- ids_for_lines(cur_lines,src)
  if (anyDuplicated(cur_ids)) stop(sprintf("Duplicate current %s source IDs",src),call.=FALSE)

  is_new <- !(cur_ids %in% old_ids)
  append_lines <- cur_lines[is_new]
  append_ids <- cur_ids[is_new]

  out_path <- file.path(output_dir,paste0(src,"_records_for_deduplication.jsonl"))
  con <- file(out_path,"wt",encoding="UTF-8")
  if (length(old_lines)) writeLines(old_lines,con,useBytes=TRUE)
  if (length(append_lines)) writeLines(append_lines,con,useBytes=TRUE)
  close(con)

  final_ids <- c(old_ids,append_ids)
  if (anyDuplicated(final_ids)) stop(sprintf("Union %s IDs are not unique",src),call.=FALSE)

  summary_rows[[length(summary_rows)+1L]] <- data.frame(
    source=src,
    preserved_old=length(old_ids),
    current_harvest=length(cur_ids),
    appended_new=length(append_ids),
    current_already_present=sum(!is_new),
    union_total=length(final_ids),
    stringsAsFactors=FALSE
  )
  old_total <- old_total + length(old_ids)
  appended_total <- appended_total + length(append_ids)
}

s <- do.call(rbind,summary_rows)
if (!is.na(expected_prior) && old_total != expected_prior) stop(sprintf("Preserved corpus should contain %d manifestations, found %d",expected_prior,old_total),call.=FALSE)
write.csv(s,file.path(output_dir,"union_source_counts.csv"),row.names=FALSE)

union_inputs <- setNames(
  lapply(s$source,function(src)file.path(output_dir,paste0(src,"_records_for_deduplication.jsonl"))),
  s$source
)
writeLines(toJSON(list(
  schema="living-evidence-map-workflow01-source-inputs-v1",
  status="validated_union",
  sources=union_inputs
),auto_unbox=TRUE,pretty=TRUE,null="null"),
file.path(output_dir,"source_inputs.json"))

writeLines(toJSON(list(
  workflow="01_deduplication_incremental_union",
  status="success",
  preserved_prior_manifestations=old_total,
  appended_new_manifestations=appended_total,
  union_manifestations=old_total+appended_total,
  source_counts=split(s,s$source)
),auto_unbox=TRUE,pretty=TRUE,null="null"),
file.path(output_dir,"union_manifest.json"))
cat(sprintf("PASS: preserved %d prior manifestations and appended %d new manifestations; union=%d\n",
            old_total,appended_total,old_total+appended_total))
