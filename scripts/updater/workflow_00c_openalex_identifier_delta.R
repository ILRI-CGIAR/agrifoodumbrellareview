#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag), call.=FALSE)
  args[[i+1L]]
}

current_dir <- arg("--current-dir")
previous_dir <- arg("--previous-dir","")
output_dir <- arg("--output-dir")
if (is.null(current_dir) || is.null(output_dir)) stop("--current-dir and --output-dir are required",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
short_id <- function(x) sub("^https://openalex.org/","",as.character(x))

read_works <- function(root) {
  fs <- sort(list.files(file.path(root,"raw"),pattern="^response_[0-9]{6}\\.json$",full.names=TRUE))
  if (!length(fs)) return(list())
  out <- list()
  for (f in fs) {
    x <- fromJSON(f,simplifyVector=FALSE)
    out <- c(out,x$results %||% list())
  }
  out
}

current <- read_works(current_dir)
if (!length(current)) stop("Current OpenAlex harvest contains no works",call.=FALSE)
current_ids_raw <- vapply(current,function(w) short_id(w$id %||% ""),character(1))
if (any(!nzchar(current_ids_raw))) stop("Current OpenAlex harvest contains missing work IDs",call.=FALSE)
duplicate_current_ids <- unique(current_ids_raw[duplicated(current_ids_raw)])
keep_current <- !duplicated(current_ids_raw)
current <- current[keep_current]
current_ids <- current_ids_raw[keep_current]

previous <- if (nzchar(previous_dir) && dir.exists(previous_dir)) read_works(previous_dir) else list()
previous_ids <- if (length(previous)) vapply(previous,function(w) short_id(w$id %||% ""),character(1)) else character()
previous_ids <- unique(previous_ids[nzchar(previous_ids)])

is_new <- !(current_ids %in% previous_ids)
delta <- current[is_new]

con <- file(file.path(output_dir,"openalex_delta_records.jsonl"),"wt",encoding="UTF-8")
for (w in delta) writeLines(toJSON(w,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
close(con)

writeLines(current_ids,file.path(output_dir,"current_openalex_ids.txt"))
writeLines(previous_ids,file.path(output_dir,"previous_openalex_ids.txt"))
writeLines(current_ids[is_new],file.path(output_dir,"new_openalex_ids.txt"))

summary <- list(
  status="success",
  deduplication_key="OpenAlex Work ID",
  previous_baseline_available=length(previous_ids)>0L,
  previous_unique_ids=length(previous_ids),
  current_raw_records=length(current_ids_raw),
  current_unique_ids=length(current_ids),
  exact_duplicate_ids_suppressed=length(duplicate_current_ids),
  duplicate_openalex_ids=duplicate_current_ids,
  duplicate_policy="First occurrence of an exact repeated OpenAlex Work ID is retained for delta comparison; all raw source responses remain preserved unchanged.",
  already_known_ids=sum(!is_new),
  new_ids=sum(is_new),
  delta_jsonl="openalex_delta_records.jsonl"
)
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"delta_summary.json"))
message(sprintf("PASS: OpenAlex ID delta complete; raw=%d unique=%d duplicate IDs suppressed=%d previous=%d known=%d new=%d",
                length(current_ids_raw),length(current_ids),length(duplicate_current_ids),
                length(previous_ids),sum(!is_new),sum(is_new)))
