#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

source <- arg("--source")
current_root <- arg("--current-root")
history_root <- arg("--history-root")
output_dir <- arg("--output-dir")
if (is.null(source)||is.null(current_root)||is.null(history_root)||is.null(output_dir)) {
  stop("--source, --current-root, --history-root and --output-dir are required",call.=FALSE)
}
if (!(source %in% c("lens","scopus","openalex","agricola","pubmed","ethos","cba","epmc_preprints","wos"))) stop("Unsupported source",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
scalar <- function(x,default="") {
  if (is.null(x)||length(x)==0L) return(default)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) default else y
}
normalise_openalex <- function(x) sub("^https?://openalex\\.org/","",trimws(as.character(x)),ignore.case=TRUE)

extract_from_json_file <- function(path, source) {
  x <- fromJSON(path,simplifyVector=FALSE)
  out <- list()
  if (source=="scopus") {
    rows <- x[["search-results"]][["entry"]] %||% list()
    for (r in rows) {
      id <- scalar(r[["eid"]])
      if (!nzchar(id)) stop(sprintf("Missing Scopus EID in %s",path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=id,raw=r)
    }
  } else if (source=="openalex") {
    rows <- x[["results"]] %||% list()
    for (r in rows) {
      id <- normalise_openalex(scalar(r[["id"]]))
      if (!nzchar(id)) stop(sprintf("Missing OpenAlex Work ID in %s",path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=id,raw=r)
    }
  } else if (source=="agricola") {
    rows <- x[["resultList"]][["result"]] %||% list()
    for (r in rows) {
      sid <- scalar(r[["id"]]); src <- scalar(r[["source"]])
      if (!nzchar(sid)||!nzchar(src)) stop(sprintf("Missing AGRICOLA Europe PMC source/id in %s",path),call.=FALSE)
      if (src!="AGR") stop(sprintf("Non-AGRICOLA source %s found in %s",src,path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=paste(src,sid,sep=":"),raw=r)
    }
  } else if (source=="wos") {
    rows <- x[["hits"]] %||% list()
    for (r in rows) {
      id <- scalar(r[["uid"]])
      if (!nzchar(id)) stop(sprintf("Missing WoS UID in %s",path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=id,raw=r)
    }
  }
  out
}

extract_lens_jsonl <- function(path) {
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  out <- vector("list",length(lines))
  for (i in seq_along(lines)) {
    r <- fromJSON(lines[[i]],simplifyVector=FALSE)
    id <- scalar(r$identity$lens_id %||% r$canonical$lens_id %||% r$lens_id)
    if (!nzchar(id)) stop(sprintf("Missing Lens ID in %s line %d",path,i),call.=FALSE)
    raw <- r$lens$raw_payload %||% r
    out[[i]] <- list(id=id,raw=raw)
  }
  out
}

extract_records <- function(root,source) {
  if (!dir.exists(root)) return(list())
  if (source=="lens") {
    fs <- list.files(root,pattern="records\\.jsonl$",recursive=TRUE,full.names=TRUE)
    fs <- fs[!grepl("new_records\\.jsonl$",fs)]
    if (!length(fs)) return(list())
    out <- list()
    for (f in fs) out <- c(out,extract_lens_jsonl(f))
    return(out)
  }
  fs <- list.files(root,pattern="^response_[0-9]{6}\\.json$",recursive=TRUE,full.names=TRUE)
  if (!length(fs)) return(list())
  out <- list()
  for (f in fs) out <- c(out,extract_from_json_file(f,source))
  out
}

current <- extract_records(current_root,source)
if (!length(current)) stop(sprintf("No current %s expansion records found",source),call.=FALSE)
current_ids <- vapply(current,`[[`,character(1),"id")
if (anyDuplicated(current_ids)) stop(sprintf("Current %s expansion harvest contains duplicate native IDs",source),call.=FALSE)

history <- extract_records(history_root,source)
history_ids <- if(length(history)) unique(vapply(history,`[[`,character(1),"id")) else character()
registry_files <- list.files(history_root,pattern="native_ids\\.txt$",recursive=TRUE,full.names=TRUE)
registry_ids <- character()
if (length(registry_files)) {
  registry_ids <- unique(unlist(lapply(registry_files,function(p) {
    x <- trimws(readLines(p,warn=FALSE,encoding="UTF-8"))
    x[nzchar(x)]
  }),use.names=FALSE))
}
history_ids <- unique(c(history_ids,registry_ids))

known <- current_ids %in% history_ids
new <- !known

writeLines(current_ids[new],file.path(output_dir,"new_native_ids.txt"))
writeLines(current_ids[known],file.path(output_dir,"already_known_native_ids.txt"))
writeLines(sort(unique(c(history_ids,current_ids))),file.path(output_dir,"updated_native_ids.txt"))

con <- file(file.path(output_dir,"new_records.jsonl"),"wt",encoding="UTF-8")
for (r in current[new]) {
  writeLines(toJSON(list(source=source,native_id=r$id,raw_payload=r$raw),
                    auto_unbox=TRUE,null="null",na="null",digits=NA),con)
}
close(con)

manifest <- list(
  workflow="00_expansion_native_id_reconciliation",
  status="success",
  reconciled_at=now_utc(),
  source=source,
  reconciliation_key=switch(source,
    lens="Lens ID",
    scopus="Scopus EID",
    openalex="OpenAlex Work ID",
    agricola="Europe PMC AGR source + ID",
    pubmed="Europe PMC MED source + ID",
    ethos="Europe PMC ETH source + ID",
    cba="Europe PMC CBA source + ID",
    epmc_preprints="Europe PMC PPR source + ID",
    wos="Web of Science UID"
  ),
  current_expansion_records=length(current_ids),
  historical_unique_native_ids=length(history_ids),
  already_known_native_ids=sum(known),
  new_native_ids=sum(new),
  records_passed_downstream=sum(new),
  bibliographic_deduplication_performed=FALSE,
  doi_matching_performed=FALSE,
  fuzzy_matching_performed=FALSE,
  downstream_deduplication="Workflow 01"
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"reconciliation_manifest.json"))
message(sprintf("PASS: %s expansion reconciliation: retrieved=%d known=%d new=%d",
                source,length(current_ids),sum(known),sum(new)))
