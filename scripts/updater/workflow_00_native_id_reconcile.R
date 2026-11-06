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
history_root <- arg("--history-root","")
registry_path <- arg("--registry","")
output_dir <- arg("--output-dir")
run_type <- arg("--run-type","fortnightly")

if (is.null(source)||is.null(current_root)||is.null(output_dir)) {
  stop("--source, --current-root and --output-dir are required",call.=FALSE)
}
if (!(source %in% c("lens","scopus","openalex","agricola","pubmed","ethos","cba","epmc_preprints","wos"))) stop("Unsupported source",call.=FALSE)
if (!nzchar(registry_path) && !nzchar(history_root)) stop("Supply --registry or --history-root",call.=FALSE)
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
      out[[length(out)+1L]] <- list(id=id,record=r)
    }
  } else if (source=="openalex") {
    rows <- x[["results"]] %||% list()
    for (r in rows) {
      id <- normalise_openalex(scalar(r[["id"]]))
      if (!nzchar(id)) stop(sprintf("Missing OpenAlex Work ID in %s",path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=id,record=r)
    }
  } else if (source %in% c("agricola","pubmed","ethos","cba","epmc_preprints")) {
    expected_src <- c(agricola="AGR",pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
    rows <- x[["resultList"]][["result"]] %||% list()
    for (r in rows) {
      sid <- scalar(r[["id"]]); src <- scalar(r[["source"]])
      if (!nzchar(sid)||!nzchar(src)) stop(sprintf("Missing Europe PMC source/id for %s in %s",source,path),call.=FALSE)
      if (src!=expected_src) stop(sprintf("Unexpected Europe PMC source %s for %s in %s",src,source,path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=paste(src,sid,sep=":"),record=r)
    }
  } else if (source=="wos") {
    rows <- x[["hits"]] %||% list()
    for (r in rows) {
      id <- scalar(r[["uid"]])
      if (!nzchar(id)) stop(sprintf("Missing WoS UID in %s",path),call.=FALSE)
      out[[length(out)+1L]] <- list(id=id,record=r)
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
    out[[i]] <- list(id=id,record=r)
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
    for (p in fs) out <- c(out,extract_lens_jsonl(p))
    return(out)
  }
  fs <- list.files(root,pattern="^response_[0-9]{6}\\.json$",recursive=TRUE,full.names=TRUE)
  if (!length(fs)) return(list())
  out <- list()
  for (p in fs) out <- c(out,extract_from_json_file(p,source))
  out
}

read_registry <- function() {
  ids <- character()
  if (nzchar(registry_path)) {
    if (!file.exists(registry_path)) stop(sprintf("Native-ID registry not found: %s",registry_path),call.=FALSE)
    ids <- trimws(readLines(registry_path,warn=FALSE,encoding="UTF-8"))
  } else {
    history <- extract_records(history_root,source)
    if (length(history)) ids <- c(ids,vapply(history,`[[`,character(1),"id"))
    files <- list.files(history_root,pattern="native_ids\\.txt$",recursive=TRUE,full.names=TRUE)
    if (length(files)) {
      ids <- c(ids,unlist(lapply(files,function(p)trimws(readLines(p,warn=FALSE,encoding="UTF-8"))),use.names=FALSE))
    }
  }
  ids <- ids[nzchar(ids)]
  unique(ids)
}

write_filtered_source <- function(records,source,root) {
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  if (source=="lens") {
    con <- file(file.path(root,"records.jsonl"),"wt",encoding="UTF-8")
    on.exit(close(con),add=TRUE)
    if (length(records)) for (z in records) writeLines(toJSON(z$record,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
    return(invisible(NULL))
  }
  dir.create(file.path(root,"raw"),recursive=TRUE,showWarnings=FALSE)
  rows <- lapply(records,`[[`,"record")
  payload <- switch(source,
    scopus=list("search-results"=list(entry=rows)),
    openalex=list(results=rows),
    agricola=list(hitCount=length(rows),resultList=list(result=rows)),
    pubmed=list(hitCount=length(rows),resultList=list(result=rows)),
    ethos=list(hitCount=length(rows),resultList=list(result=rows)),
    cba=list(hitCount=length(rows),resultList=list(result=rows)),
    epmc_preprints=list(hitCount=length(rows),resultList=list(result=rows)),
    wos=list(metadata=list(total=length(rows)),hits=rows)
  )
  writeLines(toJSON(payload,auto_unbox=TRUE,null="null",na="null",digits=NA),
             file.path(root,"raw","response_000001.json"))
}

current_raw <- extract_records(current_root,source)
if (!length(current_raw)) {
  dir.create(file.path(output_dir,"filtered_source"),recursive=TRUE,showWarnings=FALSE)
  writeLines(character(),file.path(output_dir,"new_native_ids.txt"))
  writeLines(character(),file.path(output_dir,"already_known_native_ids.txt"))
  known_ids <- read_registry()
  writeLines(sort(known_ids),file.path(output_dir,"updated_native_ids.txt"))
  write_filtered_source(list(),source,file.path(output_dir,"filtered_source"))
  manifest <- list(
    workflow="00_native_id_reconciliation",status="success",reconciled_at=now_utc(),
    run_type=run_type,source=source,current_raw_records=0,current_unique_records=0,
    exact_duplicate_ids_suppressed=0,historical_unique_native_ids=length(known_ids),
    already_known_native_ids=0,new_native_ids=0,records_passed_downstream=0,
    bibliographic_deduplication_performed=FALSE,doi_matching_performed=FALSE,
    fuzzy_matching_performed=FALSE,downstream_deduplication="Workflow 01"
  )
  writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
             file.path(output_dir,"reconciliation_manifest.json"))
  message(sprintf("PASS: %s native-ID reconciliation: retrieved=0 known=0 new=0",source))
  quit(status=0L)
}

current_ids_raw <- vapply(current_raw,`[[`,character(1),"id")
duplicate_ids <- unique(current_ids_raw[duplicated(current_ids_raw)])
keep <- !duplicated(current_ids_raw)
current <- current_raw[keep]
current_ids <- current_ids_raw[keep]
history_ids <- read_registry()

known <- current_ids %in% history_ids
new <- !known
new_records <- current[new]

writeLines(current_ids[new],file.path(output_dir,"new_native_ids.txt"))
writeLines(current_ids[known],file.path(output_dir,"already_known_native_ids.txt"))
writeLines(sort(unique(c(history_ids,current_ids))),file.path(output_dir,"updated_native_ids.txt"))

con <- file(file.path(output_dir,"new_records.jsonl"),"wt",encoding="UTF-8")
if (length(new_records)) {
  for (r in new_records) {
    writeLines(toJSON(list(source=source,native_id=r$id,raw_payload=r$record),
                      auto_unbox=TRUE,null="null",na="null",digits=NA),con)
  }
}
close(con)

write_filtered_source(new_records,source,file.path(output_dir,"filtered_source"))

manifest <- list(
  workflow="00_native_id_reconciliation",
  status="success",
  reconciled_at=now_utc(),
  run_type=run_type,
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
  current_raw_records=length(current_ids_raw),
  current_unique_records=length(current_ids),
  exact_duplicate_ids_suppressed=length(duplicate_ids),
  duplicate_native_ids=duplicate_ids,
  historical_unique_native_ids=length(history_ids),
  already_known_native_ids=sum(known),
  new_native_ids=sum(new),
  records_passed_downstream=sum(new),
  filtered_source_root="filtered_source",
  bibliographic_deduplication_performed=FALSE,
  doi_matching_performed=FALSE,
  fuzzy_matching_performed=FALSE,
  downstream_deduplication="Workflow 01"
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"reconciliation_manifest.json"))
message(sprintf("PASS: %s native-ID reconciliation: raw=%d unique=%d known=%d new=%d",
                source,length(current_ids_raw),length(current_ids),sum(known),sum(new)))
