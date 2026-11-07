#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_path <- arg("--input")
output_dir <- arg("--output-dir")
batch_size <- as.integer(arg("--batch-size","1000"))
recheck_after_days <- as.numeric(arg("--recheck-after-days","90"))
enrich_author_keywords <- tolower(arg("--enrich-author-keywords","false")) %in% c("true","1","yes")
max_records <- as.integer(arg("--max-records","0"))
mode <- tolower(arg("--mode","incremental"))
if(is.null(input_path)||is.null(output_dir)) stop("Required: --input --output-dir",call.=FALSE)
if(is.na(batch_size)||batch_size<1L) stop("--batch-size must be >= 1",call.=FALSE)
if(is.na(recheck_after_days)||recheck_after_days<0) stop("--recheck-after-days must be >= 0",call.=FALSE)
if(is.na(max_records)||max_records<0L) stop("--max-records must be >= 0",call.=FALSE)
if(!mode %in% c("incremental","repair","backfill")) stop("--mode must be incremental, repair, or backfill",call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
clean_text <- function(x){
  if(is.null(x)||!length(x)) return(NULL)
  s <- trimws(gsub("[[:space:]]+"," ",as.character(x[[1L]])))
  if(is.na(s)||!nzchar(s)) NULL else s
}
norm_doi <- function(x){
  s <- clean_text(x)
  if(is.null(s)) return(NULL)
  s <- tolower(s)
  s <- sub("^https?://(dx\\.)?doi\\.org/","",s,perl=TRUE)
  s <- sub("^doi:\\s*","",s,perl=TRUE)
  s <- sub("[[:space:]]+$","",s)
  s <- sub("[[:space:][:punct:]]+$","",s)
  if(!nzchar(s)) NULL else s
}
is_missing <- function(x) is.null(clean_text(x))
keywords_missing <- function(x){
  if(is.null(x)||!length(x)) return(TRUE)
  vals <- trimws(gsub("[[:space:]]+"," ",as.character(unlist(x,use.names=FALSE))))
  vals <- vals[!is.na(vals)&nzchar(vals)]
  !length(vals)
}
previous_attempt_due <- function(meta){
  if(is.null(meta)||!is.list(meta)) return(TRUE)
  if(isTRUE(meta$technical_error)) return(TRUE)
  completed <- clean_text(meta$completed_at)
  if(is.null(completed)) return(TRUE)
  t <- suppressWarnings(as.POSIXct(completed,tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"))
  if(is.na(t)) return(TRUE)
  age_days <- as.numeric(difftime(Sys.time(),t,units="days"))
  is.na(age_days)||age_days>=recheck_after_days
}
read_jsonl <- function(path){
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  lapply(seq_along(lines),function(i)fromJSON(lines[[i]],simplifyVector=FALSE))
}

rows <- read_jsonl(input_path)
due <- character()
due_rows <- list()
eligible_total <- 0L
deferred <- 0L

for(r in rows){
  if(is.null(r$canonical)) r$canonical <- list()
  d <- norm_doi(r$canonical$doi)
  eligible <- !is.null(d) && (
    is_missing(r$canonical$title) ||
    is_missing(r$canonical$abstract) ||
    (enrich_author_keywords && keywords_missing(r$canonical$author_keywords))
  )
  if(!eligible) next
  eligible_total <- eligible_total + 1L

  meta <- r$metadata_enrichment
  select <- switch(
    mode,
    incremental = is.null(meta) || !is.list(meta),
    repair = is.list(meta) && isTRUE(meta$technical_error),
    backfill = previous_attempt_due(meta)
  )
  if(!select){
    deferred <- deferred + 1L
    next
  }

  rid <- clean_text((r$identity %||% list())$record_id %||% (r$identity %||% list())$lens_id)
  if(is.null(rid)) stop("Eligible Workflow 02 record missing identity.record_id",call.=FALSE)
  due <- c(due,rid)
  due_rows[[length(due_rows)+1L]] <- r
}

if(anyDuplicated(due)) stop("Duplicate record IDs in Workflow 02 due set",call.=FALSE)
if(max_records>0L && length(due)>max_records){
  keep <- seq_len(max_records)
  due <- due[keep]
  due_rows <- due_rows[keep]
}

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
batch_count <- if(length(due)) ceiling(length(due)/batch_size) else 0L
include <- list()

if(batch_count){
  for(i in seq_len(batch_count)){
    lo <- (i-1L)*batch_size+1L
    hi <- min(i*batch_size,length(due))
    ids <- due[lo:hi]
    batch_rows <- due_rows[lo:hi]
    batch <- sprintf("%04d",i)

    ids_path <- file.path(output_dir,paste0("batch-",batch,".txt"))
    writeLines(ids,ids_path,useBytes=TRUE)

    jsonl_path <- file.path(output_dir,paste0("batch-",batch,".jsonl"))
    con <- file(jsonl_path,"wt",encoding="UTF-8")
    for(z in batch_rows) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
    close(con)

    include[[length(include)+1L]] <- list(
      batch=batch,
      records=length(ids),
      input_sha256=digest(file=jsonl_path,algo="sha256",serialize=FALSE)
    )
  }
}

matrix <- list(include=include)
matrix_path <- file.path(output_dir,"matrix.json")
writeLines(toJSON(matrix,auto_unbox=TRUE,null="null"),matrix_path,useBytes=TRUE)

plan <- list(
  schema="living-evidence-map-workflow02-batch-plan-v1",
  mode=mode,
  input_sha256=digest(file=input_path,algo="sha256",serialize=FALSE),
  total_records=length(rows),
  eligible_records=eligible_total,
  deferred_recent_attempts=deferred,
  due_records=length(due),
  batch_size=batch_size,
  batch_count=batch_count,
  max_records=if(max_records>0L) max_records else NULL,
  matrix_sha256=digest(file=matrix_path,algo="sha256",serialize=FALSE),
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(plan,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"plan.json"),useBytes=TRUE)
cat(sprintf("PASS: Workflow 02 plan (%s): %d due records in %d batches of up to %d\n",mode,length(due),batch_count,batch_size))
