#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
  library(googlesheets4)
})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(name) { i <- match(name,args); if(is.na(i)||i==length(args))stop("Missing ",name); args[[i+1L]] }
input <- arg("--input")
sheet <- arg("--sheet-id")
run <- arg("--source-run-id")
credential <- arg("--credential")
if(!grepl("^[0-9]+$",run) || !grepl("^[A-Za-z0-9_-]{15,150}$",sheet))stop("Invalid review identity")
if(!file.exists(input)||!file.exists(credential))stop("Missing corpus or credential")
null <- function(x) if(is.null(x)||!length(x)) "" else as.character(x[[1L]])
auth <- function(x) trimws(null(x))
if(!nzchar(auth(Sys.getenv("LEM_W02_ABSTRACT_QUEUE_ALLOWED","")))) stop("Explicit manual queue opt-in required")
gs4_auth(path=credential)
meta <- read_sheet(sheet,sheet="review_metadata",col_types="cc")
if(!all(c("key","value") %in% names(meta))) stop("Invalid review metadata")
review <- as.character(meta$value[match("review_id",meta$key)])
if(length(review)!=1L || is.na(review)||!grepl("^[A-Za-z0-9._-]{3,128}$",review)) stop("Invalid review ID")
con <- file(input,"rt",encoding="UTF-8"); on.exit(close(con),add=TRUE)
rows <- list()
repeat {
  line <- readLines(con,n=1L,warn=FALSE)
  if(!length(line))break
  if(!nzchar(trimws(line)))next
  rec <- fromJSON(line,simplifyVector=FALSE)
  can <- rec$canonical
  abstract <- trimws(null(can$abstract))
  if(nzchar(abstract))next
  id <- null(rec$identity$record_id)
  if(!nzchar(id))stop("Missing stable record ID in enriched corpus")
  rows[[length(rows)+1L]] <- data.frame(
    review_id=review,source_run_id=run,record_id=id,
    title=null(can$title),doi=null(can$doi),
    authors=paste(unlist(can$authors),collapse="; "),
    year=null(can$year),abstract="",stringsAsFactors=FALSE)
}
if(!length(rows)) {
  cat("PASS: no missing abstracts; nothing queued\\n")
  quit(save="no",status=0L)
}
data <- do.call(rbind,rows)
if(anyDuplicated(data$record_id)) stop("Duplicate stable record ID")
data <- data[order(data$record_id),,drop=FALSE]
digest_input <- paste(data$record_id,collapse="\\n")
queue_sha <- digest(digest_input,algo="sha256",serialize=FALSE)
data$queue_sha256 <- queue_sha
tabs <- sheet_names(sheet)
target <- "queue_w02_missing_abstracts"
if(target %in% tabs) {
  existing <- read_sheet(sheet,sheet=target,col_types="c")
  if(nrow(existing) && (!all(as.character(existing$source_run_id)==run) ||
                        !all(as.character(existing$queue_sha256)==queue_sha)))
    stop("Different missing-abstract queue already exists; refusing overwrite")
}
sheet_write(data,ss=sheet,sheet=target)
cat(sprintf("PASS: published %d missing-abstract records; review=%s run=%s queue_sha=%s\\n",
            nrow(data),review,run,queue_sha))
