#!/usr/bin/env Rscript
# Export one RIS entry per final canonical bibliographic work; no source manifestations.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(TRUE)
getarg <- function(flag) {i<-match(flag,args);if(is.na(i)||i==length(args))stop("Missing ",flag);args[[i+1L]]}
input <- getarg("--input")
output <- getarg("--output")
manifest <- getarg("--manifest")
stopifnot(file.exists(input),file.exists(manifest))
m <- fromJSON(manifest,simplifyVector=FALSE)
if(!identical(as.character(m$workflow),"01") || !identical(as.character(m$schema),"living-evidence-map-canonical-manifest-v1"))
  stop("Final canonical W01 manifest required")
scalar <- function(x) {
  if(is.null(x)||!length(x)) return("")
  z <- trimws(as.character(unlist(x,use.names=FALSE)))
  z <- z[!is.na(z)&nzchar(z)]
  if(length(z)) z[[1L]] else ""
}
field <- function(tag,val) {
  vals <- trimws(as.character(unlist(val,use.names=FALSE)))
  vals <- vals[!is.na(vals)&nzchar(vals)]
  if(!length(vals)) return(character())
  paste0(tag,"  - ",gsub("[[:cntrl:]]+", " ", vals))
}
lines <- readLines(input,warn=FALSE,encoding="UTF-8")
if(length(lines)!=as.integer(m$records)) stop("Canonical record count mismatch")
seen <- character()
con <- file(output,"wt",encoding="UTF-8")
# Close the output explicitly before the caller checks its contents.
for (ln in lines) {
  z <- fromJSON(ln,simplifyVector=FALSE)
  if(!isTRUE(z$deduplication$final)) stop("Non-final canonical record")
  id <- scalar(z$identity$record_id)
  if(!nzchar(id)||id %in% seen) stop("Missing or duplicated canonical ID")
  seen <- c(seen,id)
  x <- z$canonical
  typ <- tolower(scalar(x$publication_type))
  ris_type <- if(grepl("book",typ)) "BOOK" else if(grepl("thesis|dissertation",typ)) "THES" else if(grepl("conference",typ)) "CPAPER" else "JOUR"
  authors <- unlist(x$authors,use.names=FALSE)
  keywords <- unlist(x$author_keywords,use.names=FALSE)
  entry <- c(paste0("TY  - ",ris_type),field("ID",id),field("TI",scalar(x$title)),
             field("AU",authors),field("PY",scalar(x$year)),field("JO",scalar(x$journal)),
             field("VL",scalar(x$volume)),field("IS",scalar(x$issue)),field("SP",scalar(x$pages)),
             field("DO",scalar(x$doi)),field("AB",scalar(x$abstract)),field("KW",keywords),
             field("SN",scalar(x$issn)),field("LA",scalar(x$language)),
             "ER  - ","")
  writeLines(entry,con,useBytes=TRUE)
}
close(con)
cat(sprintf("PASS: exported %d unique canonical works to RIS\\n",length(seen)))
