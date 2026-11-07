#!/usr/bin/env Rscript
# Generic RIS -> existing W01 sidecar record structure.
# Offline conversion only; Zenodo download and review routing are handled upstream.
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(name) {
  i <- which(args == name)
  if(length(i)!=1L || i==length(args)) stop("Required argument: ",name,call.=FALSE)
  args[[i+1L]]
}
input_dir <- arg("--input-dir")
source <- arg("--source")
submission <- arg("--submission-id")
output <- arg("--output")
if(!dir.exists(input_dir)) stop("RIS input directory does not exist",call.=FALSE)
if(!grepl("^[a-z0-9][a-z0-9_-]*$",source) ||
   !grepl("^[A-Za-z0-9._-]+$",submission)) stop("Invalid source/submission",call.=FALSE)
files <- sort(list.files(input_dir,pattern="\\.[Rr][Ii][Ss]$",full.names=TRUE))
if(!length(files)) stop("No RIS files found",call.=FALSE)
first <- function(x,keys) {
  for(k in keys) if(length(x[[k]])) {
    v <- trimws(paste(x[[k]][[1L]],collapse=" "))
    if(nzchar(v)) return(v)
  }
  NULL
}
all_values <- function(x,keys) {
  unname(unlist(x[keys],use.names=FALSE))
}
parse_ris <- function(path) {
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- sub("\\r$","",lines)
  records <- list(); current <- NULL; last <- NULL
  for(line in lines) {
    if(grepl("^TY  -",line)) {
      if(!is.null(current)) stop("Nested TY in ",basename(path),call.=FALSE)
      current <- list(TY=trimws(sub("^TY  -[[:space:]]?","",line))); last <- NULL
    } else if(grepl("^ER  -",line)) {
      if(is.null(current)) stop("Unexpected ER in ",basename(path),call.=FALSE)
      records[[length(records)+1L]] <- current
      current <- NULL; last <- NULL
    } else if(grepl("^[A-Z0-9]{2}  -[[:space:]]?",line)) {
      if(is.null(current)) stop("RIS field outside TY/ER in ",basename(path),call.=FALSE)
      tag <- substr(line,1L,2L)
      val <- trimws(sub("^[A-Z0-9]{2}  -[[:space:]]?","",line))
      current[[tag]] <- c(current[[tag]],val); last <- tag
    } else if(!is.null(current) && !is.null(last) && nzchar(trimws(line))) {
      n <- length(current[[last]])
      current[[last]][[n]] <- paste(current[[last]][[n]],trimws(line),sep=" ")
    } else if(is.null(current) && nzchar(trimws(line))) {
      stop("Unexpected non-RIS content in ",basename(path),call.=FALSE)
    }
  }
  if(!is.null(current)) stop("Unclosed RIS record in ",basename(path),call.=FALSE)
  records
}
out <- list()
for(file in files) {
  hash <- digest(file=file,algo="sha256",serialize=FALSE)
  ris <- parse_ris(file)
  for(i in seq_along(ris)) {
    x <- ris[[i]]
    origin_id <- first(x,c("ID","AN","CN"))
    title <- first(x,c("TI","T1","CT","BT"))
    year_raw <- first(x,c("PY","Y1","DA"))
    year <- if(!is.null(year_raw) && grepl("(18|19|20|21)[0-9]{2}",year_raw))
      regmatches(year_raw,regexpr("(18|19|20|21)[0-9]{2}",year_raw)) else NULL
    doi_original <- first(x,c("DO"))
    doi <- if(is.null(doi_original)) NULL else {
      z <- trimws(sub("^https?://(dx\\.)?doi\\.org/","",doi_original,ignore.case=TRUE))
      z <- sub("^doi:[[:space:]]*","",z,ignore.case=TRUE)
      if(nzchar(z)) tolower(z) else NULL
    }
    au <- c(x[["AU"]],x[["A1"]])
    authors <- if(length(au)) lapply(au,function(name)
      list(full_name=trimws(name))) else NULL
    page_start <- first(x,c("SP"))
    page_end <- first(x,c("EP"))
    pages <- first(x,c("PG"))
    if(is.null(pages) && !is.null(page_start))
      pages <- if(is.null(page_end)) page_start else paste0(page_start,"-",page_end)
    stable_id <- paste(source,submission,basename(file),hash,sprintf("%06d",i),sep=":")
    out[[length(out)+1L]] <- list(
      sidecar_identity=list(sidecar_record_id=stable_id,source_record_id=origin_id,
        doi=doi),
      source=list(provider=source,source_format="ris",source_collection="manual_ris"),
      mapped_fields=list(title=title,abstract=first(x,c("AB","N2")),
        authors=authors,author_string=if(length(au))paste(au,collapse="; ") else NULL,
        year=year,source=first(x,c("JO","JF","T2","J2")),
        volume=first(x,c("VL")),issue=first(x,c("IS")),
        pages=pages,start_page=page_start,end_page=page_end,
        doi=doi,publication_type=first(x,c("TY"))),
      provenance=list(adapter_workflow="workflow_01_generic_ris_to_sidecar",
        submission_id=submission,original_filename=basename(file),
        original_file_sha256=hash,record_ordinal=i,original_doi=doi_original,
        original_ris_fields=x,canonical_json_modified=FALSE)
    )
  }
}
ids <- vapply(out,function(x)x$sidecar_identity$sidecar_record_id,character(1))
if(anyDuplicated(ids)) stop("Duplicate stable RIS manifestation IDs",call.=FALSE)
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
con <- file(output,"wt",encoding="UTF-8")
on.exit(close(con),add=TRUE)
for(x in out) writeLines(toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),con,useBytes=TRUE)
cat(sprintf("Converted %d RIS records from %d files to %s\\n",length(out),length(files),output))
