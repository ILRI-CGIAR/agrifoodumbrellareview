#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
queue_path <- arg("--queue")
decisions_path <- arg("--decisions")
repairs_path <- arg("--repairs")
output_dir <- arg("--output-dir")
if(any(vapply(list(queue_path,decisions_path,repairs_path,output_dir),is.null,logical(1)))) stop("Required: --queue --decisions --repairs --output-dir",call.=FALSE)

read_jsonl <- function(path) {
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
write_jsonl <- function(x,path) {
  con <- file(path,"wt",encoding="UTF-8")
  on.exit(close(con),add=TRUE)
  for(z in x) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),con,useBytes=TRUE)
}
queue <- read_jsonl(queue_path)
decisions <- read_jsonl(decisions_path)
repairs <- read_jsonl(repairs_path)
queue_sha <- digest(file=queue_path,algo="sha256",serialize=FALSE)
ids <- vapply(queue,function(z)as.character(z$review_case_id),character(1))
if(anyDuplicated(ids)) stop("Queue contains duplicate review_case_id",call.=FALSE)
qmap <- setNames(queue,ids)

for(i in seq_along(decisions)) {
  d <- decisions[[i]]
  id <- as.character(d$review_case_id)
  if(is.null(qmap[[id]])) stop(sprintf("Decision references unknown case %s",id),call.=FALSE)
  d$queue_sha256 <- queue_sha
  decisions[[i]] <- d
}
for(i in seq_along(repairs)) {
  r <- repairs[[i]]
  id <- as.character(r$review_case_id)
  q <- qmap[[id]]
  if(is.null(q)) stop(sprintf("Repair references unknown case %s",id),call.=FALSE)
  if(is.null(r$source_record_id)) {
    side <- toupper(as.character(r$record))
    if(!(side %in% c("A","B"))) stop(sprintf("Legacy repair %s lacks source_record_id and valid A/B record label",id),call.=FALSE)
    rec <- if(side=="A") q$record_i else q$record_j
    r$source <- as.character(rec$source)
    r$source_record_id <- as.character(rec$source_record_id)
  } else {
    members <- c(as.character(q$record_i$source_record_id),as.character(q$record_j$source_record_id))
    if(!(as.character(r$source_record_id) %in% members)) stop(sprintf("Repair %s targets record outside pair",id),call.=FALSE)
    rec <- if(identical(as.character(r$source_record_id),members[[1]])) q$record_i else q$record_j
    r$source <- as.character(rec$source)
  }
  r$queue_sha256 <- queue_sha
  repairs[[i]] <- r
}

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
write_jsonl(decisions,file.path(output_dir,"human_decisions.normalised.jsonl"))
write_jsonl(repairs,file.path(output_dir,"data_quality_repairs.normalised.jsonl"))
manifest <- list(
  schema="living-evidence-map-workflow01-human-state-migration-v1",
  queue_sha256=queue_sha,
  decisions=length(decisions),
  repairs=length(repairs),
  decisions_sha256=digest(file=file.path(output_dir,"human_decisions.normalised.jsonl"),algo="sha256",serialize=FALSE),
  repairs_sha256=digest(file=file.path(output_dir,"data_quality_repairs.normalised.jsonl"),algo="sha256",serialize=FALSE)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(output_dir,"migration_manifest.json"),useBytes=TRUE)
cat(sprintf("PASS: normalised %d decisions and %d repairs against queue SHA %s\n",length(decisions),length(repairs),queue_sha))
