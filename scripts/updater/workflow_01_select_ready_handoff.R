#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

handoff_dir <- arg("--handoff-dir","docs/deduplication/handoffs/w01")
resolution_dir <- arg("--resolution-dir","docs/deduplication/checkpoints/resolutions")
output <- arg("--output")
if(is.null(output)) stop("Required: --output",call.=FALSE)

paths <- if(dir.exists(handoff_dir)) list.files(handoff_dir,pattern="^run-[0-9]+\\.json$",full.names=TRUE) else character()
rows <- list()
for(path in paths){
  x <- fromJSON(path,simplifyVector=FALSE)
  if(!identical(as.character(x$schema),"living-evidence-map-workflow01-shiny-handoff-v1")) next
  if(!identical(as.character(x$status),"ready")) next
  run_id <- as.character(x$checkpoint_run_id)
  if(!grepl("^[0-9]+$",run_id)) stop("Handoff has invalid checkpoint_run_id: ",path,call.=FALSE)
  review_id <- if(is.null(x$review_id)) "" else as.character(x$review_id)
  if(nzchar(review_id) && !grepl("^[A-Za-z0-9._-]{3,128}$",review_id))
    stop("Ready handoff has invalid review_id: ",path,call.=FALSE)
  resolved_dir <- if(nzchar(review_id)) {
    file.path("docs/deduplication/reviews",review_id,"checkpoints","resolutions")
  } else resolution_dir
  resolution <- file.path(resolved_dir,paste0("run-",run_id,".json"))
  if(file.exists(resolution)){
    r <- fromJSON(resolution,simplifyVector=FALSE)
    if(identical(as.character(r$status),"resolved")) next
  }
  rows[[length(rows)+1L]] <- c(list(handoff_path=gsub("\\\\","/",path)),x)
}

if(length(rows)>1L){
  ids <- vapply(rows,function(z)as.character(z$checkpoint_run_id),character(1))
  stop("Multiple unresolved ready W01 handoffs found; refusing to guess: ",paste(ids,collapse=", "),call.=FALSE)
}

out <- if(!length(rows)) {
  list(schema="living-evidence-map-workflow01-ready-handoff-selection-v1",found=FALSE)
} else {
  c(list(schema="living-evidence-map-workflow01-ready-handoff-selection-v1",found=TRUE),rows[[1L]])
}
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(out,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),output,useBytes=TRUE)
cat(if(isTRUE(out$found)) paste0("FOUND ",out$checkpoint_run_id) else "NONE", "\n")
