#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
or_else <- function(a,b) if (is.null(a)) b else a

receipt_path <- arg("--receipt")
registry_path <- arg("--registry")
pointer_dir <- arg("--pointer-dir")
if (any(vapply(list(receipt_path,registry_path,pointer_dir),is.null,logical(1)))) {
  stop("Required: --receipt --registry --pointer-dir",call.=FALSE)
}
if (!file.exists(receipt_path)) stop("Zenodo receipt not found",call.=FALSE)
x <- fromJSON(receipt_path,simplifyVector=FALSE)
if (!identical(x$status,"published")) {
  stop(sprintf("Receipt is not a new published archive: %s",or_else(x$status,"<missing>")),call.=FALSE)
}

dir.create(dirname(registry_path),recursive=TRUE,showWarnings=FALSE)
dir.create(pointer_dir,recursive=TRUE,showWarnings=FALSE)

row <- data.frame(
  github_run_id=as.character(x$github_run_id),
  run_type=as.character(x$run_type),
  search_version=as.character(x$search_version),
  sources=paste(unlist(x$sources,use.names=FALSE),collapse=";"),
  zenodo_record_id=as.character(x$zenodo_record_id),
  doi=as.character(x$doi),
  record_url=as.character(x$record_url),
  visibility=as.character(x$visibility),
  manifest_sha256=as.character(x$manifest_sha256),
  published_at_utc=as.character(x$published_at_utc),
  stringsAsFactors=FALSE
)

if (file.exists(registry_path)) {
  old <- read.csv(registry_path,stringsAsFactors=FALSE,check.names=FALSE)
  old <- old[as.character(old$github_run_id) != row$github_run_id,,drop=FALSE]
  out <- rbind(old,row)
} else out <- row
out <- out[order(suppressWarnings(as.numeric(out$github_run_id))),,drop=FALSE]
write.csv(out,registry_path,row.names=FALSE,na="")

pointer <- file.path(pointer_dir,paste0("run-",row$github_run_id,".json"))
if (!file.copy(receipt_path,pointer,overwrite=TRUE)) stop("Failed to write Zenodo pointer file",call.=FALSE)
cat(sprintf("PASS: registered Zenodo record %s for Workflow 00 run %s\n",row$zenodo_record_id,row$github_run_id))
