#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
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

pointer_path <- normalizePath(arg("--pointer"),mustWork=TRUE)
output_root <- arg("--output-root")
if(is.null(output_root)) stop("Required: --pointer --output-root",call.=FALSE)
if(file.exists(output_root)) stop("Output root already exists",call.=FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)
p <- fromJSON(pointer_path,simplifyVector=FALSE)
if(!identical(p$status,"published")||!identical(p$state,"pre_adjudication")||!identical(p$visibility,"restricted")){
  stop("Pointer is not a published restricted pre_adjudication checkpoint",call.=FALSE)
}
files <- p$archive_files
if(is.data.frame(files)) files <- lapply(seq_len(nrow(files)),function(i)as.list(files[i,,drop=FALSE]))
if(is.list(files)&&!is.null(files$filename)) files <- list(files)
if(!length(files)) stop("Checkpoint pointer lacks archive_files",call.=FALSE)

dep_id <- as.character(p$zenodo_deposition_id)
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
dep <- request(paste0("https://zenodo.org/api/deposit/depositions/",dep_id)) |>
  auth() |> req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector=FALSE)
if(!isTRUE(dep$submitted)) stop("Checkpoint deposition is not published",call.=FALSE)
bucket <- as.character(dep$links$bucket)

dir.create(output_root,recursive=TRUE)
downloads <- file.path(output_root,".downloads")
dir.create(downloads)
local <- character()
for(z in files){
  fn <- as.character(z$filename)
  dest <- file.path(downloads,fn)
  resp <- request(paste0(sub("/$","",bucket),"/",URLencode(fn,reserved=TRUE))) |>
    req_method("GET") |> auth() |> req_timeout(1800) |>
    req_error(is_error=function(resp)FALSE) |> req_perform(path=dest)
  if(resp_status(resp)!=200L) stop(sprintf("Checkpoint download failed for %s",fn),call.=FALSE)
  if(as.numeric(file.info(dest)$size)!=as.numeric(z$bytes)) stop(sprintf("Byte mismatch for %s",fn),call.=FALSE)
  actual_sha <- digest(file=dest,algo="sha256",serialize=FALSE)
  if(!identical(tolower(actual_sha),tolower(as.character(z$sha256)))) stop(sprintf("SHA mismatch for %s",fn),call.=FALSE)
  if(!is.null(z$zenodo_checksum)&&nzchar(as.character(z$zenodo_checksum))){
    expected_md5 <- sub("^md5:","",tolower(as.character(z$zenodo_checksum)))
    actual_md5 <- digest(file=dest,algo="md5",serialize=FALSE)
    if(!identical(actual_md5,expected_md5)) stop(sprintf("MD5 mismatch for %s",fn),call.=FALSE)
  }
  local <- c(local,dest)
}

tar <- local[grepl("\\.tar\\.gz$",local)]
if(length(tar)!=1L) stop("Expected exactly one checkpoint tar.gz",call.=FALSE)
extract <- file.path(output_root,"extract")
dir.create(extract)
utils::untar(tar,exdir=extract)
manifests <- list.files(extract,pattern="^checkpoint_manifest\\.json$",recursive=TRUE,full.names=TRUE)
if(length(manifests)!=1L) stop("Expected exactly one checkpoint_manifest.json",call.=FALSE)
checkpoint_dir <- dirname(manifests[[1L]])
cm <- fromJSON(manifests[[1L]],simplifyVector=FALSE)
if(xor(is.null(cm$review_id),is.null(cm$google_sheet_id)) ||
   xor(is.null(p$review_id),is.null(p$google_sheet_id)))
  stop("Checkpoint or pointer has incomplete review identity",call.=FALSE)
if(!is.null(cm$review_id) || !is.null(p$review_id)) {
  if(is.null(cm$review_id) || is.null(p$review_id) ||
     !nzchar(as.character(cm$review_id)) || !nzchar(as.character(cm$google_sheet_id)) ||
     !identical(as.character(cm$review_id),as.character(p$review_id)) ||
     !identical(as.character(cm$google_sheet_id),as.character(p$google_sheet_id)))
    stop("Checkpoint review identity differs from its Zenodo pointer",call.=FALSE)
}
if(!identical(as.character(cm$queue_sha256),as.character(p$queue_sha256))) stop("Checkpoint queue SHA does not match pointer",call.=FALSE)
if(!identical(as.character(cm$previous_workflow01$zenodo_record_id),as.character(p$previous_zenodo_record_id))) stop("Checkpoint previous-state lineage mismatch",call.=FALSE)

audit <- list(
  schema="living-evidence-map-workflow01-pending-checkpoint-restore-v1",
  status="PASS",
  github_run_id=as.character(p$github_run_id),
  zenodo_record_id=as.character(p$zenodo_record_id),
  checkpoint_dir=checkpoint_dir,
  queue_sha256=as.character(cm$queue_sha256),
  restored_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_root,"checkpoint_restore_audit.json"),useBytes=TRUE)
cat(checkpoint_dir)
