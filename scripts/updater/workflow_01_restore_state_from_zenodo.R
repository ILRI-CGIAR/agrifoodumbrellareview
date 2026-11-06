#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
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

pointer_path <- arg("--pointer")
output_root <- arg("--output-root")
if (is.null(pointer_path)||is.null(output_root)) stop("Required: --pointer --output-root",call.=FALSE)
if (!file.exists(pointer_path)) stop("Workflow 01 Zenodo pointer not found",call.=FALSE)
token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)

x <- fromJSON(pointer_path,simplifyVector=FALSE)
if (!identical(x$status,"published")) stop("Workflow 01 pointer is not published",call.=FALSE)
if (!identical(x$visibility,"restricted")) stop("Expected restricted Workflow 01 archive",call.=FALSE)
dep_id <- as.character(x$zenodo_deposition_id)
if (!nzchar(dep_id)) stop("Pointer lacks zenodo_deposition_id",call.=FALSE)

files <- x$archive_files
if (is.data.frame(files)) files <- lapply(seq_len(nrow(files)),function(i)as.list(files[i,,drop=FALSE]))
if (is.list(files)&&!is.null(files$filename)) files <- list(files)
if (!length(files)) stop("Pointer lacks archive_files",call.=FALSE)

expected <- lapply(files,function(z)list(
  filename=as.character(z$filename),
  bytes=as.numeric(z$bytes),
  sha256=as.character(z$sha256),
  zenodo_checksum=if(is.null(z$zenodo_checksum)) NA_character_ else as.character(z$zenodo_checksum)
))
names(expected) <- vapply(expected,function(z)z$filename,character(1))

api <- paste0("https://zenodo.org/api/deposit/depositions/",dep_id)
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
resp <- request(api) |> auth() |> req_timeout(120) |> req_perform()
dep <- resp_body_json(resp,simplifyVector=FALSE)
if (!isTRUE(dep$submitted)) stop("Zenodo Workflow 01 deposition is not published",call.=FALSE)
bucket <- as.character(dep$links$bucket)
if (!nzchar(bucket)) stop("Published deposition lacks bucket link",call.=FALSE)

dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
download_dir <- file.path(output_root,".workflow01_zenodo_download")
dir.create(download_dir,recursive=TRUE,showWarnings=FALSE)

download_one <- function(e) {
  dest <- file.path(download_dir,e$filename)
  url <- paste0(sub("/$","",bucket),"/",URLencode(e$filename,reserved=TRUE))
  r <- request(url) |> req_method("GET") |> auth() |> req_timeout(1800) |>
    req_error(is_error=function(resp)FALSE) |> req_perform(path=dest)
  if (resp_status(r)!=200L) stop(sprintf("Download failed for %s: HTTP %d",e$filename,resp_status(r)),call.=FALSE)
  bytes <- unname(file.info(dest)$size)
  sha <- digest(file=dest,algo="sha256",serialize=FALSE)
  if (!identical(as.numeric(bytes),as.numeric(e$bytes))) stop(sprintf("Byte-size mismatch for %s",e$filename),call.=FALSE)
  if (!identical(tolower(sha),tolower(e$sha256))) stop(sprintf("SHA-256 mismatch for %s",e$filename),call.=FALSE)
  if (!is.na(e$zenodo_checksum)&&nzchar(e$zenodo_checksum)) {
    md5e <- sub("^md5:","",tolower(e$zenodo_checksum))
    md5a <- digest(file=dest,algo="md5",serialize=FALSE)
    if (!identical(md5a,md5e)) stop(sprintf("MD5 mismatch for %s",e$filename),call.=FALSE)
  }
  dest
}

local <- lapply(expected,download_one)
manifest_ix <- grep("_manifest\\.json$",basename(unlist(local)))
if (length(manifest_ix)!=1L) stop("Expected exactly one Workflow 01 manifest",call.=FALSE)
manifest_path <- unlist(local)[manifest_ix]
manifest_sha <- digest(file=manifest_path,algo="sha256",serialize=FALSE)
if (!is.null(x$manifest_sha256)&&!identical(tolower(manifest_sha),tolower(as.character(x$manifest_sha256)))) {
  stop("Workflow 01 manifest SHA-256 does not match repository pointer",call.=FALSE)
}

tar_paths <- unlist(local)[grepl("\\.tar\\.gz$",unlist(local))]
for (p in tar_paths) utils::untar(p,exdir=output_root)

audit <- list(
  schema="living-evidence-map-workflow01-zenodo-restore-audit-v1",
  status="success",
  state=as.character(x$state),
  source_workflow01_run_id=as.character(x$github_run_id),
  zenodo_record_id=as.character(x$zenodo_record_id),
  doi=as.character(x$doi),
  pointer=basename(pointer_path),
  manifest_sha256=manifest_sha,
  restored_files=vapply(expected,function(z)z$filename,character(1))
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_root,"workflow01_zenodo_restore_audit.json"))
cat(sprintf("PASS: restored Workflow 01 %s state from Zenodo record %s\n",
            audit$state,audit$zenodo_record_id))
