#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

pointer_path <- arg("--pointer")
output_root <- arg("--output-root")
source_filter <- arg("--sources","")
expected_review_id <- arg("--review-id","")
if (is.null(pointer_path) || is.null(output_root)) {
  stop("Required: --pointer --output-root",call.=FALSE)
}
if (!file.exists(pointer_path)) stop("Zenodo pointer file not found",call.=FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)

x <- fromJSON(pointer_path,simplifyVector=FALSE)
if (!identical(x$status,"published")) stop("Zenodo pointer is not a published archive",call.=FALSE)
if (nzchar(expected_review_id) && !identical(as.character(x$review_id),expected_review_id)) stop("W00 Zenodo receipt review identity mismatch",call.=FALSE)
if (!identical(x$visibility,"restricted")) stop("Expected restricted Workflow 00 archive",call.=FALSE)
if (is.null(x$zenodo_deposition_id) || !nzchar(as.character(x$zenodo_deposition_id))) {
  stop("Pointer lacks zenodo_deposition_id",call.=FALSE)
}

sources <- unlist(x$sources,use.names=FALSE)
if (length(sources)==1L && grepl(",",sources,fixed=TRUE)) {
  sources <- strsplit(sources,",",fixed=TRUE)[[1L]]
}
sources <- trimws(as.character(sources))
sources <- sources[nzchar(sources)]
if (nzchar(source_filter)) {
  wanted <- trimws(strsplit(source_filter,",",fixed=TRUE)[[1L]])
  missing <- setdiff(wanted,sources)
  if (length(missing)) stop(sprintf("Requested source(s) absent from archive: %s",paste(missing,collapse=", ")),call.=FALSE)
  sources <- wanted
}
if (!length(sources)) stop("No sources selected from pointer",call.=FALSE)

archive_files <- x$archive_files
if (is.null(archive_files) || !length(archive_files)) stop("Pointer lacks archive_files",call.=FALSE)

as_file_list <- function(z) {
  if (is.data.frame(z)) {
    lapply(seq_len(nrow(z)),function(i) as.list(z[i,,drop=FALSE]))
  } else if (is.list(z) && !is.null(z$filename)) {
    list(z)
  } else z
}
archive_files <- as_file_list(archive_files)

expected <- lapply(archive_files,function(z) {
  list(
    filename=as.character(z$filename),
    bytes=as.numeric(z$bytes),
    sha256=as.character(z$sha256),
    zenodo_checksum=if (is.null(z$zenodo_checksum)) NA_character_ else as.character(z$zenodo_checksum)
  )
})
names(expected) <- vapply(expected,function(z) z$filename,character(1))

wanted_names <- c(
  vapply(sources,function(src)
    sprintf("LivingEvidenceMap_workflow00_run-%s_%s-harvest.tar.gz",x$github_run_id,src),
    character(1)),
  sprintf("LivingEvidenceMap_workflow00_run-%s_manifest.json",x$github_run_id)
)
missing_expected <- setdiff(wanted_names,names(expected))
if (length(missing_expected)) {
  stop(sprintf("Pointer does not describe required file(s): %s",paste(missing_expected,collapse=", ")),call.=FALSE)
}

api <- paste0(
  "https://zenodo.org/api/deposit/depositions/",
  as.character(x$zenodo_deposition_id)
)
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
perform <- function(req,expected_status,label,timeout=600) {
  resp <- req |>
    req_timeout(timeout) |>
    req_error(is_error=function(resp) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (!(status %in% expected_status)) {
    body <- tryCatch(resp_body_string(resp),error=function(e) "")
    stop(sprintf("Zenodo %s returned HTTP %d: %s",label,status,body),call.=FALSE)
  }
  resp
}

dep <- perform(
  request(api) |> auth(),
  200L,
  "deposition read",
  120
) |> resp_body_json(simplifyVector=FALSE)

if (!isTRUE(dep$submitted)) stop("Zenodo deposition is not published/submitted",call.=FALSE)
remote_files <- dep$files
if (is.null(remote_files)) remote_files <- list()
remote_names <- vapply(remote_files,function(z) {
  if (!is.null(z$filename)) as.character(z$filename)
  else if (!is.null(z$key)) as.character(z$key)
  else ""
},character(1))
names(remote_files) <- remote_names

dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
download_dir <- file.path(output_root,".workflow00_zenodo_download")
dir.create(download_dir,recursive=TRUE,showWarnings=FALSE)

download_one <- function(filename) {
  if (!(filename %in% names(remote_files))) {
    stop(sprintf("Zenodo deposition lacks required file: %s",filename),call.=FALSE)
  }
  rf <- remote_files[[filename]]
  bucket <- dep$links$bucket
  if (is.null(bucket) || !nzchar(bucket)) stop("Published deposition lacks bucket link",call.=FALSE)
  url <- paste0(sub("/$","",bucket),"/",URLencode(filename,reserved=TRUE))

  dest <- file.path(download_dir,filename)
  resp <- request(url) |>
    req_method("GET") |>
    auth() |>
    req_timeout(1800) |>
    req_error(is_error=function(resp) FALSE) |>
    req_perform(path=dest)
  if (resp_status(resp) != 200L) stop(sprintf("Download failed for %s: HTTP %d",filename,resp_status(resp)),call.=FALSE)

  e <- expected[[filename]]
  bytes <- unname(file.info(dest)$size)
  sha <- digest(file=dest,algo="sha256",serialize=FALSE)
  if (!identical(as.numeric(bytes),as.numeric(e$bytes))) {
    stop(sprintf("Byte-size mismatch for %s: expected %s got %s",filename,e$bytes,bytes),call.=FALSE)
  }
  if (!identical(tolower(sha),tolower(e$sha256))) {
    stop(sprintf("SHA-256 mismatch for %s",filename),call.=FALSE)
  }
  if (!is.na(e$zenodo_checksum) && nzchar(e$zenodo_checksum)) {
    md5_expected <- sub("^md5:","",tolower(e$zenodo_checksum))
    md5_actual <- digest(file=dest,algo="md5",serialize=FALSE)
    if (!identical(md5_actual,md5_expected)) stop(sprintf("MD5 mismatch for %s",filename),call.=FALSE)
  }
  dest
}

manifest_name <- sprintf("LivingEvidenceMap_workflow00_run-%s_manifest.json",x$github_run_id)
manifest_local <- download_one(manifest_name)
manifest_sha <- digest(file=manifest_local,algo="sha256",serialize=FALSE)
if (!is.null(x$manifest_sha256) && !identical(tolower(manifest_sha),tolower(as.character(x$manifest_sha256)))) {
  stop("Manifest SHA-256 does not match repository pointer",call.=FALSE)
}

for (src in sources) {
  filename <- sprintf("LivingEvidenceMap_workflow00_run-%s_%s-harvest.tar.gz",x$github_run_id,src)
  tar_path <- download_one(filename)
  utils::untar(tar_path,exdir=output_root)
  src_dir <- file.path(output_root,src)
  if (!dir.exists(src_dir) || !length(list.files(src_dir,recursive=TRUE,all.files=TRUE,no..=TRUE))) {
    stop(sprintf("Extracted archive for %s is empty or incorrectly structured",src),call.=FALSE)
  }
  cat(sprintf("PASS: restored Workflow 00 source %s from Zenodo record %s\n",
              src,as.character(x$zenodo_record_id)))
}

audit <- list(
  status="success",
  workflow00_run_id=as.character(x$github_run_id),
  zenodo_record_id=as.character(x$zenodo_record_id),
  doi=as.character(x$doi),
  sources=sources,
  repository_pointer=basename(pointer_path),
  manifest_sha256=manifest_sha
)
writeLines(
  toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
  file.path(output_root,"workflow00_zenodo_restore_audit.json")
)
