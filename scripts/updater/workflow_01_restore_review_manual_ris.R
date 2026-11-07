#!/usr/bin/env Rscript
# Restore new Shiny-uploaded manual RIS archives to the existing W01 sidecar schema.
# Separate from the legacy manual JSONL restoration mechanism.
suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
  library(digest)
})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k) {
  i <- which(args==k)
  if(length(i)!=1L || i==length(args)) stop("Required option: ",k,call.=FALSE)
  args[[i+1L]]
}
manifest_path <- arg("--input-manifest")
output_root <- arg("--output-root")
if(!file.exists(manifest_path)) stop("Input manifest not found",call.=FALSE)
m <- fromJSON(manifest_path,simplifyVector=FALSE)
if(!identical(m$schema,"living-evidence-map-workflow01-review-input-references-v1"))
  stop("Unsupported W01 review input manifest",call.=FALSE)
review <- as.character(m$review_id)
if(!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{3,127}$",review))
  stop("Invalid review ID",call.=FALSE)
if(dir.exists(output_root) || file.exists(output_root))
  stop("Output directory must not already exist",call.=FALSE)
token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)
manual <- m$manual_ris
if(is.null(manual)) manual <- list()
ids <- vapply(manual,function(z)as.character(z$submission_id),character(1))
if(anyDuplicated(ids)) stop("Repeated manual submission",call.=FALSE)
auth <- function(url) req_headers(request(url),Authorization=paste("Bearer",token))
fetch_file <- function(url,destination) {
  resp <- auth(url) |> req_timeout(1800) |>
    req_error(is_error=function(resp)FALSE) |> req_perform(path=destination)
  if(resp_status(resp)!=200L) stop("Zenodo download failed HTTP ",resp_status(resp),call.=FALSE)
}
dir.create(output_root,recursive=TRUE)
outputs <- list()
for(j in seq_along(manual)){
  z <- manual[[j]]
  rec <- z$receipt
  if(!identical(as.character(rec$review_id),review) ||
     !identical(as.character(rec$submission_id),as.character(z$submission_id)) ||
     !identical(as.character(rec$zenodo_record_id),as.character(z$zenodo_record_id)) ||
     !identical(as.character(rec$status),"published"))
    stop("Manual RIS receipt identity mismatch",call.=FALSE)
  dep_id <- as.character(rec$deposition_id)
  if(!grepl("^[0-9]+$",dep_id)) stop("Invalid Zenodo deposition",call.=FALSE)
  dep <- auth(paste0("https://zenodo.org/api/deposit/depositions/",dep_id)) |>
    req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector=FALSE)
  if(!isTRUE(dep$submitted) ||
     !identical(as.character(dep$metadata$access_right),"restricted"))
    stop("Manual RIS archive must be published and restricted",call.=FALSE)
  bucket <- as.character(dep$links$bucket)
  if(!grepl("^https://zenodo.org/",bucket))
    stop("Unexpected Zenodo download bucket",call.=FALSE)
  folder <- file.path(output_root,paste0("submission-",as.character(z$submission_id)))
  ris_dir <- file.path(folder,"ris")
  dir.create(ris_dir,recursive=TRUE)
  files <- rec$files
  for(f in files){
    filename <- as.character(f$archive_name)
    expected <- tolower(as.character(f$sha256))
    if(!grepl("^[0-9]{3}_[A-Za-z0-9._-]+\\.[Rr][Ii][Ss]$",filename) ||
       !grepl("^[0-9a-f]{64}$",expected)) stop("Unsafe RIS archive filename/checksum",call.=FALSE)
    dest <- file.path(ris_dir,filename)
    fetch_file(paste0(sub("/$","",bucket),"/",URLencode(filename,reserved=TRUE)),dest)
    if(!identical(tolower(digest(file=dest,algo="sha256",serialize=FALSE)),expected))
      stop("RIS archive checksum mismatch",call.=FALSE)
  }
  source <- paste0("manual_",substr(digest(as.character(z$submission_id),algo="sha256",serialize=FALSE),1L,16L))
  target <- file.path(folder,"records.jsonl")
  cmd <- system2(file.path(R.home("bin"),"Rscript"),
    c("scripts/updater/workflow_01_generic_ris_to_sidecar.R",
      "--input-dir",shQuote(ris_dir),"--source",source,
      "--submission-id",shQuote(as.character(z$submission_id)),
      "--output",shQuote(target)))
  if(!identical(as.integer(cmd),0L)) stop("RIS conversion failed",call.=FALSE)
  lines <- readLines(target,warn=FALSE,encoding="UTF-8")
  count <- sum(nzchar(trimws(lines)))
  expected_count <- as.integer(z$records_downloaded)
  if(is.na(expected_count) || count!=expected_count)
    stop("RIS conversion count mismatch for submission ",z$submission_id,call.=FALSE)
  outputs[[length(outputs)+1L]] <- list(submission_id=z$submission_id,
    zenodo_record_id=z$zenodo_record_id,source=source,
    records=count,path=target,
    sha256=digest(file=target,algo="sha256",serialize=FALSE))
}
write_json(list(schema="living-evidence-map-workflow01-restored-manual-ris-v1",
  review_id=review,sources=outputs),file.path(output_root,"manifest.json"),
  auto_unbox=TRUE,pretty=TRUE,null="null")
cat(sprintf("PASS: restored %d manual RIS submissions\\n",length(outputs)))
