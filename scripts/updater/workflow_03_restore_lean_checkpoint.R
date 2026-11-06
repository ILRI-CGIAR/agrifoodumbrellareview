#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
pointer <- arg("--pointer")
out <- arg("--output-dir")
if(is.null(pointer)||is.null(out)) stop("Required: --pointer --output-dir",call.=FALSE)

x <- fromJSON(pointer,simplifyVector=FALSE)
if(!identical(x$status,"published") || !identical(x$visibility,"restricted") ||
   !identical(x$state,"post_w02_lean_canonical_checkpoint")){
  stop("Invalid lean canonical checkpoint pointer",call.=FALSE)
}
expected_sha <- tolower(as.character(x$lean_canonical_sha256))
if(!nzchar(expected_sha)) stop("Lean checkpoint pointer lacks lean_canonical_sha256",call.=FALSE)
expected_records <- suppressWarnings(as.integer(x$canonical_records))
if(is.na(expected_records) || expected_records < 1L) stop("Lean checkpoint pointer lacks a valid canonical_records count",call.=FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN required",call.=FALSE)
dep_id <- as.character(x$zenodo_deposition_id)
api <- paste0("https://zenodo.org/api/deposit/depositions/",dep_id)
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
dep <- request(api) |> auth() |> req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector=FALSE)
if(!isTRUE(dep$submitted)) stop("Lean checkpoint Zenodo deposition is not published",call.=FALSE)
bucket <- as.character(dep$links$bucket)

files <- x$archive_files
if(is.data.frame(files)) files <- lapply(seq_len(nrow(files)),function(i)as.list(files[i,,drop=FALSE]))
if(is.list(files) && !is.null(files$filename)) files <- list(files)
archive_meta <- Filter(function(z) grepl("\\.tar\\.gz$",as.character(z$filename)),files)
if(length(archive_meta)!=1L) stop("Expected exactly one lean checkpoint tar.gz",call.=FALSE)

dir.create(out,recursive=TRUE,showWarnings=FALSE)
dl <- file.path(out,".download"); dir.create(dl,showWarnings=FALSE)
z <- archive_meta[[1L]]
fn <- as.character(z$filename); dest <- file.path(dl,fn)
resp <- request(paste0(sub("/$","",bucket),"/",URLencode(fn,reserved=TRUE))) |>
  req_method("GET") |> auth() |> req_timeout(1800) |>
  req_error(is_error=function(resp)FALSE) |> req_perform(path=dest)
if(resp_status(resp)!=200L) stop(sprintf("Download failed for %s",fn),call.=FALSE)
if(unname(file.info(dest)$size)!=as.numeric(z$bytes)) stop("Lean archive byte mismatch",call.=FALSE)
actual_archive_sha <- digest(file=dest,algo="sha256",serialize=FALSE)
if(tolower(actual_archive_sha)!=tolower(as.character(z$sha256))) stop("Lean archive SHA mismatch",call.=FALSE)

extract <- file.path(out,"extract"); dir.create(extract,showWarnings=FALSE)
utils::untar(dest,exdir=extract)
hits <- list.files(extract,pattern="^canonical_lean\\.jsonl$",recursive=TRUE,full.names=TRUE)
reports <- list.files(extract,pattern="^compaction_report\\.json$",recursive=TRUE,full.names=TRUE)
if(length(hits)!=1L || length(reports)!=1L) stop("Restored lean archive has unexpected contents",call.=FALSE)

lean <- file.path(out,"canonical_lean.jsonl")
report <- file.path(out,"compaction_report.json")
file.copy(hits[[1L]],lean,overwrite=TRUE)
file.copy(reports[[1L]],report,overwrite=TRUE)
actual_sha <- digest(file=lean,algo="sha256",serialize=FALSE)
if(!identical(tolower(actual_sha),expected_sha)) stop(sprintf("Lean canonical SHA mismatch: %s",actual_sha),call.=FALSE)

r <- fromJSON(report,simplifyVector=FALSE)
stopifnot(identical(r$status,"PASS"),as.integer(r$canonical_records)==expected_records,
          isTRUE(r$record_ids_unchanged),isTRUE(r$non_manifestation_fields_unchanged),
          isTRUE(r$manifestation_refs_exact),
          identical(tolower(as.character(r$output_lean_canonical_sha256)),expected_sha))

audit <- list(
  schema="living-evidence-map-workflow03-lean-checkpoint-restore-v1",
  status="PASS",
  source_pointer=pointer,
  zenodo_record_id=as.character(x$zenodo_record_id),
  lean_canonical_sha256=actual_sha,
  canonical_records=as.integer(r$canonical_records),
  restored_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),file.path(out,"workflow03_restore_audit.json"),useBytes=TRUE)
cat(sprintf("PASS: Workflow 03 restored lean canonical checkpoint; records=%d SHA256=%s\n",as.integer(r$canonical_records),actual_sha))
