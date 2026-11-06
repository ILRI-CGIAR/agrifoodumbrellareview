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
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

registry_path <- arg("--registry")
expected_source <- arg("--source")
output_path <- arg("--output")
if (any(vapply(list(registry_path,expected_source,output_path),is.null,logical(1)))) {
  stop("Required: --registry --source --output",call.=FALSE)
}
if (!file.exists(registry_path)) stop(sprintf("Registry not found: %s",registry_path),call.=FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)

scalar <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
or_else <- function(x,y) if (is.null(x)) y else x
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
perform <- function(req, expected, label, timeout=300) {
  resp <- req |>
    req_timeout(timeout) |>
    req_error(is_error=function(resp) FALSE) |>
    req_perform()
  st <- resp_status(resp)
  if (!(st %in% expected)) {
    body <- tryCatch(resp_body_string(resp),error=function(e)"")
    stop(sprintf("Zenodo %s returned HTTP %d: %s",label,st,body),call.=FALSE)
  }
  resp
}

local_registry <- fromJSON(registry_path,simplifyVector=FALSE)
source <- scalar(local_registry$database$short_name)
if (!identical(source,expected_source)) {
  stop(sprintf("Registry source %s does not match expected source %s",or_else(source,"<missing>"),expected_source),call.=FALSE)
}
doi <- scalar(local_registry$staging$reserved_doi)
dep_id <- scalar(local_registry$staging$deposition_id)
if (is.null(dep_id) && !is.null(doi)) dep_id <- sub("^.*\\.","",doi)
if (is.null(dep_id)) stop("Registry lacks resolvable Zenodo deposition ID",call.=FALSE)

legacy_url <- paste0("https://zenodo.org/api/deposit/depositions/",dep_id)
dep <- resp_body_json(
  perform(request(legacy_url)|>auth(),200L,"published record preflight",60),
  simplifyVector=FALSE
)
if (!isTRUE(dep$submitted)) stop(sprintf("Zenodo record %s is not published",dep_id),call.=FALSE)
if (!identical(scalar(dep$metadata$access_right),"restricted")) {
  stop(sprintf("Zenodo record %s is not restricted",dep_id),call.=FALSE)
}
candidate_dois <- unique(na.omit(c(
  scalar(dep$doi),
  scalar(dep$metadata$doi),
  scalar(dep$metadata$prereserve_doi$doi),
  scalar(dep$prereserve_doi$doi)
)))
if (!is.null(doi) && !(doi %in% candidate_dois)) stop("Registry DOI does not match published Zenodo record",call.=FALSE)

rdm_url <- paste0("https://zenodo.org/api/records/",dep_id)
record <- resp_body_json(
  perform(request(rdm_url)|>auth(),200L,"published RDM record",60),
  simplifyVector=FALSE
)
files_url <- scalar(record$links$files)
if (is.null(files_url)) files_url <- paste0(rdm_url,"/files")
files_body <- resp_body_json(
  perform(request(files_url)|>auth(),200L,"published file listing",60),
  simplifyVector=FALSE
)
entries <- if (!is.null(files_body$entries)) files_body$entries else list()
key_of <- function(x) scalar(or_else(x$key,or_else(x$filename,x$name)))
keys <- vapply(entries,function(x)or_else(key_of(x),""),character(1))
required <- c("source_registry.json","records.jsonl","manifest.json","SHA256SUMS")
missing <- setdiff(required,keys)

download_raw <- function(name, timeout=1800) {
  entry <- entries[[match(name,keys)]]
  url <- scalar(entry$links$content)
  if (is.null(url)) url <- paste0(files_url,"/",URLencode(name,reserved=TRUE),"/content")
  resp_body_raw(perform(request(url)|>auth(),200L,paste0("download ",name),timeout))
}

# Older manual searches contain pre-normalised W00 files. A newly published
# concept expansion may contain only the original restricted RIS exports.
# In that case, normalise ONLY this new archive in isolation; never replay the
# original September search or silently combine separate search events.
if (length(missing)) {
  if (length(missing) != length(required)) {
    stop(sprintf("Incomplete W00 derived-file set in Zenodo %s: %s",dep_id,paste(missing,collapse=", ")),call.=FALSE)
  }
  ris_keys <- keys[grepl("\\.ris$",keys,ignore.case=TRUE)]
  if (!length(ris_keys)) stop(sprintf("Zenodo %s has no W00 derived package or RIS files",dep_id),call.=FALSE)
  work <- tempfile(paste0("w00_new_ris_",expected_source,"_"))
  dir.create(work,recursive=TRUE)
  raw_paths <- file.path(work,basename(ris_keys))
  if (anyDuplicated(basename(ris_keys))) stop("Duplicate RIS basenames in published archive",call.=FALSE)
  for (i in seq_along(ris_keys)) writeBin(download_raw(ris_keys[[i]],1800),raw_paths[[i]])
  derived_registry <- local_registry
  derived_registry$input$files <- lapply(seq_along(raw_paths),function(i) {
    list(filename=basename(raw_paths[[i]]),bytes=unname(file.info(raw_paths[[i]])$size),
         sha256=digest(file=raw_paths[[i]],algo="sha256",serialize=FALSE))
  })
  derived_registry$input$expected_chunk_count <- length(raw_paths)
  registry_tmp <- file.path(work,"source_registry.json")
  writeLines(toJSON(derived_registry,auto_unbox=TRUE,pretty=TRUE,null="null"),registry_tmp)
  target <- file.path(work,"derived")
  commands <- c("scripts/updater/workflow_00_ris_ingestion.R",
                unlist(lapply(raw_paths,function(p)c("--ris",shQuote(p))),use.names=FALSE),
                "--registry",shQuote(registry_tmp),"--output-dir",shQuote(target))
  status <- system2(file.path(R.home("bin"),"Rscript"),commands)
  if (!identical(as.integer(status),0L)) stop("Published expansion RIS normalization failed",call.=FALSE)
  handoff <- file.path(target,"handoff","records.jsonl")
  manifest_path <- file.path(target,"manifest.json")
  if (!file.exists(handoff)) stop("No RIS handover was produced",call.=FALSE)
  manifest_files <- list.files(target,pattern="manifest\\.json$",recursive=TRUE,full.names=TRUE)
  if (!length(manifest_files)) stop("RIS normalization did not create a manifest",call.=FALSE)
  dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
  if (!file.copy(handoff,output_path,overwrite=TRUE)) stop("Cannot copy RIS handover",call.=FALSE)
  lines <- readLines(output_path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  expected <- suppressWarnings(as.integer(local_registry$search$reported_results))
  if (length(expected) && !is.na(expected) && length(lines)!=expected) {
    stop(sprintf("Published RIS count mismatch: expected %d, parsed %d",expected,length(lines)),call.=FALSE)
  }
  cat(sprintf("PASS: normalised new published RIS expansion source=%s doi=%s records=%d\\n",
              expected_source,doi,length(lines)))
  quit(save="no",status=0)
}

archived_registry_raw <- download_raw("source_registry.json",120)
archived_registry <- fromJSON(rawToChar(archived_registry_raw),simplifyVector=FALSE)
archived_source <- scalar(archived_registry$database$short_name)
if (!identical(archived_source,expected_source)) stop("Archived source_registry source mismatch",call.=FALSE)

manifest <- fromJSON(rawToChar(download_raw("manifest.json",120)),simplifyVector=FALSE)
if (!identical(scalar(manifest$source),expected_source)) stop("Archived manifest source mismatch",call.=FALSE)
expected_records <- as.integer(manifest$records$unique_records_for_handover)
if (is.na(expected_records) || expected_records < 0L) stop("Archived manifest lacks valid unique record count",call.=FALSE)

checksums <- strsplit(rawToChar(download_raw("SHA256SUMS",120)),"\n",fixed=TRUE)[[1L]]
checksums <- checksums[nzchar(trimws(checksums))]
parts <- strsplit(checksums,"  ",fixed=TRUE)
checksum_map <- setNames(
  vapply(parts,function(z)z[[1L]],character(1)),
  vapply(parts,function(z)basename(z[[2L]]),character(1))
)
if (!("records.jsonl" %in% names(checksum_map))) stop("SHA256SUMS lacks records.jsonl",call.=FALSE)

records_raw <- download_raw("records.jsonl",3600)
dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
writeBin(records_raw,output_path)

actual_sha <- digest(file=output_path,algo="sha256",serialize=FALSE)
if (!identical(tolower(actual_sha),tolower(checksum_map[["records.jsonl"]]))) {
  stop("records.jsonl SHA-256 mismatch against archived SHA256SUMS",call.=FALSE)
}

lines <- readLines(output_path,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
if (length(lines) != expected_records) {
  stop(sprintf("records.jsonl count mismatch: manifest=%d actual=%d",expected_records,length(lines)),call.=FALSE)
}

for (i in seq_along(lines)) {
  r <- fromJSON(lines[[i]],simplifyVector=FALSE)
  p <- scalar((r$source %||% list())$provider)
  rid <- scalar((r$sidecar_identity %||% list())$sidecar_record_id)
  if (!identical(p,expected_source)) stop(sprintf("Line %d provider mismatch: %s",i,or_else(p,"<missing>")),call.=FALSE)
  if (is.null(rid) || !nzchar(rid)) stop(sprintf("Line %d lacks sidecar source_record_id",i),call.=FALSE)
}

cat(sprintf(
  "PASS: restored published restricted manual RIS source=%s records=%d sha256=%s\n",
  expected_source,length(lines),actual_sha
))
