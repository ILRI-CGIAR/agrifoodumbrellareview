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
or_else <- function(x,y) if (is.null(x)) y else x

staging_root <- normalizePath(arg("--staging-root"),mustWork=TRUE)
run_id <- arg("--run-id")
run_type <- arg("--run-type")
search_version <- arg("--search-version")
sources <- strsplit(arg("--sources",""),",",fixed=TRUE)[[1L]]
sources <- sources[nzchar(sources)]
repository <- arg("--repository")
output_dir <- arg("--output-dir")
ebsco_config_path <- arg("--ebsco-config","config/workflow00_ebsco_sources.json")
zenodo_config_path <- arg("--zenodo-config")
validate_only <- identical(tolower(arg("--validate-only","false")),"true")
if(is.null(zenodo_config_path)||!file.exists(zenodo_config_path))
  stop("Provide an explicit project Zenodo metadata config via --zenodo-config",call.=FALSE)
zenodo_cfg <- fromJSON(zenodo_config_path,simplifyVector=FALSE)
if(is.null(zenodo_cfg$creators)||!length(zenodo_cfg$creators))
  stop("Zenodo metadata config requires project creators",call.=FALSE)
project_title <- as.character(or_else(zenodo_cfg$title,""))
if(!nzchar(trimws(project_title)))stop("Zenodo metadata config requires project title",call.=FALSE)
archive_prefix <- gsub("[^a-zA-Z0-9_-]+","_",basename(repository))
if(!nzchar(archive_prefix))stop("Invalid repository archive prefix",call.=FALSE)

if (any(vapply(list(run_id,run_type,search_version,repository,output_dir),is.null,logical(1))) || !length(sources)) {
  stop("Required: --staging-root --run-id --run-type --search-version --sources --repository --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
output_dir <- normalizePath(output_dir,mustWork=TRUE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!validate_only && !nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)

if (any(!grepl("^[a-z0-9][a-z0-9_-]*$",sources)) || anyDuplicated(sources))
  stop("Archive sources must be unique stable lower-case identifiers",call.=FALSE)

api <- "https://zenodo.org/api/deposit/depositions"
marker <- paste0(archive_prefix,"-workflow00-run-",run_id)
run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
archive_dir <- file.path(output_dir,"archive_files")
dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)

all_paths <- list.files(staging_root,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if (length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))

make_tar <- function(base,members,out) {
  members <- members[file.exists(file.path(base,members))]
  if (!length(members)) return(FALSE)
  old <- setwd(base); on.exit(setwd(old),add=TRUE)
  utils::tar(out,files=members,compression="gzip",tar="internal")
  if (!file.exists(out) || file.info(out)$size <= 0) stop(sprintf("Failed to create %s",out),call.=FALSE)
  TRUE
}

tree_sha256 <- function(root) {
  files <- list.files(root,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (!length(files)) return(NA_character_)
  rel <- substring(files,nchar(normalizePath(root,mustWork=TRUE))+2L)
  ord <- order(rel); files <- files[ord]; rel <- rel[ord]
  lines <- vapply(seq_along(files),function(i) paste(rel[[i]],digest(file=files[[i]],algo="sha256",serialize=FALSE),sep="\t"),character(1))
  digest(paste(lines,collapse="\n"),algo="sha256",serialize=FALSE)
}
handoff_sources <- list()
for (src in sources) {
  src_dir <- file.path(staging_root,"harvests",src)
  if (!dir.exists(src_dir)) stop(sprintf("Staged harvest missing for %s",src),call.=FALSE)
  handoff_sources[[src]] <- list(tree_sha256=tree_sha256(src_dir))
}

archive_paths <- character()
for (src in sources) {
  src_dir <- file.path(staging_root,"harvests",src)
  if (!dir.exists(src_dir)) stop(sprintf("Staged harvest missing for %s",src),call.=FALSE)
  out <- file.path(archive_dir,sprintf("%s_workflow00_run-%s_%s-harvest.tar.gz",archive_prefix,run_id,src))
  make_tar(file.path(staging_root,"harvests"),src,out)
  archive_paths <- c(archive_paths,out)
}

doc_members <- c("plan","search_records")
if (dir.exists(file.path(staging_root,"expansion_reconciliation"))) {
  doc_members <- c(doc_members,"expansion_reconciliation")
}
doc_out <- file.path(archive_dir,sprintf("%s_workflow00_run-%s_search-documentation.tar.gz",archive_prefix,run_id))
make_tar(staging_root,doc_members,doc_out)
archive_paths <- c(archive_paths,doc_out)

file_meta <- lapply(archive_paths,function(p) list(
  filename=basename(p),
  bytes=unname(file.info(p)$size),
  sha256=digest(file=p,algo="sha256",serialize=FALSE)
))
names(file_meta) <- basename(archive_paths)

manifest <- list(
  schema="living-evidence-map-workflow00-search-archive-v1",
  github_run_id=run_id,
  github_run_url=run_url,
  archive_prefix=archive_prefix,
  repository=repository,
  archive_prefix=archive_prefix,
  run_type=run_type,
  search_version=search_version,
  sources=sources,
  handoff_sources=handoff_sources,
  file_visibility="restricted",
  files=file_meta
)
manifest_path <- file.path(archive_dir,sprintf("%s_workflow00_run-%s_manifest.json",archive_prefix,run_id))
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),manifest_path)
archive_paths <- c(archive_paths,manifest_path)

total_bytes <- sum(file.info(archive_paths)$size)
if (!is.finite(total_bytes) || total_bytes <= 0) stop("Archive payload is empty",call.=FALSE)
if (total_bytes > 49 * 1024^3) stop("Archive payload exceeds 49 GiB preflight limit",call.=FALSE)
if (length(archive_paths) > 100L) stop("Archive payload exceeds Zenodo 100-file limit",call.=FALSE)

# Use the exact legacy-deposition semantics already validated for the
# deduplication large-artifact uploader in this repository.
auth <- function(req) req |> req_headers(Authorization = paste("Bearer", token))

perform <- function(req, expected, label, timeout = 600) {
  resp <- req |>
    req_timeout(timeout) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (!(status %in% expected)) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "")
    stop(sprintf("Zenodo %s returned HTTP %d: %s", label, status, body), call. = FALSE)
  }
  resp
}

expected_title <- sprintf("%s | Search archive | %s | run %s",
  project_title,run_type,run_id)

description <- paste0(
  "<p>Source search archive for a reproducible evidence-synthesis project.</p>",
  "<p>This record corresponds to GitHub Actions run <a href=\"",run_url,"\">",run_id,"</a> and ",
  "contains the downloaded source search outputs, source manifests, search-plan documentation and provenance records ",
  "for the selected databases. Files are restricted because source database/API terms may limit redistribution.</p>",
  "<p>Run type: ",run_type,". Search strategy version: ",search_version,". Sources: ",
  paste(sources,collapse=", "),".</p>"
)

metadata <- list(metadata = list(
  title = expected_title,
  upload_type = "dataset",
  publication_date = format(Sys.Date(), "%Y-%m-%d"),
  description = description,
  creators = zenodo_cfg$creators,
  access_right = "restricted",
  access_conditions = paste(
    "Files contain database/API search-result exports and are restricted",
    "because source licensing or terms may limit redistribution.",
    "Access may be granted by the depositor where permitted."
  ),
  keywords = list(
    project_title,
    "evidence synthesis",
    "search archive",
    "Workflow 00",
    marker
  ),
  notes = paste0(
    "Source GitHub Actions run: ",run_id,
    ". Search strategy version: ",search_version,
    ". Sources: ",paste(sources,collapse=", "),"."
  )
))

if(validate_only){
  writeLines(toJSON(list(status="validated_not_published",sources=sources,
    archive_prefix=archive_prefix,files=file_meta,manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE)),
    auto_unbox=TRUE,pretty=TRUE,null="null"),
    file.path(output_dir,"offline_validation.json"))
  cat(sprintf("PASS: W00 offline archive package for %d source(s); zero Zenodo requests\n",length(sources)))
  quit(save="no",status=0L)
}
cat("ZENODO CREATE\n")
created_resp <- perform(
  request(api) |>
    req_method("POST") |>
    auth() |>
    req_headers("Content-Type" = "application/json") |>
    req_body_raw(charToRaw("{}"), type = "application/json"),
  201L,
  "draft creation",
  60
)
dep <- resp_body_json(created_resp, simplifyVector = FALSE)
dep_id <- as.character(dep$id)
bucket <- as.character(dep$links$bucket)
if (!nzchar(dep_id) || !nzchar(bucket)) {
  stop("Zenodo draft response missing id/bucket",call.=FALSE)
}
cat(sprintf("ZENODO DRAFT id=%s\n",dep_id))

writeLines(
  toJSON(
    list(
      status="draft_created",
      github_run_id=run_id,
      zenodo_deposition_id=dep_id
    ),
    auto_unbox=TRUE,
    pretty=TRUE
  ),
  file.path(output_dir,"zenodo_draft_receipt.json")
)

perform(
  request(paste0(api,"/",dep_id)) |>
    req_method("PUT") |>
    auth() |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(metadata, auto_unbox = TRUE),
  200L,
  "metadata update",
  60
)

uploaded <- vector("list",length(archive_paths))
for (i in seq_along(archive_paths)) {
  p <- archive_paths[[i]]
  fn <- basename(p)
  upload_url <- paste0(bucket,"/",URLencode(fn,reserved=TRUE))
  cat(sprintf("ZENODO UPLOAD %s bytes=%s\n",fn,file.info(p)$size))
  upload_resp <- perform(
    request(upload_url) |>
      req_method("PUT") |>
      auth() |>
      req_headers(Expect = "") |>
      req_body_file(p),
    c(200L,201L),
    paste0("file upload: ",fn),
    1800
  )
  uploaded[[i]] <- resp_body_json(upload_resp,simplifyVector=FALSE)
}

pub_resp <- perform(
  request(paste0(api,"/",dep_id,"/actions/publish")) |>
    req_method("POST") |>
    auth(),
  c(200L,201L,202L),
  "publish",
  120
)
published <- resp_body_json(pub_resp,simplifyVector=FALSE)
cat(sprintf("ZENODO PUBLISHED id=%s\n",dep_id))

receipt <- list(
  status="published",
  github_run_id=run_id,
  github_run_url=run_url,
  run_type=run_type,
  search_version=search_version,
  sources=sources,
  handoff_sources=handoff_sources,
  zenodo_record_id=as.character(if (is.null(published$record_id)) published$id else published$record_id),
  zenodo_deposition_id=dep_id,
  doi=if (is.null(published$doi)) NA_character_ else published$doi,
  record_url=if (!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",dep_id),
  title=expected_title,
  visibility="restricted",
  total_bytes=total_bytes,
  archive_files=lapply(seq_along(archive_paths),function(i) {
    p <- archive_paths[[i]]
    z <- uploaded[[i]]
    list(
      filename=basename(p),
      bytes=unname(file.info(p)$size),
      sha256=digest(file=p,algo="sha256",serialize=FALSE),
      zenodo_checksum=if (is.null(z$checksum)) NULL else z$checksum
    )
  }),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)

writeLines(
  toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
  file.path(output_dir,"zenodo_receipt.json")
)
cat(sprintf(
  "PASS: published restricted Zenodo search archive %s for Workflow 00 run %s\n",
  receipt$zenodo_record_id,run_id
))
