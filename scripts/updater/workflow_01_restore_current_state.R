#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr2)
  library(digest)
})


args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

latest_pointer <- normalizePath(arg("--pointer"),mustWork=TRUE)
pointer_dir <- normalizePath(arg("--pointer-dir"),mustWork=TRUE)
output_root <- arg("--output-root")
if(is.null(output_root)) stop("Required: --pointer --pointer-dir --output-root",call.=FALSE)
if(file.exists(output_root)) stop("Output root already exists",call.=FALSE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is required",call.=FALSE)

read_pointer <- function(path){
  x <- fromJSON(path,simplifyVector=FALSE)
  if(!identical(x$status,"published")) stop(sprintf("Pointer is not published: %s",path),call.=FALSE)
  if(!identical(x$visibility,"restricted")) stop(sprintf("Pointer is not restricted: %s",path),call.=FALSE)
  if(is.null(x$state) || !(x$state %in% c("final","delta"))) stop(sprintf("Unsupported Workflow 01 pointer state: %s",x$state),call.=FALSE)
  x
}

# Traverse pointer lineage backwards to the immutable full baseline.
chain_paths <- character()
chain <- list()
p_path <- latest_pointer
repeat{
  p <- read_pointer(p_path)
  chain_paths <- c(chain_paths,p_path)
  chain[[length(chain)+1L]] <- p
  if(identical(p$state,"final")) break
  prev_run <- as.character(p$previous_github_run_id %||% "")
  prev_record <- as.character(p$previous_zenodo_record_id %||% "")
  if(!nzchar(prev_run)||!nzchar(prev_record)) stop("Delta pointer lacks previous run/Zenodo lineage",call.=FALSE)
  prev_path <- file.path(pointer_dir,paste0("run-",prev_run,".json"))
  if(!file.exists(prev_path)) stop(sprintf("Previous Workflow 01 pointer missing: %s",prev_path),call.=FALSE)
  prev <- read_pointer(prev_path)
  if(!identical(as.character(prev$zenodo_record_id),prev_record)) {
    stop(sprintf("Delta lineage mismatch: expected previous Zenodo record %s, pointer has %s",
                 prev_record,as.character(prev$zenodo_record_id)),call.=FALSE)
  }
  p_path <- prev_path
}
chain <- rev(chain)
chain_paths <- rev(chain_paths)
if(!identical(chain[[1L]]$state,"final")) stop("Workflow 01 lineage does not terminate in a full final baseline",call.=FALSE)

work <- tempfile("workflow01_restore_")
dir.create(work,recursive=TRUE)
on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
state_dir <- file.path(work,"state")
baseline_pointer <- chain_paths[[1L]]

# Use the checksum-verifying baseline restorer.
status <- system2("Rscript",c(
  "scripts/updater/workflow_01_restore_state_from_zenodo.R",
  "--pointer",baseline_pointer,
  "--output-root",state_dir
))
if(status!=0L) stop("Failed to restore Workflow 01 full baseline",call.=FALSE)

auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
download_delta <- function(pointer, dest_root){
  dep_id <- as.character(pointer$zenodo_deposition_id)
  if(!nzchar(dep_id)) stop("Delta pointer lacks zenodo_deposition_id",call.=FALSE)
  files <- pointer$archive_files
  if(is.data.frame(files)) files <- lapply(seq_len(nrow(files)),function(i)as.list(files[i,,drop=FALSE]))
  if(is.list(files)&&!is.null(files$filename)) files <- list(files)
  if(!length(files)) stop("Delta pointer lacks archive_files",call.=FALSE)

  api <- paste0("https://zenodo.org/api/deposit/depositions/",dep_id)
  dep <- request(api) |> auth() |> req_timeout(120) |> req_perform() |> resp_body_json(simplifyVector=FALSE)
  if(!isTRUE(dep$submitted)) stop(sprintf("Zenodo delta %s is not published",dep_id),call.=FALSE)
  bucket <- as.character(dep$links$bucket)
  if(!nzchar(bucket)) stop("Published delta deposition lacks bucket link",call.=FALSE)

  dir.create(dest_root,recursive=TRUE,showWarnings=FALSE)
  downloads <- file.path(dest_root,"downloads")
  dir.create(downloads,showWarnings=FALSE)
  local <- character()
  for(z in files){
    fn <- as.character(z$filename)
    dest <- file.path(downloads,fn)
    url <- paste0(sub("/$","",bucket),"/",URLencode(fn,reserved=TRUE))
    resp <- request(url) |> req_method("GET") |> auth() |> req_timeout(1800) |>
      req_error(is_error=function(resp)FALSE) |> req_perform(path=dest)
    if(resp_status(resp)!=200L) stop(sprintf("Delta download failed for %s",fn),call.=FALSE)
    if(as.numeric(file.info(dest)$size)!=as.numeric(z$bytes)) stop(sprintf("Delta byte mismatch for %s",fn),call.=FALSE)
    actual_sha <- digest(file=dest,algo="sha256",serialize=FALSE)
    if(!identical(tolower(actual_sha),tolower(as.character(z$sha256)))) stop(sprintf("Delta SHA mismatch for %s",fn),call.=FALSE)
    if(!is.null(z$zenodo_checksum) && nzchar(as.character(z$zenodo_checksum))){
      expected_md5 <- sub("^md5:","",tolower(as.character(z$zenodo_checksum)))
      actual_md5 <- digest(file=dest,algo="md5",serialize=FALSE)
      if(!identical(actual_md5,expected_md5)) stop(sprintf("Delta MD5 mismatch for %s",fn),call.=FALSE)
    }
    local <- c(local,dest)
  }
  tar <- local[grepl("\\.tar\\.gz$",local)]
  if(length(tar)!=1L) stop("Expected exactly one delta tar.gz",call.=FALSE)
  extract <- file.path(dest_root,"extract")
  dir.create(extract,showWarnings=FALSE)
  utils::untar(tar,exdir=extract)
  manifests <- list.files(extract,pattern="^delta_manifest\\.json$",recursive=TRUE,full.names=TRUE)
  if(length(manifests)!=1L) stop("Expected exactly one restored delta_manifest.json",call.=FALSE)
  dirname(manifests[[1L]])
}

if(length(chain)>1L){
  for(i in 2:length(chain)){
    p <- chain[[i]]
    if(!identical(p$state,"delta")) stop("Only the first lineage member may be full final state",call.=FALSE)
    delta_root <- download_delta(p,file.path(work,paste0("delta-",i)))
    next_state <- file.path(work,paste0("state-",i))
    status <- system2("Rscript",c(
      "scripts/updater/workflow_01_replay_delta.R",
      "--previous-root",state_dir,
      "--delta-dir",delta_root,
      "--output-root",next_state
    ))
    if(status!=0L) stop(sprintf("Failed replaying Workflow 01 delta run %s",p$github_run_id),call.=FALSE)
    unlink(state_dir,recursive=TRUE,force=TRUE)
    state_dir <- next_state
  }
}

if(!dir.create(dirname(output_root),recursive=TRUE,showWarnings=FALSE) && !dir.exists(dirname(output_root))){
  stop("Could not create output parent directory",call.=FALSE)
}
if(!file.rename(state_dir,output_root)){
  stop("Failed to move reconstructed Workflow 01 state to requested output root",call.=FALSE)
}

latest <- chain[[length(chain)]]
audit <- list(
  schema="living-evidence-map-workflow01-current-state-restore-v1",
  status="PASS",
  baseline_github_run_id=as.character(chain[[1L]]$github_run_id),
  baseline_zenodo_record_id=as.character(chain[[1L]]$zenodo_record_id),
  latest_github_run_id=as.character(latest$github_run_id),
  latest_zenodo_record_id=as.character(latest$zenodo_record_id),
  deltas_replayed=max(0L,length(chain)-1L),
  lineage=lapply(chain,function(z)list(
    github_run_id=as.character(z$github_run_id),
    zenodo_record_id=as.character(z$zenodo_record_id),
    state=as.character(z$state),
    manifest_sha256=as.character(z$manifest_sha256)
  )),
  restored_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_root,"workflow01_current_state_restore_audit.json"),useBytes=TRUE)
cat(sprintf("PASS: restored Workflow 01 current state from baseline plus %d delta(s); latest run %s\n",
            audit$deltas_replayed,audit$latest_github_run_id))

`%||%` <- function(x,y) if(is.null(x)) y else x
