#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
state_dir <- normalizePath(arg("--state-dir"),mustWork=TRUE)
run_id <- arg("--run-id")
source_run_id <- arg("--source-run-id")
repository <- arg("--repository")
output_dir <- arg("--output-dir")
input_sha <- arg("--input-canonical-sha256")
lean_sha <- arg("--lean-canonical-sha256")
if(any(vapply(list(run_id,source_run_id,repository,output_dir,input_sha,lean_sha),is.null,logical(1)))) stop("Missing required argument",call.=FALSE)

lean_path <- file.path(state_dir,"canonical_lean.jsonl")
report_path <- file.path(state_dir,"compaction_report.json")
if(!all(file.exists(c(lean_path,report_path)))) stop("Lean checkpoint state is incomplete",call.=FALSE)
actual_lean_sha <- digest(file=lean_path,algo="sha256",serialize=FALSE)
if(!identical(tolower(actual_lean_sha),tolower(lean_sha))) stop(sprintf("Lean SHA mismatch: %s",actual_lean_sha),call.=FALSE)
rep <- fromJSON(report_path,simplifyVector=FALSE)
stopifnot(identical(rep$status,"PASS"),as.integer(rep$canonical_records)>=1L,
          isTRUE(rep$record_ids_unchanged),isTRUE(rep$non_manifestation_fields_unchanged),
          isTRUE(rep$manifestation_refs_exact),
          identical(tolower(as.character(rep$input_canonical_sha256)),tolower(input_sha)),
          identical(tolower(as.character(rep$output_lean_canonical_sha256)),tolower(lean_sha)))

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
output_dir <- normalizePath(output_dir,mustWork=TRUE)
archive_dir <- file.path(output_dir,"archive_files")
dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)

checkpoint_dir <- file.path(output_dir,"checkpoint")
dir.create(checkpoint_dir,recursive=TRUE,showWarnings=FALSE)
file.copy(lean_path,file.path(checkpoint_dir,"canonical_lean.jsonl"),overwrite=TRUE)
file.copy(report_path,file.path(checkpoint_dir,"compaction_report.json"),overwrite=TRUE)
all_paths <- list.files(checkpoint_dir,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if(length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))
archive_name <- sprintf("LivingEvidenceMap_post_w02_lean_checkpoint_run-%s.tar.gz",source_run_id)
archive_path <- file.path(archive_dir,archive_name)
old <- setwd(dirname(checkpoint_dir));on.exit(setwd(old),add=TRUE)
utils::tar(archive_path,files=basename(checkpoint_dir),compression="gzip",tar="internal")
setwd(old);on.exit(NULL,add=FALSE)

source_run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,source_run_id)
publish_run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
manifest <- list(
  schema="living-evidence-map-post-w02-lean-checkpoint-archive-v1",
  workflow="02",
  state="post_w02_lean_canonical_checkpoint",
  source_github_run_id=as.character(source_run_id),
  source_github_run_url=source_run_url,
  publication_github_run_id=as.character(run_id),
  publication_github_run_url=publish_run_url,
  repository=repository,
  input_canonical_sha256=input_sha,
  lean_canonical_sha256=lean_sha,
  canonical_records=as.integer(rep$canonical_records),
  manifestations=as.integer(rep$manifestations),
  manifestation_refs=as.integer(rep$manifestation_refs),
  record_ids_unchanged=TRUE,
  non_manifestation_fields_unchanged=TRUE,
  manifestation_refs_exact=TRUE,
  input_bytes=as.numeric(rep$input_bytes),
  output_bytes=as.numeric(rep$output_bytes),
  bytes_removed=as.numeric(rep$bytes_removed),
  size_reduction_fraction=as.numeric(rep$size_reduction_fraction),
  file_visibility="restricted",
  files=list(state_archive=list(
    filename=archive_name,
    bytes=unname(file.info(archive_path)$size),
    sha256=digest(file=archive_path,algo="sha256",serialize=FALSE)
  ))
)
manifest_name <- sprintf("LivingEvidenceMap_post_w02_lean_checkpoint_run-%s_manifest.json",source_run_id)
manifest_path <- file.path(archive_dir,manifest_name)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),manifest_path,useBytes=TRUE)

api <- "https://zenodo.org/api/deposit/depositions"
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
perform <- function(req,expected,label,timeout=600){
  resp <- req |> req_timeout(timeout) |> req_error(is_error=function(resp)FALSE) |> req_perform()
  st <- resp_status(resp)
  if(!(st %in% expected)){
    body <- tryCatch(resp_body_string(resp),error=function(e)"")
    stop(sprintf("Zenodo %s HTTP %d: %s",label,st,body),call.=FALSE)
  }
  resp
}
metadata <- list(metadata=list(
  title=sprintf("Living Evidence Map post-Workflow 02 lean canonical checkpoint | source run %s",source_run_id),
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=paste0(
    "<p>Authoritative lean canonical checkpoint produced after Living Evidence Map Workflow 02.</p>",
    "<p>The checkpoint preserves all ",as.integer(rep$canonical_records)," canonical works and all canonical metadata while replacing embedded manifestation objects with exact source:source_record_id references.</p>",
    "<p>Input post-Workflow 02 canonical SHA-256: <code>",input_sha,"</code>.</p>",
    "<p>Lean canonical SHA-256: <code>",lean_sha,"</code>.</p>"
  ),
  creators=list(list(name="Haddaway, Neal")),
  access_right="restricted",
  access_conditions="Files contain bibliographic metadata whose redistribution may be restricted by source terms.",
  keywords=list("Living Evidence Map","Workflow 02","lean canonical checkpoint","provenance",paste0("LivingEvidenceMap-post-w02-run-",source_run_id))
))
created <- perform(request(api)|>req_method("POST")|>auth()|>req_headers("Content-Type"="application/json")|>req_body_raw(charToRaw("{}"),type="application/json"),201L,"draft creation",60)|>resp_body_json(simplifyVector=FALSE)
dep_id <- as.character(created$id)
bucket <- as.character(created$links$bucket)
perform(request(paste0(api,"/",dep_id))|>req_method("PUT")|>auth()|>req_headers("Content-Type"="application/json")|>req_body_json(metadata,auto_unbox=TRUE),200L,"metadata update",60)

paths <- c(archive_path,manifest_path)
uploaded <- vector("list",length(paths))
for(i in seq_along(paths)){
  p <- paths[[i]]; fn <- basename(p); ok <- NULL
  for(attempt in seq_len(5L)){
    resp <- request(paste0(bucket,"/",URLencode(fn,reserved=TRUE)))|>req_method("PUT")|>auth()|>req_headers(Expect="")|>req_body_file(p)|>req_timeout(1800)|>req_error(is_error=function(resp)FALSE)|>req_perform()
    st <- resp_status(resp)
    if(st %in% c(200L,201L)){ok <- resp; break}
    if(!(st %in% c(429L,500L,502L,503L,504L)) || attempt==5L) stop(sprintf("Upload failed for %s HTTP %d",fn,st),call.=FALSE)
    Sys.sleep(min(60,5*2^(attempt-1L)))
  }
  uploaded[[i]] <- resp_body_json(ok,simplifyVector=FALSE)
}
published <- perform(request(paste0(api,"/",dep_id,"/actions/publish"))|>req_method("POST")|>auth(),c(200L,201L,202L),"publish",120)|>resp_body_json(simplifyVector=FALSE)
record_id <- as.character(if(is.null(published$record_id)) published$id else published$record_id)
receipt <- list(
  status="published",
  workflow="02",
  state="post_w02_lean_canonical_checkpoint",
  source_github_run_id=as.character(source_run_id),
  source_github_run_url=source_run_url,
  publication_github_run_id=as.character(run_id),
  publication_github_run_url=publish_run_url,
  input_canonical_sha256=input_sha,
  lean_canonical_sha256=lean_sha,
  canonical_records=as.integer(rep$canonical_records),
  manifestations=as.integer(rep$manifestations),
  manifestation_refs=as.integer(rep$manifestation_refs),
  input_bytes=as.numeric(rep$input_bytes),
  output_bytes=as.numeric(rep$output_bytes),
  bytes_removed=as.numeric(rep$bytes_removed),
  size_reduction_fraction=as.numeric(rep$size_reduction_fraction),
  zenodo_record_id=record_id,
  zenodo_deposition_id=dep_id,
  doi=if(is.null(published$doi)) NA_character_ else published$doi,
  record_url=if(!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",record_id),
  visibility="restricted",
  archive_files=lapply(seq_along(paths),function(i){
    p<-paths[[i]];z<-uploaded[[i]]
    list(filename=basename(p),bytes=unname(file.info(p)$size),sha256=digest(file=p,algo="sha256",serialize=FALSE),
         zenodo_checksum=if(is.null(z$checksum))NULL else z$checksum)
  }),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),file.path(output_dir,"zenodo_receipt.json"),useBytes=TRUE)
cat(sprintf("PASS: published post-W02 lean checkpoint as restricted Zenodo record %s; lean SHA256=%s\n",record_id,lean_sha))
