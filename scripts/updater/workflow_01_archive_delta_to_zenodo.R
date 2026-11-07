#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
})


`%||%` <- function(x,y) if(is.null(x)) y else x

zenodo_config <- jsonlite::fromJSON("user_input/zenodo_config.json",simplifyVector=FALSE)
zenodo_creators <- zenodo_config$creators
zenodo_access_right <- as.character(zenodo_config$access_right)
if(is.null(zenodo_creators)||!length(zenodo_creators)||any(!vapply(zenodo_creators,function(z)!is.null(z$name)&&length(z$name)==1L&&nzchar(trimws(as.character(z$name))),logical(1))))stop("Configure at least one Zenodo creator",call.=FALSE)
if(length(zenodo_access_right)!=1L||!identical(zenodo_access_right,"restricted"))stop("W00-W08 archives require restricted Zenodo deposits",call.=FALSE)
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

delta_dir <- normalizePath(arg("--delta-dir"),mustWork=TRUE)
run_id <- arg("--run-id")
repository <- arg("--repository")
review_id <- arg("--review-id","")
review_sheet_id <- arg("--review-sheet-id","")
if(xor(nzchar(review_id),nzchar(review_sheet_id)))
  stop("Delta archive requires both review ID and Google Sheet ID",call.=FALSE)
if(!nzchar(review_id)) review_id <- NULL
if(!nzchar(review_sheet_id)) review_sheet_id <- NULL
output_dir <- arg("--output-dir")
if(any(vapply(list(run_id,repository,output_dir),is.null,logical(1)))) {
  stop("Required: --delta-dir --run-id --repository --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
output_dir <- normalizePath(output_dir,mustWork=TRUE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)

dm_path <- file.path(delta_dir,"delta_manifest.json")
if(!file.exists(dm_path)) stop("delta_manifest.json missing",call.=FALSE)
dm <- fromJSON(dm_path,simplifyVector=FALSE)
if(!identical(dm$schema,"living-evidence-map-workflow01-delta-v1")) stop("Unsupported Workflow 01 delta schema",call.=FALSE)

summary_path <- file.path(delta_dir,"target_summary.json")
if(!file.exists(summary_path)) stop("target_summary.json missing",call.=FALSE)
summary <- fromJSON(summary_path,simplifyVector=FALSE)

archive_dir <- file.path(output_dir,"archive_files")
dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)
all_paths <- list.files(delta_dir,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if(length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))

archive_name <- sprintf("LivingEvidenceMap_workflow01_run-%s_delta.tar.gz",run_id)
archive_path <- file.path(archive_dir,archive_name)
old <- setwd(dirname(delta_dir)); on.exit(setwd(old),add=TRUE)
utils::tar(archive_path,files=basename(delta_dir),compression="gzip",tar="internal")
setwd(old); on.exit(NULL,add=FALSE)
if(!file.exists(archive_path)||file.info(archive_path)$size<=0) stop("Failed to create delta archive",call.=FALSE)

run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
archive_sha <- digest(file=archive_path,algo="sha256",serialize=FALSE)
manifest <- list(
  schema="living-evidence-map-workflow01-delta-archive-v1",
  workflow="01",
  state="delta",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  repository=repository,
  review_id=review_id,
  google_sheet_id=review_sheet_id,
  previous=dm$previous,
  target=dm$target,
  delta=dm$delta,
  file_visibility="restricted",
  files=list(delta_archive=list(
    filename=archive_name,
    bytes=unname(file.info(archive_path)$size),
    sha256=archive_sha
  ))
)
manifest_name <- sprintf("LivingEvidenceMap_workflow01_run-%s_delta-manifest.json",run_id)
manifest_path <- file.path(archive_dir,manifest_name)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),manifest_path,useBytes=TRUE)

api <- "https://zenodo.org/api/deposit/depositions"
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
perform <- function(req,expected,label,timeout=600){
  resp <- req |> req_timeout(timeout) |> req_error(is_error=function(resp)FALSE) |> req_perform()
  status <- resp_status(resp)
  if(!(status %in% expected)){
    body <- tryCatch(resp_body_string(resp),error=function(e)"")
    stop(sprintf("Zenodo %s returned HTTP %d: %s",label,status,body),call.=FALSE)
  }
  resp
}

title <- sprintf("Living Evidence Map Workflow 01 delta | run %s",run_id)
description <- paste0(
  "<p>Incremental durable-state delta for the Living Evidence Map Workflow 01 deduplication pipeline.</p>",
  "<p>This record does not duplicate the full corpus. It extends Workflow 01 state from Zenodo record ",
  dm$previous$zenodo_record_id," to GitHub Actions run <a href=\"",run_url,"\">",run_id,"</a>.</p>",
  "<p>New source manifestations: ",dm$delta$new_source_manifestations,
  ". Pair-decision upserts: ",dm$delta$pair_decision_upserts,
  ". Cluster-map upserts: ",dm$delta$cluster_map_upserts,
  ". Canonical-record upserts: ",dm$delta$canonical_upserts,
  ". Canonical retirements: ",dm$delta$canonical_retired_ids,".</p>"
)
metadata <- list(metadata=list(
  title=title,
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=description,
  creators=zenodo_creators,
  access_right=zenodo_access_right,
  access_conditions=paste(
    "Files contain bibliographic metadata and derived deduplication state from database/API sources",
    "whose terms may limit redistribution. Access may be granted by the depositor where permitted."
  ),
  keywords=list("Living Evidence Map","evidence synthesis","deduplication","Workflow 01","incremental delta",
                paste0("LivingEvidenceMap-workflow01-run-",run_id)),
  notes=paste0(
    "Previous Workflow 01 Zenodo record: ",dm$previous$zenodo_record_id,
    ". Target manifestations: ",dm$target$source_manifestations,
    ". Target canonical records: ",dm$target$canonical_records,"."
  )
))

created <- perform(
  request(api) |> req_method("POST") |> auth() |>
    req_headers("Content-Type"="application/json") |>
    req_body_raw(charToRaw("{}"),type="application/json"),
  201L,"draft creation",60
) |> resp_body_json(simplifyVector=FALSE)
dep_id <- as.character(created$id)
bucket <- as.character(created$links$bucket)
if(!nzchar(dep_id)||!nzchar(bucket)) stop("Zenodo draft response missing id/bucket",call.=FALSE)

perform(
  request(paste0(api,"/",dep_id)) |> req_method("PUT") |> auth() |>
    req_headers("Content-Type"="application/json") |>
    req_body_json(metadata,auto_unbox=TRUE),
  200L,"metadata update",60
)

archive_paths <- c(archive_path,manifest_path)
uploaded <- vector("list",length(archive_paths))
for(i in seq_along(archive_paths)){
  p <- archive_paths[[i]]
  fn <- basename(p)
  upload_url <- paste0(bucket,"/",URLencode(fn,reserved=TRUE))
  cat(sprintf("ZENODO DELTA UPLOAD %s bytes=%s\n",fn,file.info(p)$size))
  resp_ok <- NULL
  for(attempt in seq_len(5L)){
    resp <- request(upload_url) |> req_method("PUT") |> auth() |>
      req_headers(Expect="") |> req_body_file(p) |> req_timeout(1800) |>
      req_error(is_error=function(resp)FALSE) |> req_perform()
    status <- resp_status(resp)
    if(status %in% c(200L,201L)){ resp_ok <- resp; break }
    body <- tryCatch(resp_body_string(resp),error=function(e)"")
    transient <- status %in% c(429L,500L,502L,503L,504L)
    if(!transient || attempt==5L) stop(sprintf("Zenodo delta upload failed for %s, HTTP %d: %s",fn,status,body),call.=FALSE)
    Sys.sleep(min(60,2^(attempt-1L)*5))
  }
  uploaded[[i]] <- resp_body_json(resp_ok,simplifyVector=FALSE)
}

published <- perform(
  request(paste0(api,"/",dep_id,"/actions/publish")) |> req_method("POST") |> auth(),
  c(200L,201L,202L),"publish",120
) |> resp_body_json(simplifyVector=FALSE)

record_id <- as.character(if(is.null(published$record_id)) published$id else published$record_id)
receipt <- list(
  status="published",
  workflow="01",
  state="delta",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  review_id=review_id,
  google_sheet_id=review_sheet_id,
  source_manifestations=as.integer(dm$target$source_manifestations),
  preserved_pair_decisions=as.integer(summary$preserved_pair_decisions %||% NA),
  incremental_pair_decisions=as.integer(summary$incremental_pair_decisions %||% NA),
  manual_review_pairs=as.integer(summary$manual_review_pairs %||% NA),
  canonical_records=as.integer(dm$target$canonical_records),
  canonical_jsonl_sha256=as.character(dm$target$canonical_jsonl_sha256),
  canonical_jsonl_bytes=as.numeric(dm$target$canonical_jsonl_bytes),
  previous_github_run_id=as.character(dm$previous$github_run_id),
  previous_zenodo_record_id=as.character(dm$previous$zenodo_record_id),
  previous_manifest_sha256=as.character(dm$previous$manifest_sha256),
  delta_new_source_manifestations=as.integer(dm$delta$new_source_manifestations),
  delta_pair_decision_upserts=as.integer(dm$delta$pair_decision_upserts),
  delta_cluster_map_upserts=as.integer(dm$delta$cluster_map_upserts),
  delta_canonical_upserts=as.integer(dm$delta$canonical_upserts),
  delta_canonical_retired_ids=as.integer(dm$delta$canonical_retired_ids),
  zenodo_record_id=record_id,
  zenodo_deposition_id=dep_id,
  doi=if(is.null(published$doi)) NA_character_ else published$doi,
  record_url=if(!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",record_id),
  title=title,
  visibility="restricted",
  archive_files=lapply(seq_along(archive_paths),function(i){
    p <- archive_paths[[i]]; z <- uploaded[[i]]
    list(filename=basename(p),bytes=unname(file.info(p)$size),
         sha256=digest(file=p,algo="sha256",serialize=FALSE),
         zenodo_checksum=if(is.null(z$checksum)) NULL else z$checksum)
  }),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"zenodo_receipt.json"),useBytes=TRUE)
cat(sprintf("PASS: published restricted Workflow 01 delta as Zenodo record %s; archive bytes=%s\n",
            record_id,file.info(archive_path)$size))
