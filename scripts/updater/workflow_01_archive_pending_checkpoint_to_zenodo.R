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

checkpoint_dir <- normalizePath(arg("--checkpoint-dir"),mustWork=TRUE)
run_id <- arg("--run-id")
repository <- arg("--repository")
output_dir <- arg("--output-dir")
if(any(vapply(list(run_id,repository,output_dir),is.null,logical(1)))) stop("Missing required argument",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
output_dir <- normalizePath(output_dir,mustWork=TRUE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)
cm_path <- file.path(checkpoint_dir,"checkpoint_manifest.json")
if(!file.exists(cm_path)) stop("checkpoint_manifest.json missing",call.=FALSE)
cm <- fromJSON(cm_path,simplifyVector=FALSE)
if(!identical(cm$schema,"living-evidence-map-workflow01-pending-checkpoint-v1")) stop("Unsupported checkpoint schema",call.=FALSE)
if(xor(is.null(cm$review_id),is.null(cm$google_sheet_id)))
  stop("Checkpoint has incomplete review-specific identity",call.=FALSE)

archive_dir <- file.path(output_dir,"archive_files")
dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)
all_paths <- list.files(checkpoint_dir,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if(length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))

archive_name <- sprintf("LivingEvidenceMap_workflow01_run-%s_pre-adjudication-checkpoint.tar.gz",run_id)
archive_path <- file.path(archive_dir,archive_name)
old <- setwd(dirname(checkpoint_dir)); on.exit(setwd(old),add=TRUE)
utils::tar(archive_path,files=basename(checkpoint_dir),compression="gzip",tar="internal")
setwd(old); on.exit(NULL,add=FALSE)
if(!file.exists(archive_path)||file.info(archive_path)$size<=0) stop("Failed to create checkpoint archive",call.=FALSE)

run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
manifest <- list(
  schema="living-evidence-map-workflow01-pending-checkpoint-archive-v1",
  workflow="01",
  state="pre_adjudication",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  repository=repository,
  review_id=cm$review_id,
  google_sheet_id=cm$google_sheet_id,
  previous_workflow01=cm$previous_workflow01,
  workflow00=cm$workflow00,
  new_source_manifestations=cm$new_source_manifestations,
  pending_human_cases=cm$pending_human_cases,
  queue_sha256=cm$queue_sha256,
  file_visibility="restricted",
  files=list(checkpoint_archive=list(
    filename=archive_name,
    bytes=unname(file.info(archive_path)$size),
    sha256=digest(file=archive_path,algo="sha256",serialize=FALSE)
  ))
)
manifest_name <- sprintf("LivingEvidenceMap_workflow01_run-%s_pre-adjudication-manifest.json",run_id)
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

title <- sprintf("Living Evidence Map Workflow 01 human-review checkpoint | run %s",run_id)
metadata <- list(metadata=list(
  title=title,
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=paste0(
    "<p>Compact pre-adjudication checkpoint for Living Evidence Map Workflow 01.</p>",
    if(isTRUE(cm$first_run)) {
      "<p>First-run checkpoint containing the initial source manifestations, pair decisions, LLM adjudications and locked human-review queue.</p>"
    } else {
      paste0("<p>Incremental checkpoint for Workflow 01 Zenodo record ",
             as.character(cm$previous_workflow01$zenodo_record_id),
             " containing new manifestations, decisions and the locked human-review queue.</p>")
    },
    "<p>Pending human-review cases: ",cm$pending_human_cases,".</p>"
  ),
  creators=list(list(name="Haddaway, Neal")),
  access_right="restricted",
  access_conditions="Files contain bibliographic metadata and derived deduplication state from database/API sources whose terms may limit redistribution.",
  keywords=list("Living Evidence Map","Workflow 01","deduplication","human review","checkpoint",
                paste0("LivingEvidenceMap-workflow01-run-",run_id))
))

created <- perform(
  request(api) |> req_method("POST") |> auth() |>
    req_headers("Content-Type"="application/json") |>
    req_body_raw(charToRaw("{}"),type="application/json"),
  201L,"draft creation",60
) |> resp_body_json(simplifyVector=FALSE)
dep_id <- as.character(created$id); bucket <- as.character(created$links$bucket)
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
  p <- archive_paths[[i]]; fn <- basename(p)
  resp <- request(paste0(bucket,"/",URLencode(fn,reserved=TRUE))) |>
    req_method("PUT") |> auth() |> req_headers(Expect="") |> req_body_file(p) |>
    req_timeout(1800) |> req_error(is_error=function(resp)FALSE) |> req_perform()
  if(!(resp_status(resp) %in% c(200L,201L))) stop(sprintf("Checkpoint upload failed for %s",fn),call.=FALSE)
  uploaded[[i]] <- resp_body_json(resp,simplifyVector=FALSE)
}

published <- perform(
  request(paste0(api,"/",dep_id,"/actions/publish")) |> req_method("POST") |> auth(),
  c(200L,201L,202L),"publish",120
) |> resp_body_json(simplifyVector=FALSE)
record_id <- as.character(if(is.null(published$record_id)) published$id else published$record_id)

receipt <- list(
  status="published",
  workflow="01",
  state="pre_adjudication",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  review_id=cm$review_id,
  google_sheet_id=cm$google_sheet_id,
  previous_github_run_id=if(isTRUE(cm$first_run)) NULL else as.character(cm$previous_workflow01$github_run_id),
  previous_zenodo_record_id=if(isTRUE(cm$first_run)) NULL else as.character(cm$previous_workflow01$zenodo_record_id),
  previous_manifest_sha256=if(isTRUE(cm$first_run)) NULL else as.character(cm$previous_workflow01$manifest_sha256),
  workflow00=cm$workflow00,
  new_source_manifestations=as.integer(cm$new_source_manifestations),
  pending_human_cases=as.integer(cm$pending_human_cases),
  queue_sha256=as.character(cm$queue_sha256),
  zenodo_record_id=record_id,
  zenodo_deposition_id=dep_id,
  doi=if(is.null(published$doi)) NA_character_ else published$doi,
  record_url=if(!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",record_id),
  visibility="restricted",
  archive_files=lapply(seq_along(archive_paths),function(i){
    p<-archive_paths[[i]];z<-uploaded[[i]]
    list(filename=basename(p),bytes=unname(file.info(p)$size),
         sha256=digest(file=p,algo="sha256",serialize=FALSE),
         zenodo_checksum=if(is.null(z$checksum))NULL else z$checksum)
  }),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"zenodo_receipt.json"),useBytes=TRUE)
cat(sprintf("PASS: published compact Workflow 01 human-review checkpoint as Zenodo record %s\n",record_id))
