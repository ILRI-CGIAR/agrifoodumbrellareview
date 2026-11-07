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

staging_root <- normalizePath(arg("--staging-root"),mustWork=TRUE)
run_id <- arg("--run-id")
repository <- arg("--repository")
state <- arg("--state","pre_adjudication")
review_id <- arg("--review-id","")
review_sheet_id <- arg("--review-sheet-id","")
execution_pathway <- arg("--execution-pathway",if(nzchar(review_id)) "shiny" else "github")
if(!execution_pathway %in% c("shiny","github")) stop("Invalid execution pathway",call.=FALSE)
if(xor(nzchar(review_id),nzchar(review_sheet_id)))
  stop("State archive requires both review ID and Sheet ID",call.=FALSE)
if(!nzchar(review_id)) review_id <- NULL
if(!nzchar(review_sheet_id)) review_sheet_id <- NULL
archive_only <- identical(tolower(arg("--archive-only","false")),"true")
output_dir <- arg("--output-dir")
if (any(vapply(list(run_id,repository,state,output_dir),is.null,logical(1)))) {
  stop("Required: --staging-root --run-id --repository --state --output-dir",call.=FALSE)
}
if (!(state %in% c("pre_adjudication","final"))) stop("state must be pre_adjudication or final",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
output_dir <- normalizePath(output_dir,mustWork=TRUE)

token <- Sys.getenv("ZENODO_ACCESS_TOKEN")
if (!archive_only && !nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)

summary_path <- file.path(staging_root,"workflow01_full_five_source","summary.json")
if (!file.exists(summary_path)) stop("Workflow 01 summary.json missing from staged state",call.=FALSE)
summary <- fromJSON(summary_path,simplifyVector=FALSE)

required_summary <- c(
  "source_manifestations","preserved_pair_decisions","incremental_candidate_rows",
  "incremental_pair_decisions","exact_duplicate_candidate_rows_removed",
  "manual_review_pairs"
)
missing <- required_summary[!vapply(required_summary,function(nm)!is.null(summary[[nm]]),logical(1))]
if (length(missing)) stop(sprintf("Workflow 01 summary missing: %s",paste(missing,collapse=", ")),call.=FALSE)

archive_dir <- file.path(output_dir,"archive_files")
dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)
all_paths <- list.files(staging_root,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if (length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))

state_filename <- sprintf("LivingEvidenceMap_workflow01_run-%s_%s-state.tar.gz",run_id,state)
state_path <- file.path(archive_dir,state_filename)
old <- setwd(staging_root); on.exit(setwd(old),add=TRUE)
members <- c("workflow01_seed","workflow01_incremental_rescore","workflow01_full_five_source","adjudication","canonical","reports","provenance")
members <- members[file.exists(members)]
if (!length(members)) stop("No Workflow 01 state members found",call.=FALSE)
utils::tar(state_path,files=members,compression="gzip",tar="internal")
if (!file.exists(state_path) || file.info(state_path)$size <= 0) stop("Failed to create Workflow 01 state archive",call.=FALSE)
setwd(old)
on.exit(NULL,add=FALSE)

run_url <- sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
state_sha <- digest(file=state_path,algo="sha256",serialize=FALSE)
canonical_manifest_path <- file.path(staging_root,"canonical","canonical_manifest.json")
canonical_manifest <- if (file.exists(canonical_manifest_path)) fromJSON(canonical_manifest_path,simplifyVector=FALSE) else NULL

manifest <- list(
  schema="living-evidence-map-workflow01-deduplication-state-v2",
  workflow="01",
  state=state,
  github_run_id=run_id,
  github_run_url=run_url,
  repository=repository,
  review_id=review_id,
  google_sheet_id=review_sheet_id,
  execution_pathway=execution_pathway,
  source_manifestations=as.integer(summary$source_manifestations),
  preserved_pair_decisions=as.integer(summary$preserved_pair_decisions),
  incremental_candidate_rows=as.integer(summary$incremental_candidate_rows),
  incremental_pair_decisions=as.integer(summary$incremental_pair_decisions),
  exact_duplicate_candidate_rows_removed=as.integer(summary$exact_duplicate_candidate_rows_removed),
  manual_review_pairs=as.integer(summary$manual_review_pairs),
  canonical_records=if(is.null(canonical_manifest)) NULL else as.integer(canonical_manifest$records),
  canonical_jsonl_sha256=if(is.null(canonical_manifest)) NULL else as.character(canonical_manifest$canonical_jsonl_sha256),
  canonical_jsonl_bytes=if(is.null(canonical_manifest)) NULL else as.numeric(canonical_manifest$canonical_jsonl_bytes),
  file_visibility="restricted",
  files=list(
    state_archive=list(
      filename=state_filename,
      bytes=unname(file.info(state_path)$size),
      sha256=state_sha
    )
  )
)
manifest_filename <- sprintf("LivingEvidenceMap_workflow01_run-%s_manifest.json",run_id)
manifest_path <- file.path(archive_dir,manifest_filename)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),manifest_path)

archive_paths <- c(state_path,manifest_path)
total_bytes <- sum(file.info(archive_paths)$size)
if (!is.finite(total_bytes) || total_bytes <= 0) stop("Archive payload is empty",call.=FALSE)
if (total_bytes > 49 * 1024^3) stop("Archive payload exceeds 49 GiB",call.=FALSE)

if (archive_only) {
  cat(sprintf("PASS: local Workflow 01 %s archive-only validation: %s; manifest: %s\\n",
              state,state_path,manifest_path))
  quit(save="no",status=0L)
}

api <- "https://zenodo.org/api/deposit/depositions"
auth <- function(req) req |> req_headers(Authorization=paste("Bearer",token))
perform <- function(req,expected,label,timeout=600) {
  resp <- req |> req_timeout(timeout) |> req_error(is_error=function(resp) FALSE) |> req_perform()
  status <- resp_status(resp)
  if (!(status %in% expected)) {
    body <- tryCatch(resp_body_string(resp),error=function(e) "")
    stop(sprintf("Zenodo %s returned HTTP %d: %s",label,status,body),call.=FALSE)
  }
  resp
}

title <- sprintf("Living Evidence Map deduplication state | Workflow 01 run %s | %s",run_id,state)
description <- paste0(
  "<p>Durable state archive for the Living Evidence Map Workflow 01 deduplication pipeline.</p>",
  "<p>GitHub Actions run <a href=\"",run_url,"\">",run_id,"</a>. ",
  "State: ",state,". Manifestations: ",summary$source_manifestations,
  ". Unique incremental pair decisions: ",summary$incremental_pair_decisions,
  ". Residual manual-review pairs: ",summary$manual_review_pairs,".</p>",
  "<p>The archive preserves the pair-decision state, cluster reconstruction outputs, ",
  "adjudication audit and, for final state archives, the source-agnostic canonical JSONL used by downstream workflows. ",
  "Files are restricted because the state includes bibliographic metadata derived from licensed/API sources.</p>"
)
metadata <- list(metadata=list(
  title=title,
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=description,
  creators=list(list(name="Haddaway, Neal")),
  access_right="restricted",
  access_conditions=paste(
    "Files contain bibliographic metadata and derived deduplication state from database/API sources",
    "whose terms may limit redistribution. Access may be granted by the depositor where permitted."
  ),
  keywords=list(
    "Living Evidence Map","evidence synthesis","deduplication","Workflow 01",
    paste0("LivingEvidenceMap-workflow01-run-",run_id),state
  ),
  notes=paste0(
    "Source GitHub Actions run: ",run_id,
    ". State: ",state,
    ". Manifestations: ",summary$source_manifestations,
    ". Manual-review pairs: ",summary$manual_review_pairs,"."
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
if (!nzchar(dep_id) || !nzchar(bucket)) stop("Zenodo draft response missing id/bucket",call.=FALSE)

writeLines(toJSON(list(status="draft_created",github_run_id=run_id,zenodo_deposition_id=dep_id),
                  auto_unbox=TRUE,pretty=TRUE),
           file.path(output_dir,"zenodo_draft_receipt.json"))

perform(
  request(paste0(api,"/",dep_id)) |> req_method("PUT") |> auth() |>
    req_headers("Content-Type"="application/json") |>
    req_body_json(metadata,auto_unbox=TRUE),
  200L,"metadata update",60
)

uploaded <- vector("list",length(archive_paths))
for (i in seq_along(archive_paths)) {
  p <- archive_paths[[i]]
  fn <- basename(p)
  upload_url <- paste0(bucket,"/",URLencode(fn,reserved=TRUE))
  cat(sprintf("ZENODO UPLOAD %s bytes=%s\n",fn,file.info(p)$size))
  upload_resp <- NULL
  for (attempt in seq_len(5L)) {
    resp <- request(upload_url) |> req_method("PUT") |> auth() |>
      req_headers(Expect="") |> req_body_file(p) |> req_timeout(1800) |>
      req_error(is_error=function(resp) FALSE) |> req_perform()
    status <- resp_status(resp)
    if (status %in% c(200L,201L)) {
      upload_resp <- resp
      break
    }
    body <- tryCatch(resp_body_string(resp),error=function(e) "")
    transient <- status %in% c(429L,500L,502L,503L,504L)
    if (!transient || attempt == 5L) {
      stop(sprintf("Zenodo upload failed for %s after %d attempt(s), HTTP %d: %s",
                   fn,attempt,status,body),call.=FALSE)
    }
    delay <- min(60,2^(attempt-1L)*5)
    message(sprintf("Transient Zenodo HTTP %d uploading %s; retry %d/5 after %ds",
                    status,fn,attempt+1L,delay))
    Sys.sleep(delay)
  }
  uploaded[[i]] <- resp_body_json(upload_resp,simplifyVector=FALSE)
}

published <- perform(
  request(paste0(api,"/",dep_id,"/actions/publish")) |> req_method("POST") |> auth(),
  c(200L,201L,202L),"publish",120
) |> resp_body_json(simplifyVector=FALSE)

receipt <- list(
  status="published",
  workflow="01",
  state=state,
  github_run_id=run_id,
  github_run_url=run_url,
  review_id=review_id,
  google_sheet_id=review_sheet_id,
  source_manifestations=as.integer(summary$source_manifestations),
  preserved_pair_decisions=as.integer(summary$preserved_pair_decisions),
  incremental_candidate_rows=as.integer(summary$incremental_candidate_rows),
  incremental_pair_decisions=as.integer(summary$incremental_pair_decisions),
  exact_duplicate_candidate_rows_removed=as.integer(summary$exact_duplicate_candidate_rows_removed),
  manual_review_pairs=as.integer(summary$manual_review_pairs),
  canonical_records=if(is.null(canonical_manifest)) NULL else as.integer(canonical_manifest$records),
  canonical_jsonl_sha256=if(is.null(canonical_manifest)) NULL else as.character(canonical_manifest$canonical_jsonl_sha256),
  canonical_jsonl_bytes=if(is.null(canonical_manifest)) NULL else as.numeric(canonical_manifest$canonical_jsonl_bytes),
  zenodo_record_id=as.character(if (is.null(published$record_id)) published$id else published$record_id),
  zenodo_deposition_id=dep_id,
  doi=if (is.null(published$doi)) NA_character_ else published$doi,
  record_url=if (!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",dep_id),
  title=title,
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
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"zenodo_receipt.json"))
cat(sprintf("PASS: published restricted Workflow 01 %s state as Zenodo record %s\n",
            state,receipt$zenodo_record_id))
