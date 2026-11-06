#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_dir <- arg("--input-dir","inputs/lens_ingestion")
output_dir <- arg("--output-dir","outputs/updater/lens_sidecar_adapter")
if (!dir.exists(input_dir)) stop(sprintf("Input directory does not exist: %s",input_dir),call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
present <- function(x) {
  if (is.null(x) || !length(x)) return(FALSE)
  y <- unlist(x,use.names=FALSE)
  any(!is.na(y) & nzchar(trimws(as.character(y))))
}

locate_records <- function(root) {
  candidates <- c(
    file.path(root,"records.jsonl"),
    file.path(root,"source_child","records.jsonl")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates)==1L) return(candidates[[1L]])
  recursive <- list.files(root,pattern="^records\\.jsonl$",recursive=TRUE,full.names=TRUE)
  recursive <- unique(normalizePath(recursive,mustWork=TRUE))
  if (length(recursive)==1L) return(recursive[[1L]])
  stop(sprintf("Could not uniquely locate authoritative Lens records.jsonl under %s",root),call.=FALSE)
}

records_path <- locate_records(input_dir)
lines <- readLines(records_path,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
if (!length(lines)) stop("Lens sidecar adapter received zero records",call.=FALSE)

adapted_at <- now_utc()
coverage <- vector("list",length(lines))
ids <- character(length(lines))
jsonl_path <- file.path(output_dir,"lens_sidecar_records.jsonl")
con <- file(jsonl_path,"wt",encoding="UTF-8")

for (i in seq_along(lines)) {
  r <- fromJSON(lines[[i]],simplifyVector=FALSE)
  id <- r$identity %||% list()
  can <- r$canonical %||% list()
  lid <- as.character(id$lens_id %||% can$lens_id %||% "")
  if (!nzchar(lid)) stop(sprintf("Lens record %d has no Lens ID",i),call.=FALSE)

  author_keywords <- can$keywords %||% NULL
  sidecar_id <- paste0("lens:",lid)

  out <- list(
    sidecar_identity=list(
      sidecar_record_id=sidecar_id,
      lens_id=lid,
      doi=can$doi %||% NULL
    ),
    source=list(
      provider="lens",
      source_format="lens_api_json",
      source_collection="Lens Scholarly"
    ),
    lens=list(
      authoritative_raw_payload_location="Workflow 00 restricted Zenodo harvest",
      raw_payload_duplicated_in_w01=FALSE
    ),
    mapped_fields=list(
      title=can$title %||% NULL,
      abstract=can$abstract %||% NULL,
      authors=can$authors %||% NULL,
      year=can$year %||% NULL,
      source=can$source %||% NULL,
      doi=can$doi %||% NULL,
      author_keywords=author_keywords,
      publication_type=can$publication_type %||% NULL
    ),
    provenance=list(
      adapter_workflow="workflow_01r_lens_sidecar_adapter",
      implementation_language="R",
      adapted_at=adapted_at,
      source_stage="restored_workflow00_lens_harvest",
      author_keyword_semantics="Lens canonical.keywords retained as article/author keywords with Lens provenance",
      canonical_json_modified=FALSE,
      downstream_workflows_modified=FALSE
    )
  )

  writeLines(toJSON(out,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
  ids[[i]] <- sidecar_id

  coverage[[i]] <- data.frame(
    sidecar_record_id=sidecar_id,
    lens_id=lid,
    doi=as.character(can$doi %||% NA_character_),
    title=as.character(can$title %||% NA_character_),
    author_keywords_present=present(author_keywords),
    publication_type_present=present(can$publication_type),
    stringsAsFactors=FALSE
  )
}
close(con)

if (anyDuplicated(ids)) stop("Duplicate Lens sidecar IDs",call.=FALSE)

cov <- do.call(rbind,coverage)
write.csv(cov,file.path(output_dir,"field_coverage_records.csv"),row.names=FALSE,na="")

audit <- list(
  workflow="workflow_01r_lens_sidecar_adapter",
  status="success",
  created_at=adapted_at,
  input=list(
    source="Lens restored Workflow 00 harvest",
    records_path=records_path,
    records_read=length(lines)
  ),
  output=list(
    sidecar_jsonl="lens_sidecar_records.jsonl",
    field_coverage_csv="field_coverage_records.csv",
    canonical_json_modified=FALSE,
    canonical_branch_written=FALSE,
    downstream_workflows_modified=FALSE
  ),
  identifier_checks=list(
    unique_sidecar_ids=length(unique(ids)),
    duplicate_sidecar_ids=anyDuplicated(ids),
    lens_id_present_n=sum(nzchar(cov$lens_id))
  ),
  mapped_field_coverage_percent=list(
    author_keywords=round(100*mean(cov$author_keywords_present),2),
    publication_type=round(100*mean(cov$publication_type_present),2)
  ),
  workflow01_schema_compatibility=list(
    canonical_bibliographic_fields=c("title","abstract","authors","year","source","doi","author_keywords","publication_type"),
    safely_mappable_now=c("title","abstract","authors","year","source","doi","author_keywords","publication_type"),
    source_specific_identity="lens_id",
    canonical_materialisation_deferred=TRUE,
    note="W00 is unchanged. W01 explicitly reclassifies Lens article keywords as author_keywords with Lens provenance."
  )
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"compatibility_audit.json"))

message(sprintf("PASS: wrote %d Lens sidecar records; author keyword semantics explicit; canonical JSON untouched.",length(lines)))
