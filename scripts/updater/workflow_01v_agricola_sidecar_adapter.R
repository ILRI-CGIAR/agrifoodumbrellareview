#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag))
  args[[i+1L]]
}

input_dir <- arg("--input-dir","inputs/agricola_ingestion")
output_dir <- arg("--output-dir","outputs/updater/agricola_sidecar_adapter")
if (!dir.exists(input_dir)) stop(sprintf("Input directory does not exist: %s",input_dir))
resolve_source_dir <- function(root) {
  direct <- file.path(root,"raw")
  child <- file.path(root,"source_child","raw")
  if (dir.exists(direct)) return(root)
  if (dir.exists(child)) return(file.path(root,"source_child"))
  raw_dirs <- list.dirs(root,recursive=TRUE,full.names=TRUE)
  raw_dirs <- raw_dirs[basename(raw_dirs)=="raw"]
  if (length(raw_dirs)==1L) return(dirname(raw_dirs[[1L]]))
  stop(sprintf("Could not uniquely locate authoritative raw directory under %s",root))
}
input_dir <- resolve_source_dir(input_dir)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
scalar <- function(x) {
  if (is.null(x)||length(x)==0L) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)) NULL else y
}
as_vec <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x)) return(unname(as.character(x)))
  unlist(x,use.names=FALSE)
}
extract_list_values <- function(x, child) {
  if (is.null(x) || !is.list(x)) return(NULL)
  vals <- x[[child]]
  if (is.null(vals)) return(NULL)
  unname(vapply(vals,function(z) {
    if (is.list(z)) scalar(z) %||% "" else as.character(z)
  },character(1)))
}

raw_files <- sort(list.files(file.path(input_dir,"raw"),pattern="\\.json$",full.names=TRUE))
if (!length(raw_files)) stop("No AGRICOLA raw JSON files found")

records <- list()
for (rf in raw_files) {
  x <- fromJSON(rf,simplifyVector=FALSE)
  records <- c(records,x[["resultList"]][["result"]] %||% list())
}
if (!length(records)) stop("No AGRICOLA records to adapt")

out <- vector("list",length(records))
rows <- vector("list",length(records))
for (i in seq_along(records)) {
  r <- records[[i]]
  agr_id <- scalar(r$id)
  src <- scalar(r$source)
  if (is.null(agr_id) || src!="AGR") stop(sprintf("Invalid AGRICOLA identity at record %d",i))

  doi <- scalar(r$doi)
  title <- scalar(r$title)
  abstract <- scalar(r$abstractText)
  year <- scalar(r$pubYear)
  journal <- scalar(r$journalTitle)
  if (is.null(journal) && is.list(r$journalInfo)) journal <- scalar(r$journalInfo$journal$title)
  pubtype <- extract_list_values(r$pubTypeList,"pubType")
  keywords <- extract_list_values(r$keywordList,"keyword")
  authors <- NULL
  if (is.list(r$authorList) && !is.null(r$authorList$author)) {
    authors <- lapply(r$authorList$author,function(a) list(
      full_name = scalar(a$fullName) %||% scalar(a$collectiveName),
      first_name = scalar(a$firstName),
      last_name = scalar(a$lastName),
      initials = scalar(a$initials),
      author_id = scalar(a$authorId)
    ))
  }

  sidecar_id <- paste0("agricola:",agr_id)
  out[[i]] <- list(
    sidecar_identity=list(
      sidecar_record_id=sidecar_id,
      agricola_id=agr_id,
      europe_pmc_source=src,
      doi=doi
    ),
    source=list(
      provider="agricola_via_europe_pmc",
      source_format="europe_pmc_rest_core_json",
      source_collection="AGRICOLA"
    ),
    agricola=list(raw_payload=r),
    mapped_fields=list(
      title=title,
      abstract=abstract,
      authors=authors,
      year=year,
      source=journal,
      doi=doi,
      author_keywords=keywords,
      publication_type=pubtype,
      author_string=scalar(r$authorString),
      affiliation=scalar(r$affiliation),
      language=scalar(r$language),
      first_publication_date=scalar(r$firstPublicationDate)
    ),
    provenance=list(
      adapter_workflow="workflow_01v_agricola_sidecar_adapter",
      adapted_at=now_utc(),
      source_api="Europe PMC",
      source_code="AGR",
      canonical_json_modified=FALSE,
      downstream_workflows_modified=FALSE
    )
  )

  rows[[i]] <- data.frame(
    sidecar_record_id=sidecar_id,
    agricola_id=agr_id,
    doi=doi %||% NA_character_,
    title=title %||% NA_character_,
    abstract_present=!is.null(abstract),
    authors_present=!is.null(authors)&&length(authors)>0L,
    year=year %||% NA_character_,
    source=journal %||% NA_character_,
    author_keywords_present=!is.null(keywords)&&length(keywords)>0L,
    publication_type_present=!is.null(pubtype)&&length(pubtype)>0L,
    stringsAsFactors=FALSE
  )
}

ids <- vapply(out,function(x)x$sidecar_identity$sidecar_record_id,character(1))
if (anyDuplicated(ids)) stop("Duplicate AGRICOLA sidecar IDs")

jsonl <- file(file.path(output_dir,"agricola_sidecar_records.jsonl"),"wt",encoding="UTF-8")
for (r in out) writeLines(toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),jsonl)
close(jsonl)

cov <- do.call(rbind,rows)
write.csv(cov,file.path(output_dir,"field_coverage_records.csv"),row.names=FALSE,na="")

pct <- function(x) round(100*mean(x),1)
present <- function(x) !is.na(x)&nzchar(as.character(x))
audit <- list(
  workflow="workflow_01v_agricola_sidecar_adapter",
  status="success",
  created_at=now_utc(),
  input=list(
    source="AGRICOLA via Europe PMC REST API",
    records_read=length(records)
  ),
  output=list(
    sidecar_jsonl="agricola_sidecar_records.jsonl",
    field_coverage_csv="field_coverage_records.csv",
    canonical_json_modified=FALSE,
    canonical_branch_written=FALSE,
    downstream_workflows_modified=FALSE
  ),
  identifier_checks=list(
    unique_sidecar_ids=length(unique(ids)),
    duplicate_sidecar_ids=anyDuplicated(ids),
    agricola_id_present_n=sum(present(cov$agricola_id)),
    doi_present_n=sum(present(cov$doi)),
    doi_missing_n=sum(!present(cov$doi))
  ),
  mapped_field_coverage_percent=list(
    title=pct(present(cov$title)),
    abstract=pct(cov$abstract_present),
    authors=pct(cov$authors_present),
    year=pct(present(cov$year)),
    source=pct(present(cov$source)),
    doi=pct(present(cov$doi)),
    author_keywords=pct(cov$author_keywords_present),
    publication_type=pct(cov$publication_type_present)
  ),
  workflow01_schema_compatibility=list(
    canonical_bibliographic_fields=c("title","abstract","authors","year","source","doi","author_keywords","publication_type"),
    safely_mappable_now=c("title","abstract","authors","year","source","doi","author_keywords","publication_type"),
    source_specific_identity="agricola_id",
    canonical_materialisation_deferred=TRUE,
    note="Diagnostic source sidecar only. Canonical materialisation is performed later by the source-agnostic Workflow 01 canonical builder."
  )
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),file.path(output_dir,"compatibility_audit.json"))
message(sprintf("PASS: wrote %d AGRICOLA sidecar records; canonical JSON untouched.",length(out)))
