#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
input_dir <- arg("--input-dir")
output_dir <- arg("--output-dir")
source_code <- toupper(arg("--source-code",""))
source_slug <- arg("--source-slug","")
source_collection <- arg("--source-collection","")
if (is.null(input_dir)||is.null(output_dir)) stop("--input-dir and --output-dir are required",call.=FALSE)
if (!source_code %in% c("MED","ETH","CBA","PPR")) stop("--source-code must be MED, ETH, CBA, or PPR",call.=FALSE)
if (!nzchar(source_slug)) stop("--source-slug is required",call.=FALSE)
if (!nzchar(source_collection)) stop("--source-collection is required",call.=FALSE)

resolve_source_dir <- function(root) {
  if (dir.exists(file.path(root,"raw"))) return(root)
  if (dir.exists(file.path(root,"source_child","raw"))) return(file.path(root,"source_child"))
  raw_dirs <- list.dirs(root,recursive=TRUE,full.names=TRUE)
  raw_dirs <- raw_dirs[basename(raw_dirs)=="raw"]
  if (length(raw_dirs)==1L) return(dirname(raw_dirs[[1L]]))
  stop(sprintf("Could not uniquely locate authoritative raw directory under %s",root),call.=FALSE)
}
input_dir <- resolve_source_dir(input_dir)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

`%||%` <- function(x,y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
scalar <- function(x) {
  if (is.null(x)||length(x)==0L) return(NULL)
  y <- trimws(as.character(x[[1L]])); if (!nzchar(y)) NULL else y
}
extract_list_values <- function(x, child) {
  if (is.null(x) || !is.list(x)) return(NULL)
  vals <- x[[child]]
  if (is.null(vals)) return(NULL)
  unname(vapply(vals,function(z) if (is.list(z)) scalar(z)%||%"" else as.character(z),character(1)))
}

raw_files <- sort(list.files(file.path(input_dir,"raw"),pattern="^response_[0-9]{6}\\.json$",full.names=TRUE))
records <- list()
for (rf in raw_files) {
  x <- fromJSON(rf,simplifyVector=FALSE)
  records <- c(records,x[["resultList"]][["result"]] %||% list())
}
if (!length(records)) stop(sprintf("No %s records to adapt",source_collection),call.=FALSE)

out <- vector("list",length(records)); rows <- vector("list",length(records))
for (i in seq_along(records)) {
  r <- records[[i]]
  rid <- scalar(r$id); src <- scalar(r$source)
  if (is.null(rid) || src!=source_code) stop(sprintf("Invalid %s identity at record %d",source_collection,i),call.=FALSE)

  doi <- scalar(r$doi); title <- scalar(r$title); abstract <- scalar(r$abstractText)
  year <- scalar(r$pubYear); journal <- scalar(r$journalTitle)
  if (is.null(journal) && is.list(r$journalInfo)) journal <- scalar(r$journalInfo$journal$title)
  pubtype <- extract_list_values(r$pubTypeList,"pubType")
  keywords <- extract_list_values(r$keywordList,"keyword")
  authors <- NULL
  if (is.list(r$authorList) && !is.null(r$authorList$author)) {
    authors <- lapply(r$authorList$author,function(a) list(
      full_name=scalar(a$fullName)%||%scalar(a$collectiveName),
      first_name=scalar(a$firstName),last_name=scalar(a$lastName),
      initials=scalar(a$initials),author_id=scalar(a$authorId)
    ))
  }

  sidecar_id <- paste0(source_slug,":",source_code,":",rid)
  out[[i]] <- list(
    sidecar_identity=list(
      sidecar_record_id=sidecar_id,
      source_record_id=paste(source_code,rid,sep=":"),
      europe_pmc_id=rid,
      europe_pmc_source=source_code,
      doi=doi
    ),
    source=list(provider=source_slug,source_format="europe_pmc_rest_core_json",source_collection=source_collection),
    europe_pmc=list(raw_payload=r),
    mapped_fields=list(
      title=title,abstract=abstract,authors=authors,year=year,source=journal,doi=doi,
      author_keywords=keywords,indexing_terms=NULL,publication_type=pubtype,
      author_string=scalar(r$authorString),affiliations=scalar(r$affiliation),
      language=scalar(r$language),publication_date=scalar(r$firstPublicationDate)
    ),
    provenance=list(
      adapter_workflow="workflow_01x_europe_pmc_sidecar_adapter",
      adapted_at=now_utc(),source_api="Europe PMC",source_code=source_code,
      source_slug=source_slug,canonical_json_modified=FALSE,downstream_workflows_modified=FALSE
    )
  )

  rows[[i]] <- data.frame(
    sidecar_record_id=sidecar_id,europe_pmc_id=rid,source_code=src,
    doi=doi%||%NA_character_,title=title%||%NA_character_,
    abstract_present=!is.null(abstract),authors_present=!is.null(authors)&&length(authors)>0L,
    year=year%||%NA_character_,journal=journal%||%NA_character_,
    author_keywords_present=!is.null(keywords)&&length(keywords)>0L,
    publication_type_present=!is.null(pubtype)&&length(pubtype)>0L,
    stringsAsFactors=FALSE
  )
}
ids <- vapply(out,function(x)x$sidecar_identity$sidecar_record_id,character(1))
if (anyDuplicated(ids)) stop(sprintf("Duplicate %s sidecar IDs",source_collection),call.=FALSE)

jsonl_path <- file.path(output_dir,paste0(source_slug,"_sidecar_records.jsonl"))
con <- file(jsonl_path,"wt",encoding="UTF-8")
for (r in out) writeLines(toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
close(con)
cov <- do.call(rbind,rows)
write.csv(cov,file.path(output_dir,"field_coverage_records.csv"),row.names=FALSE,na="")

present <- function(x)!is.na(x)&nzchar(as.character(x))
pct <- function(x)round(100*mean(x),1)
audit <- list(
  workflow="workflow_01x_europe_pmc_sidecar_adapter",status="success",created_at=now_utc(),
  input=list(source=source_collection,provider="Europe PMC",source_code=source_code,records_read=length(records)),
  output=list(sidecar_jsonl=basename(jsonl_path),field_coverage_csv="field_coverage_records.csv",canonical_json_modified=FALSE),
  identifier_checks=list(unique_sidecar_ids=length(unique(ids)),duplicate_sidecar_ids=anyDuplicated(ids),doi_present_n=sum(present(cov$doi))),
  mapped_field_coverage_percent=list(
    title=pct(present(cov$title)),abstract=pct(cov$abstract_present),authors=pct(cov$authors_present),
    year=pct(present(cov$year)),journal=pct(present(cov$journal)),doi=pct(present(cov$doi)),
    author_keywords=pct(cov$author_keywords_present),publication_type=pct(cov$publication_type_present)
  ),
  workflow01_schema_compatibility=list(
    canonical_bibliographic_fields=c("title","abstract","authors","year","source","doi","author_keywords","publication_type"),
    source_specific_identity="Europe PMC source code + ID",canonical_materialisation_deferred=TRUE
  )
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),file.path(output_dir,"compatibility_audit.json"))
message(sprintf("PASS: wrote %d %s sidecar records; canonical JSON untouched.",length(out),source_collection))
