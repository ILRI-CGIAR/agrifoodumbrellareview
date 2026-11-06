#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

source <- arg("--source")
current_root <- arg("--current-root")
registry_path <- arg("--registry")
output_dir <- arg("--output-dir")
run_type <- arg("--run-type","fortnightly")

if (is.null(source)||is.null(current_root)||is.null(registry_path)||is.null(output_dir)) {
  stop("--source, --current-root, --registry and --output-dir are required",call.=FALSE)
}
if (!grepl("^ebsco_[a-z0-9_]+$",source)) stop("Unsupported EBSCO source slug",call.=FALSE)
if (!(run_type %in% c("full","fortnightly","monthly","expansion"))) stop("Invalid --run-type",call.=FALSE)
if (!file.exists(registry_path)) stop(sprintf("Native-ID registry not found: %s",registry_path),call.=FALSE)

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_dir,"filtered_source"),recursive=TRUE,showWarnings=FALSE)

read_jsonl <- function(path) {
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  lapply(lines,fromJSON,simplifyVector=FALSE)
}
paths <- list.files(current_root,pattern="records\\.jsonl$",recursive=TRUE,full.names=TRUE)
paths <- paths[!grepl("source_delta|filtered_source|new_records",paths)]
if (!length(paths)) stop(sprintf("No EBSCO records.jsonl found under %s",current_root),call.=FALSE)
records <- unlist(lapply(paths,read_jsonl),recursive=FALSE)

ids <- if (length(records)) vapply(records,function(r) {
  p <- r$source$provider
  if (is.null(p)||!identical(as.character(p),source)) stop("EBSCO provider provenance mismatch",call.=FALSE)
  id <- r$sidecar_identity$sidecar_record_id
  if (is.null(id)||!nzchar(as.character(id))) stop("EBSCO record lacks sidecar_record_id",call.=FALSE)
  as.character(id)
},character(1)) else character()

dup_ids <- unique(ids[duplicated(ids)])
keep <- !duplicated(ids)
records <- records[keep]
ids <- ids[keep]

known_ids <- trimws(readLines(registry_path,warn=FALSE,encoding="UTF-8"))
known_ids <- unique(known_ids[nzchar(known_ids)])
known <- ids %in% known_ids
new <- !known
new_records <- records[new]

writeLines(ids[new],file.path(output_dir,"new_native_ids.txt"))
writeLines(ids[known],file.path(output_dir,"already_known_native_ids.txt"))
writeLines(sort(unique(c(known_ids,ids))),file.path(output_dir,"updated_native_ids.txt"))

write_jsonl <- function(rows,path) {
  con <- file(path,"wt",encoding="UTF-8")
  on.exit(close(con))
  if (length(rows)) for (r in rows) writeLines(toJSON(r,auto_unbox=TRUE,null="null",na="null",digits=NA),con)
}
write_jsonl(new_records,file.path(output_dir,"new_records.jsonl"))
write_jsonl(new_records,file.path(output_dir,"filtered_source","records.jsonl"))

manifest <- list(
  workflow="00_ebsco_native_id_reconciliation",
  status="success",
  run_type=run_type,
  source=source,
  reconciliation_key="EBSCO source slug + accession number",
  current_raw_records=length(keep),
  current_unique_records=length(ids),
  exact_duplicate_ids_suppressed=length(dup_ids),
  duplicate_native_ids=dup_ids,
  historical_unique_native_ids=length(known_ids),
  already_known_native_ids=sum(known),
  new_native_ids=sum(new),
  records_passed_downstream=sum(new),
  filtered_source_root="filtered_source",
  bibliographic_deduplication_performed=FALSE,
  doi_matching_performed=FALSE,
  fuzzy_matching_performed=FALSE,
  downstream_deduplication="Workflow 01",
  reconciled_at=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"reconciliation_manifest.json"))
message(sprintf("PASS: %s native-ID reconciliation: unique=%d known=%d new=%d",
                source,length(ids),sum(known),sum(new)))
