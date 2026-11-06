#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
  library(data.table)
})


`%||%` <- function(x,y) if(is.null(x)) y else x

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

previous_root <- arg("--previous-root")
current_seed_root <- arg("--current-seed-root")
incremental_rescore <- arg("--incremental-rescore")
adjudications <- arg("--adjudications")
review_package <- arg("--review-package")
pre_summary <- arg("--pre-summary")
metadata_path <- arg("--metadata")
previous_pointer <- arg("--previous-pointer")
workflow00_pointer <- arg("--workflow00-pointer")
output_dir <- arg("--output-dir")
run_id <- arg("--run-id")
required <- list(previous_root,current_seed_root,incremental_rescore,adjudications,review_package,
                 pre_summary,metadata_path,previous_pointer,workflow00_pointer,output_dir,run_id)
if(any(vapply(required,is.null,logical(1)))) stop("Missing required checkpoint argument",call.=FALSE)

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_dir,"source_manifestations"),recursive=TRUE,showWarnings=FALSE)
sha <- function(path) digest(file=path,algo="sha256",serialize=FALSE)
read_nonempty <- function(path){
  x<-readLines(path,warn=FALSE,encoding="UTF-8")
  x[nzchar(trimws(x))]
}
write_lines <- function(x,path){
  if(length(x)) writeLines(x,path,useBytes=TRUE) else file.create(path)
}

source_pattern <- "_records_for_deduplication\\.jsonl$"
current_files <- list.files(current_seed_root,pattern=source_pattern,full.names=TRUE)
if(!length(current_files)) stop("Current seed contains no source files",call.=FALSE)
current_sources <- sub(source_pattern,"",basename(current_files))
if(anyDuplicated(current_sources)) stop("Current seed contains duplicate source namespaces",call.=FALSE)
names(current_files) <- current_sources

# The deduplication row index is defined by the ordered source-input manifest.
# Never infer that order from filenames: list.files()/sort() may permute source
# blocks and make historic numeric pair indices resolve to different records.
source_inputs_path <- file.path(current_seed_root,"source_inputs.json")
if(!file.exists(source_inputs_path)) stop("Current seed lacks authoritative source_inputs.json",call.=FALSE)
source_inputs <- fromJSON(source_inputs_path,simplifyVector=FALSE)
if(!identical(source_inputs$schema,"living-evidence-map-workflow01-source-inputs-v1")) {
  stop("Unsupported current-seed source-input manifest schema",call.=FALSE)
}
source_order <- names(source_inputs$sources)
if(is.null(source_order) || !length(source_order) || anyDuplicated(source_order)) {
  stop("Current-seed source-input manifest lacks a unique ordered source list",call.=FALSE)
}
if(!setequal(source_order,current_sources)) {
  stop(sprintf(
    "Current-seed source files do not match authoritative source order; missing=%s extra=%s",
    paste(setdiff(source_order,current_sources),collapse=","),
    paste(setdiff(current_sources,source_order),collapse=",")
  ),call.=FALSE)
}


# Fingerprint the exact numeric-index -> immutable source identity mapping used
# by deduplication. Resume must reproduce this mapping byte-for-byte before any
# stored record_i/record_j pair decision is allowed to run.
if(!file.exists(metadata_path)) stop("Deduplication metadata file not found",call.=FALSE)
idx_meta <- fread(
  metadata_path,
  select=c("idx","source","source_record_id"),
  na.strings=c("","NA")
)
if(!identical(as.integer(idx_meta$idx),seq_len(nrow(idx_meta)))) {
  stop("Deduplication metadata idx must be contiguous 1..N",call.=FALSE)
}
if(any(is.na(idx_meta$source)) || any(is.na(idx_meta$source_record_id)) ||
   any(!nzchar(idx_meta$source)) || any(!nzchar(idx_meta$source_record_id))) {
  stop("Deduplication metadata contains missing source identity",call.=FALSE)
}
if(anyDuplicated(paste(idx_meta$source,idx_meta$source_record_id,sep="::"))) {
  stop("Deduplication metadata contains duplicate immutable source identity",call.=FALSE)
}
index_identity_text <- paste(
  paste(idx_meta$idx,idx_meta$source,idx_meta$source_record_id,sep="\t"),
  collapse="\n"
)
index_identity_sha256 <- digest(index_identity_text,algo="sha256",serialize=FALSE)

previous_seed_root <- file.path(previous_root,"workflow01_seed")
previous_files <- if(dir.exists(previous_seed_root)) list.files(previous_seed_root,pattern=source_pattern,full.names=TRUE) else character()
previous_sources <- sub(source_pattern,"",basename(previous_files))
if(anyDuplicated(previous_sources)) stop("Previous seed contains duplicate source namespaces",call.=FALSE)
names(previous_files) <- previous_sources
missing_current <- setdiff(previous_sources,current_sources)
if(length(missing_current)) stop(sprintf("Checkpoint current seed dropped source(s): %s",paste(missing_current,collapse=", ")),call.=FALSE)

source_counts <- list()
source_files <- list()
for(src in source_order){
  prev <- if(src %in% previous_sources) previous_files[[src]] else NULL
  cur <- current_files[[src]]
  p <- if(is.null(prev)) character() else read_nonempty(prev)
  z <- read_nonempty(cur)
  if(length(z)<length(p) || (length(p)&&!identical(z[seq_along(p)],p))){
    stop(sprintf("%s source state is not an append-only extension",src),call.=FALSE)
  }
  add <- if(length(z)>length(p)) z[(length(p)+1L):length(z)] else character()
  out <- file.path(output_dir,"source_manifestations",paste0(src,"_new.jsonl"))
  write_lines(add,out)
  source_counts[[src]] <- length(add)
  source_files[[src]] <- basename(cur)
}

file.copy(incremental_rescore,file.path(output_dir,"incremental_rescored_pairs.csv"),overwrite=TRUE)
file.copy(adjudications,file.path(output_dir,"all_adjudications.jsonl"),overwrite=TRUE)
file.copy(pre_summary,file.path(output_dir,"pre_adjudication_summary.json"),overwrite=TRUE)
file.copy(previous_pointer,file.path(output_dir,"previous_workflow01_pointer.json"),overwrite=TRUE)
file.copy(workflow00_pointer,file.path(output_dir,"workflow00_pointer.json"),overwrite=TRUE)
dir.create(file.path(output_dir,"human_review_package"),showWarnings=FALSE)
files <- list.files(review_package,full.names=TRUE,all.files=TRUE,no..=TRUE)
file.copy(files,file.path(output_dir,"human_review_package"),recursive=TRUE,overwrite=TRUE)

prev <- fromJSON(previous_pointer,simplifyVector=FALSE)
w00 <- fromJSON(workflow00_pointer,simplifyVector=FALSE)
review_manifest <- fromJSON(file.path(output_dir,"human_review_package","review_manifest.json"),simplifyVector=FALSE)
manifest <- list(
  schema="living-evidence-map-workflow01-pending-checkpoint-v1",
  workflow="01",
  state="pre_adjudication",
  github_run_id=as.character(run_id),
  previous_workflow01=list(
    github_run_id=as.character(prev$github_run_id),
    zenodo_record_id=as.character(prev$zenodo_record_id),
    manifest_sha256=as.character(prev$manifest_sha256)
  ),
  workflow00=list(
    github_run_id=as.character(w00$github_run_id %||% w00$run_id %||% ""),
    zenodo_record_id=as.character(w00$zenodo_record_id %||% ""),
    manifest_sha256=as.character(w00$manifest_sha256 %||% "")
  ),
  new_source_manifestations=sum(unlist(source_counts)),
  new_source_manifestations_by_source=source_counts,
  source_order=unname(source_order),
  source_files=source_files,
  dedup_index_identity=list(
    rows=nrow(idx_meta),
    sha256=index_identity_sha256,
    key="idx\\tsource\\tsource_record_id"
  ),
  pending_human_cases=as.integer(review_manifest$pending_count),
  queue_sha256=as.character(review_manifest$queue_sha256),
  files=list(
    incremental_rescored_pairs_sha256=sha(file.path(output_dir,"incremental_rescored_pairs.csv")),
    adjudications_sha256=sha(file.path(output_dir,"all_adjudications.jsonl")),
    pre_summary_sha256=sha(file.path(output_dir,"pre_adjudication_summary.json"))
  ),
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"checkpoint_manifest.json"),useBytes=TRUE)
cat(sprintf("PASS: pending Workflow 01 checkpoint: %d new manifestations, %d human-review cases\n",
            manifest$new_source_manifestations,manifest$pending_human_cases))
