#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

metadata_path <- arg("--metadata")
qgrams_path <- arg("--qgrams")
output_dir <- arg("--output-dir")
source_label <- arg("--source-label","workflow01")
if (any(vapply(list(metadata_path,qgrams_path,output_dir),is.null,logical(1)))) {
  stop("Required: --metadata --qgrams --output-dir",call.=FALSE)
}
if (!file.exists(metadata_path)) stop("Metadata file not found",call.=FALSE)
if (!file.exists(qgrams_path)) stop("Q-gram signature file not found",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

meta <- fread(metadata_path,na.strings=c("","NA"))
qg <- fread(qgrams_path,na.strings=c("","NA"))

required_meta <- c("idx","source","source_record_id","title_norm","doi_norm","doi_family",
                   "abstract_hash","author_norm","year","journal_norm","volume_norm",
                   "issue_norm","pages_norm")
missing_meta <- setdiff(required_meta,names(meta))
if (length(missing_meta)) stop(sprintf("Metadata missing required columns: %s",paste(missing_meta,collapse=", ")),call.=FALSE)
required_qg <- c("idx","qgram")
missing_qg <- setdiff(required_qg,names(qg))
if (length(missing_qg)) stop(sprintf("Q-gram file missing required columns: %s",paste(missing_qg,collapse=", ")),call.=FALSE)

if (!identical(as.integer(meta$idx),seq_len(nrow(meta)))) {
  stop("Metadata idx must be contiguous 1..N; persistent index will not silently reindex",call.=FALSE)
}
keys <- paste(meta$source,meta$source_record_id,sep="::")
if (anyDuplicated(keys)) stop("Metadata contains duplicate source/source_record_id keys",call.=FALSE)
if (nrow(qg) && any(!qg$idx %in% meta$idx)) stop("Q-gram signatures reference metadata indices that do not exist",call.=FALSE)
if (nrow(qg) && anyDuplicated(qg[,paste(idx,qgram,sep="::")])) stop("Duplicate idx/qgram rows in signatures",call.=FALSE)

index_meta_path <- file.path(output_dir,"dedup_index_metadata.csv")
index_qg_path <- file.path(output_dir,"dedup_index_title_qgrams.csv")
registry_path <- file.path(output_dir,"dedup_manifestation_registry.csv")

fwrite(meta[,..required_meta],index_meta_path)
fwrite(qg[,.(idx,qgram,df)],index_qg_path)
fwrite(meta[,.(source,source_record_id,idx)],registry_path)

hash_file <- function(path) digest(file=path,algo="sha256",serialize=FALSE)
manifest <- list(
  schema="living-evidence-map-dedup-index-v1",
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  source_label=source_label,
  manifestations=nrow(meta),
  title_qgram_rows=nrow(qg),
  unique_sources=sort(unique(meta$source)),
  files=list(
    dedup_index_metadata.csv=list(sha256=hash_file(index_meta_path),rows=nrow(meta)),
    dedup_index_title_qgrams.csv=list(sha256=hash_file(index_qg_path),rows=nrow(qg)),
    dedup_manifestation_registry.csv=list(sha256=hash_file(registry_path),rows=nrow(meta))
  ),
  invariants=list(
    contiguous_idx=TRUE,
    unique_source_record_ids=TRUE,
    qgram_indices_resolve=TRUE
  )
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"dedup_index_manifest.json"))

cat(sprintf("PASS: persistent deduplication index built for %d manifestations (%d q-gram rows)\n",
            nrow(meta),nrow(qg)))
