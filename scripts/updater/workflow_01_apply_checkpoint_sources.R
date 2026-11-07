#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

`%||%` <- function(x,y) if(is.null(x)) y else x

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

previous_seed <- arg("--previous-seed")
checkpoint_dir <- arg("--checkpoint-dir")
output_dir <- arg("--output-dir")
if(any(vapply(list(previous_seed,checkpoint_dir,output_dir),is.null,logical(1)))) {
  stop("Required: --previous-seed --checkpoint-dir --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

append_raw <- function(base,delta,out){
  con_in <- file(base,"rb"); con_out <- file(out,"wb")
  on.exit(close(con_in),add=TRUE); on.exit(close(con_out),add=TRUE)
  repeat{
    z <- readBin(con_in,"raw",n=1024L*1024L)
    if(!length(z)) break
    writeBin(z,con_out)
  }
  close(con_in); on.exit(NULL,add=FALSE)
  if(file.exists(delta)&&file.info(delta)$size>0){
    z <- readBin(delta,"raw",n=file.info(delta)$size)
    writeBin(z,con_out)
  }
  close(con_out); on.exit(NULL,add=FALSE)
}
count_lines <- function(path){
  con<-file(path,"rt",encoding="UTF-8");on.exit(close(con),add=TRUE)
  n<-0L
  repeat{
    x<-readLines(con,n=10000L,warn=FALSE)
    if(!length(x))break
    n<-n+sum(nzchar(trimws(x)))
  }
  n
}

cm <- fromJSON(file.path(checkpoint_dir,"checkpoint_manifest.json"),simplifyVector=FALSE)
if(xor(is.null(cm$review_id),is.null(cm$google_sheet_id)))
  stop("Checkpoint contains incomplete review-specific routing",call.=FALSE)
if(!is.null(cm$review_id) &&
   (length(cm$review_id)!=1L || length(cm$google_sheet_id)!=1L ||
    !nzchar(as.character(cm$review_id)) || !nzchar(as.character(cm$google_sheet_id))))
  stop("Checkpoint review-specific identity must be non-empty",call.=FALSE)
source_counts_expected <- cm$new_source_manifestations_by_source
if(is.null(source_counts_expected) || !length(source_counts_expected) || is.null(names(source_counts_expected))) {
  stop("Checkpoint manifest has no source manifestation counts",call.=FALSE)
}

# Numeric deduplication indices are meaningful only for the exact ordered source
# concatenation used when they were created. A legacy checkpoint without that
# order is unsafe to reconstruct and must fail closed rather than guessing.
sources <- as.character(unlist(cm$source_order %||% character(),use.names=FALSE))
if(!length(sources)) {
  stop(
    "Checkpoint lacks authoritative source_order; refusing unsafe reconstruction of numeric deduplication indices",
    call.=FALSE
  )
}
if(any(!nzchar(sources)) || anyDuplicated(sources)) {
  stop("Checkpoint source_order is empty or non-unique",call.=FALSE)
}
if(!setequal(sources,names(source_counts_expected))) {
  stop(sprintf(
    "Checkpoint source_order/count namespaces differ; missing=%s extra=%s",
    paste(setdiff(names(source_counts_expected),sources),collapse=","),
    paste(setdiff(sources,names(source_counts_expected)),collapse=",")
  ),call.=FALSE)
}

source_files <- cm$source_files
if(is.null(source_files) || !length(source_files)){
  source_files <- setNames(lapply(sources,function(src)paste0(src,"_records_for_deduplication.jsonl")),sources)
}
if(is.null(names(source_files)) || !setequal(names(source_files),sources)) {
  stop("Checkpoint source_files namespaces do not match authoritative source_order",call.=FALSE)
}

previous_files <- if(dir.exists(previous_seed)) {
  list.files(previous_seed,pattern="_records_for_deduplication\\.jsonl$",full.names=TRUE)
} else character()
previous_names <- sub("_records_for_deduplication\\.jsonl$","",basename(previous_files))
names(previous_files) <- previous_names
missing_checkpoint <- setdiff(previous_names,sources)
if(length(missing_checkpoint)) stop(sprintf(
  "Checkpoint dropped previous source(s): %s",paste(missing_checkpoint,collapse=", ")
),call.=FALSE)

counts <- list()
reconstructed_inputs <- list()
for(src in sources){
  filename <- as.character(source_files[[src]] %||% paste0(src,"_records_for_deduplication.jsonl"))
  base <- if(src %in% previous_names) previous_files[[src]] else NULL
  if(is.null(base)){
    base <- tempfile(paste0(src,"_empty_"),fileext=".jsonl")
    file.create(base)
  }
  delta <- file.path(checkpoint_dir,"source_manifestations",paste0(src,"_new.jsonl"))
  out <- file.path(output_dir,filename)
  if(!file.exists(delta)) stop(sprintf("Missing checkpoint source input for %s",src),call.=FALSE)
  before <- count_lines(base); add <- count_lines(delta)
  append_raw(base,delta,out)
  after <- count_lines(out)
  if(after!=before+add) stop(sprintf("Source append count mismatch for %s",src),call.=FALSE)
  expected_add <- as.integer(source_counts_expected[[src]])
  if(add!=expected_add) stop(sprintf("Checkpoint source count mismatch for %s",src),call.=FALSE)
  counts[[src]] <- list(previous=before,added=add,current=after,
                       sha256=digest(file=out,algo="sha256",serialize=FALSE))
  reconstructed_inputs[[src]] <- out
}
writeLines(toJSON(list(
  schema="living-evidence-map-workflow01-source-inputs-v1",
  status="checkpoint_reconstructed",
  sources=reconstructed_inputs,
  review_id=cm$review_id,
  google_sheet_id=cm$google_sheet_id
),auto_unbox=TRUE,pretty=TRUE,null="null"),
file.path(output_dir,"source_inputs.json"))
writeLines(toJSON(list(status="PASS",source_counts=counts),auto_unbox=TRUE,pretty=TRUE),
           file.path(output_dir,"checkpoint_source_reconstruction.json"))
cat(sprintf("PASS: reconstructed checkpoint source union with %d appended manifestations\n",
            sum(vapply(counts,function(z)z$added,integer(1)))))
