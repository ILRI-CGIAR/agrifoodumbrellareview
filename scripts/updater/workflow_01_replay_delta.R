#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
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

previous_root <- arg("--previous-root")
delta_dir <- arg("--delta-dir")
output_root <- arg("--output-root")
if(any(vapply(list(previous_root,delta_dir,output_root),is.null,logical(1)))) {
  stop("Required: --previous-root --delta-dir --output-root",call.=FALSE)
}

manifest_path <- file.path(delta_dir,"delta_manifest.json")
if(!file.exists(manifest_path)) stop("Delta manifest not found",call.=FALSE)
m <- fromJSON(manifest_path,simplifyVector=FALSE)
if(!identical(m$schema,"living-evidence-map-workflow01-delta-v1")) stop("Unsupported delta schema",call.=FALSE)

dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_root,"workflow01_seed"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_root,"workflow01_full_five_source"),recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_root,"canonical"),recursive=TRUE,showWarnings=FALSE)

sha <- function(path) digest(file=path,algo="sha256",serialize=FALSE)
table_state_sha <- function(dt,key_cols,exclude_cols=character()){
  x <- copy(dt)
  missing <- setdiff(key_cols,names(x))
  if(length(missing)) stop(sprintf("State-hash key columns missing: %s",paste(missing,collapse=", ")),call.=FALSE)
  setorderv(x,key_cols)
  cols <- sort(setdiff(names(x),exclude_cols))
  vals <- lapply(x[,..cols],function(v){
    if(inherits(v,"integer64")) v <- as.character(v)
    z <- as.character(v)
    z[is.na(z)] <- "<NA>"
    z
  })
  rows <- if(nrow(x)) do.call(paste,c(vals,sep="\u001f")) else character()
  payload <- paste(c(paste(cols,collapse="\u001f"),rows),collapse="\n")
  digest(payload,algo="sha256",serialize=FALSE)
}
normalise_canonical_line <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  if(!is.null(z$provenance)){
    z$provenance$workflow01_run_id <- NULL
    z$provenance$generated_at_utc <- NULL
  }
  toJSON(z,auto_unbox=TRUE,null="null",na="null")
}
`%||%` <- function(x,y) if(is.null(x)) y else x
read_nonempty <- function(path){
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x[nzchar(trimws(x))]
}
write_lines <- function(x,path){
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  if(length(x)) writeLines(x,path,useBytes=TRUE) else file.create(path)
}
append_file <- function(base,delta,out){
  in_con <- file(base,"rb"); on.exit(close(in_con),add=TRUE)
  out_con <- file(out,"wb"); on.exit(close(out_con),add=TRUE)
  repeat{
    buf <- readBin(in_con,"raw",n=1024L*1024L)
    if(!length(buf)) break
    writeBin(buf,out_con)
  }
  close(in_con); on.exit(NULL,add=FALSE)
  if(file.exists(delta) && file.info(delta)$size>0){
    # Delta JSONL files always contain complete newline-terminated records.
    d <- readBin(delta,"raw",n=file.info(delta)$size)
    writeBin(d,out_con)
  }
  close(out_con); on.exit(NULL,add=FALSE)
}

# 1. Replay append-only source manifestations.
source_target <- m$target$source_files
if(is.null(source_target) || !length(source_target) || is.null(names(source_target))) {
  stop("Delta target contains no source_files",call.=FALSE)
}
target_sources <- sort(names(source_target))
previous_seed_root <- file.path(previous_root,"workflow01_seed")
previous_files <- if(dir.exists(previous_seed_root)) {
  list.files(previous_seed_root,pattern="_records_for_deduplication\\.jsonl$",full.names=TRUE)
} else character()
previous_names <- sub("_records_for_deduplication\\.jsonl$","",basename(previous_files))
names(previous_files) <- previous_names
missing_target <- setdiff(previous_names,target_sources)
if(length(missing_target)) stop(sprintf(
  "Delta target dropped previous source(s): %s",paste(missing_target,collapse=", ")
),call.=FALSE)

for(src in target_sources){
  target <- source_target[[src]]
  filename <- as.character(target$filename %||% paste0(src,"_records_for_deduplication.jsonl"))
  base <- if(src %in% previous_names) previous_files[[src]] else NULL
  if(is.null(base)){
    base <- tempfile(paste0(src,"_empty_"),fileext=".jsonl")
    file.create(base)
  }
  delta <- file.path(delta_dir,"source_manifestations",paste0(src,"_new.jsonl"))
  out <- file.path(output_root,"workflow01_seed",filename)
  if(!file.exists(delta)) stop(sprintf("Missing source replay delta for %s",src),call.=FALSE)
  append_file(base,delta,out)
  if(!identical(tolower(sha(out)),tolower(as.character(target$current_sha256)))) {
    stop(sprintf("%s replay SHA-256 does not match target",src),call.=FALSE)
  }
}

align_cols <- function(a,b){
  cols <- union(names(a),names(b))
  for(nm in setdiff(cols,names(a))) a[[nm]] <- NA
  for(nm in setdiff(cols,names(b))) b[[nm]] <- NA
  list(a=a[,..cols],b=b[,..cols],cols=cols)
}

# 2. Replay pair-decision upserts.
prev_pairs_path <- file.path(previous_root,"workflow01_full_five_source","final_pair_decisions.csv")
prev_pairs <- fread(prev_pairs_path,na.strings=c("","NA"),colClasses="character")
up_pairs <- fread(file.path(delta_dir,"pair_decision_upserts.csv"),na.strings=c("","NA"),colClasses="character")
pair_out <- file.path(output_root,"workflow01_full_five_source","final_pair_decisions.csv")
if(nrow(up_pairs)){
  if(anyDuplicated(up_pairs$pair_key)) stop("Delta pair upserts contain duplicate pair_key",call.=FALSE)
  ab <- align_cols(prev_pairs,up_pairs); prev_pairs <- ab$a; up_pairs <- ab$b
  prev_pairs <- prev_pairs[!(pair_key %in% up_pairs$pair_key)]
  pairs <- rbindlist(list(prev_pairs,up_pairs),use.names=TRUE,fill=TRUE)
  setorder(pairs,pair_key)
  fwrite(pairs,pair_out,na="")
} else {
  file.copy(prev_pairs_path,pair_out,overwrite=TRUE)
}
pair_state_sha <- table_state_sha(
  fread(pair_out,na.strings=c("","NA"),colClasses="character"),
  "pair_key",
  exclude_cols=as.character(unlist(m$target$pair_decisions_hash_excluded_descriptive_columns %||% character()))
)
if(!identical(tolower(pair_state_sha),tolower(as.character(m$target$pair_decisions_state_sha256)))) {
  stop("Replayed pair-decision semantic state hash does not match target",call.=FALSE)
}

# 3. Replay cluster-map upserts.
prev_map_path <- file.path(previous_root,"workflow01_full_five_source","manifestation_cluster_map.csv")
prev_map <- fread(prev_map_path,na.strings=c("","NA"),colClasses="character")
up_map <- fread(file.path(delta_dir,"cluster_map_upserts.csv"),na.strings=c("","NA"),colClasses="character")
map_out <- file.path(output_root,"workflow01_full_five_source","manifestation_cluster_map.csv")
if(nrow(up_map)){
  prev_map[,manifestation_key:=paste(source,source_record_id,sep="::")]
  up_map[,manifestation_key:=paste(source,source_record_id,sep="::")]
  if(anyDuplicated(up_map$manifestation_key)) stop("Delta cluster-map upserts contain duplicate manifestation key",call.=FALSE)
  ab <- align_cols(prev_map,up_map); prev_map <- ab$a; up_map <- ab$b
  prev_map <- prev_map[!(manifestation_key %in% up_map$manifestation_key)]
  cmap <- rbindlist(list(prev_map,up_map),use.names=TRUE,fill=TRUE)
  cmap[,manifestation_key:=NULL]
  suppressWarnings(cmap[, idx_order__ := as.integer(idx)])
  setorder(cmap,idx_order__,source,source_record_id,na.last=TRUE)
  cmap[,idx_order__:=NULL]
  fwrite(cmap,map_out,na="")
} else {
  file.copy(prev_map_path,map_out,overwrite=TRUE)
}
map_state_sha <- table_state_sha(fread(map_out,na.strings=c("","NA"),colClasses="character"),c("source","source_record_id"))
if(!identical(tolower(map_state_sha),tolower(as.character(m$target$cluster_map_state_sha256)))) {
  stop("Replayed cluster-map semantic state hash does not match target",call.=FALSE)
}

# Preserve target summary exactly.
file.copy(file.path(delta_dir,"target_summary.json"),
          file.path(output_root,"workflow01_full_five_source","summary.json"),overwrite=TRUE)

# 4. Replay canonical JSONL as a sorted merge of previous records and upserts.
record_id <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  as.character(z$identity$record_id)
}
prev_path <- file.path(previous_root,"canonical","records.jsonl")
up_path <- file.path(delta_dir,"canonical_upserts.jsonl")
retired <- read_nonempty(file.path(delta_dir,"canonical_retired_ids.txt"))

up_lines <- read_nonempty(up_path)
up_ids <- if(length(up_lines)) vapply(up_lines,record_id,character(1)) else character()
if(length(up_ids) && is.unsorted(up_ids,strictly=FALSE)) stop("Canonical upserts are not sorted by record ID",call.=FALSE)
if(anyDuplicated(up_ids)) stop("Canonical upserts contain duplicate record IDs",call.=FALSE)

canonical_out <- file.path(output_root,"canonical","records.jsonl")
pin <- file(prev_path,"rt",encoding="UTF-8")
pout <- file(canonical_out,"wt",encoding="UTF-8")
on.exit(close(pin),add=TRUE); on.exit(close(pout),add=TRUE)
u <- 1L
repeat{
  pl <- readLines(pin,n=1L,warn=FALSE)
  if(!length(pl)) break
  if(!nzchar(trimws(pl))) next
  pid <- record_id(pl)
  while(u<=length(up_ids) && up_ids[[u]] < pid){
    writeLines(up_lines[[u]],pout,useBytes=TRUE)
    u <- u+1L
  }
  if(u<=length(up_ids) && identical(up_ids[[u]],pid)){
    writeLines(up_lines[[u]],pout,useBytes=TRUE)
    u <- u+1L
  } else if(!(pid %in% retired)){
    writeLines(normalise_canonical_line(pl),pout,useBytes=TRUE)
  }
}
while(u<=length(up_ids)){
  writeLines(up_lines[[u]],pout,useBytes=TRUE)
  u <- u+1L
}
close(pin); close(pout); on.exit(NULL,add=FALSE)

target_canonical_sha <- as.character(m$target$canonical_jsonl_sha256)
if(!identical(tolower(sha(canonical_out)),tolower(target_canonical_sha))) {
  stop("Replayed canonical JSONL SHA-256 does not match target",call.=FALSE)
}
file.copy(file.path(delta_dir,"target_canonical_manifest.json"),
          file.path(output_root,"canonical","canonical_manifest.json"),overwrite=TRUE)

# Preserve current alias ledger and cumulative provenance ledgers.
file.copy(file.path(delta_dir,"cluster_id_aliases.csv"),
          file.path(output_root,"workflow01_full_five_source","cluster_id_aliases.csv"),overwrite=TRUE)
dir.create(file.path(output_root,"provenance"),showWarnings=FALSE)

if(file.exists(file.path(delta_dir,"data_quality_repair_application.jsonl"))){
  file.copy(file.path(delta_dir,"data_quality_repair_application.jsonl"),
            file.path(output_root,"provenance","data_quality_repair_application.jsonl"),overwrite=TRUE)
}

repair_key <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  paste(as.character(z$review_case_id %||% ""),as.character(z$source),
        as.character(z$source_record_id),as.character(z$action),sep="|")
}
merge_jsonl_upserts <- function(previous_path,upsert_path,out_path,key_fun){
  prev <- if(file.exists(previous_path)) read_nonempty(previous_path) else character()
  ups <- if(file.exists(upsert_path)) read_nonempty(upsert_path) else character()
  pkeys <- if(length(prev)) vapply(prev,key_fun,character(1)) else character()
  ukeys <- if(length(ups)) vapply(ups,key_fun,character(1)) else character()
  if(anyDuplicated(pkeys)) stop(sprintf("Previous ledger contains duplicate keys: %s",previous_path),call.=FALSE)
  if(anyDuplicated(ukeys)) stop(sprintf("Delta ledger contains duplicate keys: %s",upsert_path),call.=FALSE)
  store <- setNames(as.list(prev),pkeys)
  if(length(ups)) for(i in seq_along(ups)) store[[ukeys[[i]]]] <- ups[[i]]
  keys <- sort(names(store))
  lines <- if(length(keys)) unlist(store[keys],use.names=FALSE) else character()
  write_lines(lines,out_path)
}

merge_jsonl_upserts(
  file.path(previous_root,"provenance","cumulative_data_quality_repairs.jsonl"),
  file.path(delta_dir,"data_quality_repair_upserts.jsonl"),
  file.path(output_root,"provenance","cumulative_data_quality_repairs.jsonl"),
  repair_key
)

strip_key <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  paste(as.character(z$source),as.character(z$source_record_id),sep="::")
}
merge_jsonl_upserts(
  file.path(previous_root,"workflow01_full_five_source","abstract_strip_actions.jsonl"),
  file.path(delta_dir,"abstract_strip_action_upserts.jsonl"),
  file.path(output_root,"workflow01_full_five_source","abstract_strip_actions.jsonl"),
  strip_key
)

audit <- list(
  schema="living-evidence-map-workflow01-delta-replay-audit-v1",
  status="PASS",
  previous_github_run_id=as.character(m$previous$github_run_id),
  delta_github_run_id=as.character(m$github_run_id),
  source_manifestations=as.integer(m$target$source_manifestations),
  canonical_records=as.integer(m$target$canonical_records),
  canonical_jsonl_sha256=target_canonical_sha,
  pair_decisions_state_sha256=as.character(m$target$pair_decisions_state_sha256),
  cluster_map_state_sha256=as.character(m$target$cluster_map_state_sha256),
  replayed_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_root,"workflow01_delta_replay_audit.json"),useBytes=TRUE)
cat(sprintf("PASS: replayed Workflow 01 delta to %d manifestations and %d canonical records with verified target state hashes\n",
            audit$source_manifestations,audit$canonical_records))
