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
current_seed_root <- arg("--current-seed-root")
current_final_root <- arg("--current-final-root")
current_canonical <- arg("--current-canonical")
current_canonical_manifest <- arg("--current-canonical-manifest")
previous_pointer_path <- arg("--previous-pointer")
repair_audit_path <- arg("--repair-audit",NULL)
repair_ledger_path <- arg("--repair-ledger",NULL)
output_dir <- arg("--output-dir")
run_id <- arg("--run-id")
required <- list(previous_root,current_seed_root,current_final_root,current_canonical,
                 current_canonical_manifest,previous_pointer_path,output_dir,run_id)
if(any(vapply(required,is.null,logical(1)))) {
  stop("Required: --previous-root --current-seed-root --current-final-root --current-canonical --current-canonical-manifest --previous-pointer --output-dir --run-id",call.=FALSE)
}

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(file.path(output_dir,"source_manifestations"),recursive=TRUE,showWarnings=FALSE)

read_nonempty <- function(path){
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x[nzchar(trimws(x))]
}
write_lines <- function(x,path){
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  if(length(x)) writeLines(x,path,useBytes=TRUE) else file.create(path)
}
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
align_cols <- function(a,b){
  cols <- union(names(a),names(b))
  for(nm in setdiff(cols,names(a))) a[[nm]] <- NA
  for(nm in setdiff(cols,names(b))) b[[nm]] <- NA
  list(a=a[,..cols],b=b[,..cols],cols=cols)
}
row_signature <- function(dt,cols){
  if(!nrow(dt)) return(character())
  vals <- lapply(dt[,..cols],function(x){
    y <- as.character(x)
    y[is.na(y)] <- "<NA>"
    y
  })
  do.call(paste,c(vals,sep="\u001f"))
}

previous_pointer <- fromJSON(previous_pointer_path,simplifyVector=FALSE)
if(!identical(previous_pointer$status,"published") || !(previous_pointer$state %in% c("final","delta"))) {
  stop("Previous Workflow 01 pointer must reference a published final or delta state",call.=FALSE)
}
current_manifest <- fromJSON(current_canonical_manifest,simplifyVector=FALSE)

# 1. Source manifestations: union construction is append-only by source.
source_pattern <- "_records_for_deduplication\\.jsonl$"
current_source_files <- list.files(current_seed_root,pattern=source_pattern,full.names=TRUE)
if(!length(current_source_files)) stop("Current Workflow 01 seed contains no source files",call.=FALSE)
current_sources <- sub(source_pattern,"",basename(current_source_files))
if(anyDuplicated(current_sources)) stop("Current Workflow 01 seed contains duplicate source namespaces",call.=FALSE)
names(current_source_files) <- current_sources

previous_seed_root <- file.path(previous_root,"workflow01_seed")
previous_source_files <- if(dir.exists(previous_seed_root)) list.files(previous_seed_root,pattern=source_pattern,full.names=TRUE) else character()
previous_sources <- sub(source_pattern,"",basename(previous_source_files))
if(anyDuplicated(previous_sources)) stop("Previous Workflow 01 seed contains duplicate source namespaces",call.=FALSE)
names(previous_source_files) <- previous_sources

missing_current <- setdiff(previous_sources,current_sources)
if(length(missing_current)) stop(sprintf(
  "Current Workflow 01 seed dropped previous source(s): %s",
  paste(missing_current,collapse=", ")
),call.=FALSE)

source_delta_counts <- list()
source_target <- list()
for(src in sort(current_sources)){
  prev <- if(src %in% previous_sources) previous_source_files[[src]] else NULL
  cur <- current_source_files[[src]]
  p <- if(is.null(prev)) character() else read_nonempty(prev)
  z <- read_nonempty(cur)
  if(length(z)<length(p)) stop(sprintf("%s current source state is shorter than previous state",src),call.=FALSE)
  if(length(p) && !identical(z[seq_along(p)],p)) {
    stop(sprintf("%s historical source prefix changed; append-only delta is unsafe",src),call.=FALSE)
  }
  add <- if(length(z)>length(p)) z[(length(p)+1L):length(z)] else character()
  out <- file.path(output_dir,"source_manifestations",paste0(src,"_new.jsonl"))
  write_lines(add,out)
  source_delta_counts[[src]] <- length(add)
  source_target[[src]] <- list(
    filename=basename(cur),
    previous_records=length(p),current_records=length(z),added_records=length(add),
    current_sha256=sha(cur),delta_sha256=sha(out)
  )
}

# 2. Pair-decision upserts.
prev_pairs_path <- file.path(previous_root,"workflow01_full_five_source","final_pair_decisions.csv")
cur_pairs_path <- file.path(current_final_root,"final_pair_decisions.csv")
prev_pairs <- fread(prev_pairs_path,na.strings=c("","NA"),colClasses="character")
cur_pairs <- fread(cur_pairs_path,na.strings=c("","NA"),colClasses="character")
if(anyDuplicated(prev_pairs$pair_key)||anyDuplicated(cur_pairs$pair_key)) stop("Pair keys must be unique",call.=FALSE)
missing_pairs <- setdiff(prev_pairs$pair_key,cur_pairs$pair_key)
if(length(missing_pairs)) stop(sprintf("%d previous pair decisions disappeared",length(missing_pairs)),call.=FALSE)
ab <- align_cols(prev_pairs,cur_pairs); prev_pairs <- ab$a; cur_pairs <- ab$b; pair_cols <- ab$cols
setkey(prev_pairs,pair_key); setkey(cur_pairs,pair_key)
new_pair_keys <- setdiff(cur_pairs$pair_key,prev_pairs$pair_key)
common_pair_keys <- intersect(prev_pairs$pair_key,cur_pairs$pair_key)
pcommon <- prev_pairs[J(common_pair_keys)]
ccommon <- cur_pairs[J(common_pair_keys)]
changed_pair_keys <- common_pair_keys[row_signature(pcommon,pair_cols)!=row_signature(ccommon,pair_cols)]
pair_upsert_keys <- unique(c(new_pair_keys,changed_pair_keys))
pair_upserts <- cur_pairs[J(pair_upsert_keys)]
fwrite(pair_upserts,file.path(output_dir,"pair_decision_upserts.csv"),na="")

# 3. Cluster-map upserts. Existing manifestations may change cluster IDs after a merge.
prev_map_path <- file.path(previous_root,"workflow01_full_five_source","manifestation_cluster_map.csv")
cur_map_path <- file.path(current_final_root,"manifestation_cluster_map.csv")
prev_map <- fread(prev_map_path,na.strings=c("","NA"),colClasses="character")
cur_map <- fread(cur_map_path,na.strings=c("","NA"),colClasses="character")
prev_map[,manifestation_key:=paste(source,source_record_id,sep="::")]
cur_map[,manifestation_key:=paste(source,source_record_id,sep="::")]
if(anyDuplicated(prev_map$manifestation_key)||anyDuplicated(cur_map$manifestation_key)) stop("Manifestation keys must be unique",call.=FALSE)
missing_manifestations <- setdiff(prev_map$manifestation_key,cur_map$manifestation_key)
if(length(missing_manifestations)) stop(sprintf("%d previous manifestations disappeared from cluster map",length(missing_manifestations)),call.=FALSE)
ab <- align_cols(prev_map,cur_map); prev_map <- ab$a; cur_map <- ab$b; map_cols <- setdiff(ab$cols,"manifestation_key")
setkey(prev_map,manifestation_key); setkey(cur_map,manifestation_key)
new_manifestation_keys <- setdiff(cur_map$manifestation_key,prev_map$manifestation_key)
common_manifestation_keys <- intersect(prev_map$manifestation_key,cur_map$manifestation_key)
pm <- prev_map[J(common_manifestation_keys)]
cm <- cur_map[J(common_manifestation_keys)]
changed_manifestation_keys <- common_manifestation_keys[row_signature(pm,map_cols)!=row_signature(cm,map_cols)]
map_upsert_keys <- unique(c(new_manifestation_keys,changed_manifestation_keys))
map_upserts <- cur_map[J(map_upsert_keys)]
map_upserts[,manifestation_key:=NULL]
fwrite(map_upserts,file.path(output_dir,"cluster_map_upserts.csv"),na="")

aliases_path <- file.path(current_final_root,"cluster_id_aliases.csv")
if(file.exists(aliases_path)) {
  file.copy(aliases_path,file.path(output_dir,"cluster_id_aliases.csv"),overwrite=TRUE)
} else {
  fwrite(
    data.table(retired_cluster_id=character(),surviving_cluster_id=character(),reason=character()),
    file.path(output_dir,"cluster_id_aliases.csv")
  )
}

# 4. Canonical upserts and retired IDs. Only changed/new work records are stored.
index_jsonl <- function(path,write_upserts=NULL,previous_hashes=NULL){
  con <- file(path,"rt",encoding="UTF-8"); on.exit(close(con),add=TRUE)
  ids <- character(); hashes <- character(); n <- 0L
  out <- if(!is.null(write_upserts)) file(write_upserts,"wt",encoding="UTF-8") else NULL
  if(!is.null(out)) on.exit(close(out),add=TRUE)
  repeat{
    lines <- readLines(con,n=100L,warn=FALSE)
    if(!length(lines)) break
    for(line in lines){
      if(!nzchar(trimws(line))) next
      z <- fromJSON(line,simplifyVector=FALSE)
      id <- as.character(z$identity$record_id)
      if(!nzchar(id)) stop("Canonical record lacks identity.record_id",call.=FALSE)
      stable_line <- normalise_canonical_line(line)
      h <- digest(stable_line,algo="sha256",serialize=FALSE)
      n <- n+1L; ids[[n]] <- id; hashes[[n]] <- h
      prev_h <- previous_hashes[id]
      changed <- length(prev_h)==0L || is.na(prev_h[[1L]]) || !identical(unname(prev_h[[1L]]),h)
      if(!is.null(out) && changed) {
        writeLines(line,out,useBytes=TRUE)
      }
    }
  }
  if(anyDuplicated(ids)) stop("Canonical JSONL contains duplicate record IDs",call.=FALSE)
  setNames(hashes,ids)
}

prev_canonical_path <- file.path(previous_root,"canonical","records.jsonl")
prev_hashes <- index_jsonl(prev_canonical_path)
canonical_upserts_path <- file.path(output_dir,"canonical_upserts.jsonl")
cur_hashes <- index_jsonl(current_canonical,write_upserts=canonical_upserts_path,previous_hashes=prev_hashes)
retired_ids <- setdiff(names(prev_hashes),names(cur_hashes))
write_lines(sort(retired_ids),file.path(output_dir,"canonical_retired_ids.txt"))
canonical_upsert_count <- length(read_nonempty(canonical_upserts_path))

# Preserve current target summaries/manifests needed for exact replay validation.
file.copy(file.path(current_final_root,"summary.json"),file.path(output_dir,"target_summary.json"),overwrite=TRUE)
file.copy(current_canonical_manifest,file.path(output_dir,"target_canonical_manifest.json"),overwrite=TRUE)
if(!is.null(repair_audit_path) && file.exists(repair_audit_path)) {
  file.copy(repair_audit_path,file.path(output_dir,"data_quality_repair_application.jsonl"),overwrite=TRUE)
}
if(!is.null(repair_ledger_path) && file.exists(repair_ledger_path)) {
  file.copy(repair_ledger_path,file.path(output_dir,"data_quality_repair_upserts.jsonl"),overwrite=TRUE)
} else {
  file.create(file.path(output_dir,"data_quality_repair_upserts.jsonl"))
}

# Preserve newly introduced/changed abstract-strip actions as provenance upserts.
read_jsonl_keyed <- function(path){
  if(!file.exists(path)) return(list(lines=character(),keys=character()))
  lines <- read_nonempty(path)
  if(!length(lines)) return(list(lines=character(),keys=character()))
  objs <- lapply(lines,fromJSON,simplifyVector=FALSE)
  keys <- vapply(objs,function(z)paste(z$source,z$source_record_id,sep="::"),character(1))
  if(anyDuplicated(keys)) stop(sprintf("Duplicate abstract-strip action keys in %s",path),call.=FALSE)
  list(lines=lines,keys=keys)
}
prev_strip <- read_jsonl_keyed(file.path(previous_root,"workflow01_full_five_source","abstract_strip_actions.jsonl"))
cur_strip <- read_jsonl_keyed(file.path(current_final_root,"abstract_strip_actions.jsonl"))
missing_strip <- setdiff(prev_strip$keys,cur_strip$keys)
if(length(missing_strip)) stop(sprintf("%d previous abstract-strip actions disappeared",length(missing_strip)),call.=FALSE)
prev_line <- setNames(prev_strip$lines,prev_strip$keys)
cur_line <- setNames(cur_strip$lines,cur_strip$keys)
strip_upsert_keys <- names(cur_line)[vapply(names(cur_line),function(k){
  p <- prev_line[k]
  length(p)==0L || is.na(p[[1L]]) || !identical(unname(p[[1L]]),unname(cur_line[[k]]))
},logical(1))]
write_lines(unname(cur_line[strip_upsert_keys]),file.path(output_dir,"abstract_strip_action_upserts.jsonl"))

manifest <- list(
  schema="living-evidence-map-workflow01-delta-v1",
  workflow="01",
  state="delta",
  github_run_id=as.character(run_id),
  previous=list(
    github_run_id=as.character(previous_pointer$github_run_id),
    zenodo_record_id=as.character(previous_pointer$zenodo_record_id),
    doi=as.character(previous_pointer$doi),
    manifest_sha256=as.character(previous_pointer$manifest_sha256)
  ),
  target=list(
    source_manifestations=as.integer(current_manifest$source_manifestations),
    canonical_records=as.integer(current_manifest$records),
    canonical_jsonl_sha256=as.character(current_manifest$canonical_jsonl_sha256),
    canonical_jsonl_bytes=as.numeric(current_manifest$canonical_jsonl_bytes),
    pair_decisions_state_sha256=table_state_sha(
      fread(cur_pairs_path,na.strings=c("","NA"),colClasses="character"),
      "pair_key",
      exclude_cols=c("title_i","title_j")
    ),
    cluster_map_state_sha256=table_state_sha(fread(cur_map_path,na.strings=c("","NA"),colClasses="character"),c("source","source_record_id")),
    pair_decisions_hash_excluded_descriptive_columns=c("title_i","title_j"),
    source_files=source_target
  ),
  delta=list(
    new_source_manifestations=sum(unlist(source_delta_counts)),
    new_source_manifestations_by_source=source_delta_counts,
    pair_decision_upserts=nrow(pair_upserts),
    new_pair_decisions=length(new_pair_keys),
    changed_pair_decisions=length(changed_pair_keys),
    cluster_map_upserts=nrow(map_upserts),
    new_manifestations=length(new_manifestation_keys),
    changed_existing_manifestations=length(changed_manifestation_keys),
    canonical_upserts=canonical_upsert_count,
    canonical_retired_ids=length(retired_ids),
    data_quality_repair_upserts=length(read_nonempty(file.path(output_dir,"data_quality_repair_upserts.jsonl"))),
    abstract_strip_action_upserts=length(strip_upsert_keys)
  ),
  files=list(
    pair_decision_upserts_sha256=sha(file.path(output_dir,"pair_decision_upserts.csv")),
    cluster_map_upserts_sha256=sha(file.path(output_dir,"cluster_map_upserts.csv")),
    cluster_id_aliases_sha256=sha(file.path(output_dir,"cluster_id_aliases.csv")),
    canonical_upserts_sha256=sha(canonical_upserts_path),
    canonical_retired_ids_sha256=sha(file.path(output_dir,"canonical_retired_ids.txt")),
    data_quality_repair_upserts_sha256=sha(file.path(output_dir,"data_quality_repair_upserts.jsonl")),
    abstract_strip_action_upserts_sha256=sha(file.path(output_dir,"abstract_strip_action_upserts.jsonl"))
  ),
  created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"delta_manifest.json"),useBytes=TRUE)
cat(sprintf("PASS: Workflow 01 delta: %d new manifestations, %d pair upserts, %d cluster upserts, %d canonical upserts, %d retired canonical IDs\n",
            manifest$delta$new_source_manifestations,manifest$delta$pair_decision_upserts,
            manifest$delta$cluster_map_upserts,manifest$delta$canonical_upserts,
            manifest$delta$canonical_retired_ids))
