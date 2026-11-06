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

pairs_path <- arg("--pair-decisions")
map_path <- arg("--cluster-map")
clusters_path <- arg("--clusters")
summary_path <- arg("--summary")
corrections_path <- arg("--corrections")
output_dir <- arg("--output-dir")

required <- list(pairs_path,map_path,clusters_path,summary_path,corrections_path,output_dir)
if(any(vapply(required,is.null,logical(1)))) {
  stop("Required: --pair-decisions --cluster-map --clusters --summary --corrections --output-dir",call.=FALSE)
}
for(p in c(pairs_path,map_path,clusters_path,summary_path,corrections_path)) {
  if(!file.exists(p)) stop(sprintf("Required input not found: %s",p),call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

pairs <- fread(pairs_path,na.strings=c("","NA"))
map <- fread(map_path,na.strings=c("","NA"))
stopifnot(all(c("record_i","record_j","rescored_classification","rescored_rule","review_route") %in% names(pairs)))
stopifnot(all(c("idx","source","source_record_id","cluster_id","cluster_size","cluster_id_origin") %in% names(map)))
setorder(map,idx)
if(anyDuplicated(map$idx) || !identical(as.integer(map$idx),seq_len(nrow(map)))) {
  stop("Cluster map idx must be unique and contiguous 1..N",call.=FALSE)
}
map[,identity_key:=paste(source,source_record_id,sep="::")]
if(anyDuplicated(map$identity_key)) stop("Cluster map contains duplicate immutable source identities",call.=FALSE)

cfg <- fromJSON(corrections_path,simplifyVector=FALSE)
if(!identical(cfg$schema,"living-evidence-map-workflow01-explicit-cluster-splits-v1")) {
  stop("Unsupported explicit cluster split schema",call.=FALSE)
}
corrections <- cfg$corrections
if(!length(corrections)) stop("Explicit cluster split registry contains no corrections",call.=FALSE)
cluster_ids <- vapply(corrections,function(x)as.character(x$previous_cluster_id),character(1))
if(anyDuplicated(cluster_ids)) stop("Explicit cluster split registry repeats previous_cluster_id",call.=FALSE)

audit <- list()
overrides <- list()
split_rows <- list()
all_expected_keys <- character()

for(ci in seq_along(corrections)){
  corr <- corrections[[ci]]
  old_id <- as.character(corr$previous_cluster_id)
  groups <- corr$groups
  if(length(groups)<2L) stop(sprintf("%s must define at least two groups",old_id),call.=FALSE)
  group_names <- vapply(groups,function(g)as.character(g$group),character(1))
  if(anyDuplicated(group_names)) stop(sprintf("%s contains duplicate group labels",old_id),call.=FALSE)
  preserve_group <- as.character(corr$preserve_group)
  if(!(preserve_group %in% group_names)) stop(sprintf("%s preserve_group is not defined",old_id),call.=FALSE)

  expected_by_group <- list()
  for(g in groups){
    members <- g$members
    if(!length(members)) stop(sprintf("%s/%s has no members",old_id,g$group),call.=FALSE)
    keys <- vapply(members,function(m){
      if(is.null(m$source)||is.null(m$source_record_id)) stop(sprintf("%s/%s has incomplete member identity",old_id,g$group),call.=FALSE)
      paste(as.character(m$source),as.character(m$source_record_id),sep="::")
    },character(1))
    if(anyDuplicated(keys)) stop(sprintf("%s/%s repeats a member",old_id,g$group),call.=FALSE)
    expected_by_group[[as.character(g$group)]] <- keys
  }
  expected_keys <- unlist(expected_by_group,use.names=FALSE)
  if(anyDuplicated(expected_keys)) stop(sprintf("%s assigns one manifestation to multiple groups",old_id),call.=FALSE)
  all_expected_keys <- c(all_expected_keys,expected_keys)

  target_ids <- setNames(character(length(group_names)),group_names)
  for(gname in group_names){
    keys <- sort(expected_by_group[[gname]])
    if(gname==preserve_group){
      target_ids[[gname]] <- old_id
    } else {
      digest_keys <- sub("::",":",keys,fixed=TRUE)
      target_ids[[gname]] <- paste0("work-",substr(digest(paste(sort(digest_keys),collapse="|"),algo="sha256",serialize=FALSE),1,16))
    }
  }

  present <- map[identity_key %in% expected_keys]
  if(nrow(present)!=length(expected_keys) || !setequal(present$identity_key,expected_keys)) {
    stop(sprintf("%s explicit split members are not all present exactly once",old_id),call.=FALSE)
  }

  initial_state <- setequal(map[cluster_id==old_id,identity_key],expected_keys)
  already_state <- TRUE
  for(gname in group_names){
    cid <- target_ids[[gname]]
    keys <- expected_by_group[[gname]]
    if(!setequal(map[cluster_id==cid,identity_key],keys)) already_state <- FALSE
  }
  if(!initial_state && !already_state){
    current <- present[,.(identity_key,cluster_id)]
    stop(sprintf(
      "Explicit split target %s is neither the known unsplit state nor the known corrected state: %s",
      old_id,
      paste(sprintf("%s=%s",current$identity_key,current$cluster_id),collapse=";")
    ),call.=FALSE)
  }
  state_before <- if(initial_state) "unsplit" else "already_applied"

  key_to_group <- unlist(lapply(names(expected_by_group),function(g)setNames(rep(g,length(expected_by_group[[g]])),expected_by_group[[g]])))
  idx_to_group <- setNames(unname(key_to_group[present$identity_key]),as.character(present$idx))

  in_scope <- pairs$record_i %in% present$idx & pairs$record_j %in% present$idx
  row_ids <- which(in_scope)
  cross <- row_ids[
    unname(idx_to_group[as.character(pairs$record_i[row_ids])]) !=
    unname(idx_to_group[as.character(pairs$record_j[row_ids])])
  ]

  if(length(cross)){
    for(rr in cross){
      overrides[[length(overrides)+1L]] <- list(
        previous_cluster_id=old_id,
        record_i=as.integer(pairs$record_i[[rr]]),
        record_j=as.integer(pairs$record_j[[rr]]),
        group_i=unname(idx_to_group[[as.character(pairs$record_i[[rr]])]]),
        group_j=unname(idx_to_group[[as.character(pairs$record_j[[rr]])]]),
        before_classification=as.character(pairs$rescored_classification[[rr]]),
        before_rule=as.character(pairs$rescored_rule[[rr]]),
        after_classification="not_duplicate",
        after_rule="workflow01_explicit_cluster_split",
        reason=as.character(corr$reason)
      )
    }
    pairs[cross,`:=`(
      rescored_classification="not_duplicate",
      rescored_rule="workflow01_explicit_cluster_split",
      review_route="resolved_explicit_correction"
    )]
  }

  new_ids <- as.list(target_ids)
  for(gname in group_names){
    keys <- sort(expected_by_group[[gname]])
    cid <- target_ids[[gname]]
    origin <- if(gname==preserve_group) "preserved" else "new"
    existing_cid_keys <- map[cluster_id==cid,identity_key]
    if(length(existing_cid_keys) && !setequal(existing_cid_keys,keys) && !initial_state) {
      stop(sprintf("Generated split work ID %s is occupied by unexpected manifestations",cid),call.=FALSE)
    }
    if(initial_state && cid!=old_id && length(existing_cid_keys)) {
      stop(sprintf("Generated split work ID %s already exists before correction",cid),call.=FALSE)
    }
    map[identity_key %in% keys,`:=`(
      cluster_id=cid,
      cluster_size=length(keys),
      cluster_id_origin=origin
    )]
    split_rows[[length(split_rows)+1L]] <- data.table(
      previous_cluster_id=old_id,
      correction_group=gname,
      resulting_cluster_id=cid,
      preserved_historical_id=(gname==preserve_group),
      member_count=length(keys),
      reason=as.character(corr$reason)
    )
  }

  audit[[length(audit)+1L]] <- list(
    previous_cluster_id=old_id,
    reason=as.character(corr$reason),
    original_member_count=length(expected_keys),
    state_before=state_before,
    resulting_cluster_count=length(group_names),
    preserve_group=preserve_group,
    resulting_cluster_ids=new_ids,
    cross_group_pair_rows_overridden=length(cross)
  )
}

if(anyDuplicated(all_expected_keys)) stop("A manifestation occurs in more than one explicit split correction",call.=FALSE)
if(anyDuplicated(map[,.(idx)])) stop("Split correction duplicated cluster-map rows",call.=FALSE)
if(anyDuplicated(map[,.(identity_key)])) stop("Split correction duplicated immutable identities",call.=FALSE)

# Validate that no corrected cluster contains members from two declared groups.
for(corr in corrections){
  expected_by_group <- lapply(corr$groups,function(g)vapply(g$members,function(m)paste(m$source,m$source_record_id,sep="::"),character(1)))
  names(expected_by_group) <- vapply(corr$groups,function(g)as.character(g$group),character(1))
  result_ids <- vapply(expected_by_group,function(keys)unique(map[identity_key %in% keys,cluster_id]),character(1))
  if(anyDuplicated(result_ids)) stop(sprintf("%s explicit groups still share a work ID",corr$previous_cluster_id),call.=FALSE)
}

# Rewrite clusters.jsonl from the corrected map so archived state is internally coherent.
cluster_meta <- unique(map[,.(cluster_id,cluster_id_origin)])
cluster_rows <- lapply(cluster_meta$cluster_id,function(cid){
  z <- map[cluster_id==cid]
  list(
    cluster_id=cid,
    cluster_id_origin=as.character(z$cluster_id_origin[[1L]]),
    retired_cluster_ids=character(),
    status=if(nrow(z)>1L)"reconciled" else "singleton",
    member_count=nrow(z),
    members=lapply(seq_len(nrow(z)),function(i)list(
      idx=as.integer(z$idx[[i]]),
      source=as.character(z$source[[i]]),
      source_record_id=as.character(z$source_record_id[[i]])
    ))
  )
})

summary <- fromJSON(summary_path,simplifyVector=FALSE)
sizes <- map[,.(cluster_size=.N),by=cluster_id]
summary$clusters <- nrow(sizes)
summary$duplicate_clusters <- sum(sizes$cluster_size>1L)
summary$singleton_clusters <- sum(sizes$cluster_size==1L)
summary$manifestations_in_duplicate_clusters <- sum(sizes$cluster_size[sizes$cluster_size>1L])
summary$explicit_cluster_split_corrections <- length(corrections)
summary$explicit_cluster_split_added_work_ids <- nrow(sizes) - length(unique(fread(map_path,select="cluster_id")$cluster_id))
summary$explicit_cluster_split_registry <- basename(corrections_path)
summary$explicit_cluster_split_audit_file <- "explicit_cluster_split_audit.jsonl"
summary$explicit_cluster_split_mapping_file <- "explicit_cluster_split_mapping.csv"

fwrite(pairs,file.path(output_dir,"final_pair_decisions.csv"))
map[,identity_key:=NULL]
setorder(map,idx)
fwrite(map,file.path(output_dir,"manifestation_cluster_map.csv"))

con <- file(file.path(output_dir,"clusters.jsonl"),"wt",encoding="UTF-8")
for(z in cluster_rows) writeLines(toJSON(z,auto_unbox=TRUE,null="null"),con,useBytes=TRUE)
close(con)

acon <- file(file.path(output_dir,"explicit_cluster_split_audit.jsonl"),"wt",encoding="UTF-8")
for(z in audit) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),acon,useBytes=TRUE)
close(acon)

ocon <- file(file.path(output_dir,"explicit_cluster_split_pair_overrides.jsonl"),"wt",encoding="UTF-8")
if(length(overrides)) for(z in overrides) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),ocon,useBytes=TRUE)
close(ocon)

fwrite(rbindlist(split_rows,use.names=TRUE,fill=TRUE),file.path(output_dir,"explicit_cluster_split_mapping.csv"))
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"summary.json"),useBytes=TRUE)

cat(sprintf(
  "PASS: applied %d explicit W01 split corrections; clusters %d -> %d; added %d work IDs; pair overrides=%d\n",
  length(corrections),
  length(unique(fread(map_path,select="cluster_id")$cluster_id)),
  nrow(sizes),
  summary$explicit_cluster_split_added_work_ids,
  length(overrides)
))
