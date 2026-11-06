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
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

combined_path <- arg("--combined-decisions")
manifestation_map_path <- arg("--manifestation-map")
llm_path <- arg("--llm-adjudications")
human_path <- arg("--human-decisions",NULL)
integrity_path <- arg("--human-integrity-manifest",NULL)
data_quality_repairs_path <- arg("--data-quality-repairs",NULL)
precomputed_strip_actions_path <- arg("--precomputed-strip-actions",NULL)
previous_cluster_map_path <- arg("--previous-cluster-map",NULL)
explicit_splits_path <- arg("--explicit-cluster-splits",NULL)
output_dir <- arg("--output-dir")
if (any(vapply(list(combined_path,manifestation_map_path,llm_path,human_path,integrity_path,output_dir),is.null,logical(1)))) {
  stop("Required: --combined-decisions --manifestation-map --llm-adjudications --human-decisions --human-integrity-manifest --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

pairs <- fread(combined_path,na.strings=c("","NA"))
meta <- fread(manifestation_map_path,na.strings=c("","NA"))
stopifnot(all(c("pair_key","rescored_classification","rescored_rule") %in% names(pairs)))
stopifnot(all(c("idx","source","source_record_id") %in% names(meta)))
if (anyDuplicated(pairs$pair_key)) stop("Combined pair decisions contain duplicate pair keys",call.=FALSE)
setkey(pairs,pair_key)
setorder(meta,idx)
if (!identical(meta$idx,seq_len(nrow(meta)))) stop("Manifestation map idx is not contiguous",call.=FALSE)

read_jsonl <- function(path) {
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x <- x[nzchar(trimws(x))]
  lapply(x,fromJSON,simplifyVector=FALSE)
}
llm <- read_jsonl(llm_path)
human <- read_jsonl(human_path)
integrity <- fromJSON(integrity_path,simplifyVector=FALSE)
if (is.null(integrity$resume_allowed) || !isTRUE(integrity$resume_allowed)) {
  stop("Human-review integrity manifest does not permit resume",call.=FALSE)
}

human_review_ids <- vapply(Filter(function(x)identical(x$promotion,"human_review"),llm),
                           function(x)as.character(x$review_case_id),character(1))
if (anyDuplicated(human_review_ids)) stop("LLM adjudications contain duplicate human-review IDs",call.=FALSE)
human_ids <- vapply(human,function(x)as.character(x$review_case_id),character(1))
if (anyDuplicated(human_ids)) stop("Human decisions contain duplicate review_case_id",call.=FALSE)
unknown_human <- setdiff(human_ids,human_review_ids)
missing_human <- setdiff(human_review_ids,human_ids)
if (length(unknown_human)) stop(sprintf("Human decisions contain %d IDs outside the LLM human-review queue",length(unknown_human)),call.=FALSE)
if (length(missing_human)) stop(sprintf("Human decisions are missing %d required LLM human-review IDs",length(missing_human)),call.=FALSE)
if (length(human_ids) != length(human_review_ids)) stop("Human decision count does not exactly match human-review queue",call.=FALSE)
if (!is.null(integrity$active_decisions) && as.integer(integrity$active_decisions) != length(human_ids)) {
  stop("Human-review integrity manifest decision count does not match supplied decisions",call.=FALSE)
}
human_by_id <- setNames(human,human_ids)

# Abstract/title mismatch detected during duplicate adjudication is a metadata-cleaning
# action, not a second deduplication stage. Emit immutable source-record strip actions
# for the downstream corpus materialisation step. Workflow 02 will then discover the
# resulting missing abstracts through its normal corpus scan.
strip_by_key <- list()
if (!is.null(precomputed_strip_actions_path) && file.exists(precomputed_strip_actions_path)) {
  precomputed_strips <- read_jsonl(precomputed_strip_actions_path)
  for (z in precomputed_strips) {
    if (is.null(z$source) || is.null(z$source_record_id) ||
        is.null(z$action) || !identical(as.character(z$action),"strip_abstract")) {
      stop("Invalid precomputed strip action",call.=FALSE)
    }
    key <- paste(as.character(z$source),as.character(z$source_record_id),sep=":")
    if (!is.null(strip_by_key[[key]])) stop(sprintf("Duplicate precomputed strip action for %s",key),call.=FALSE)
    strip_by_key[[key]] <- z
  }
}
for (a in llm) {
  for (side in c("record_i","record_j")) {
    flag_name <- if (identical(side,"record_i")) "abstract_consistent_with_record_i" else "abstract_consistent_with_record_j"
    if (is.null(a[[flag_name]]) || isTRUE(a[[flag_name]])) next
    rec <- a[[side]]
    if (is.null(rec$source) || is.null(rec$source_record_id)) {
      stop(sprintf("Abstract mismatch for %s lacks immutable source identity",a$review_case_id),call.=FALSE)
    }
    key <- paste(as.character(rec$source),as.character(rec$source_record_id),sep=":")
    if (is.null(strip_by_key[[key]])) {
      strip_by_key[[key]] <- list(
        source=as.character(rec$source),
        source_record_id=as.character(rec$source_record_id),
        action="strip_abstract",
        reason="title_abstract_mismatch_detected_during_deduplication",
        original_abstract=if(is.null(rec$abstract)) NULL else rec$abstract,
        supporting_review_case_ids=as.character(a$review_case_id),
        pair_keys=as.character(a$pair_key)
      )
    } else {
      strip_by_key[[key]]$supporting_review_case_ids <- unique(c(strip_by_key[[key]]$supporting_review_case_ids,as.character(a$review_case_id)))
      strip_by_key[[key]]$pair_keys <- unique(c(strip_by_key[[key]]$pair_keys,as.character(a$pair_key)))
    }
  }
}
# Explicit human data-quality repairs are authoritative when they request
# stripping an abstract from a specific immutable source manifestation. Other
# repair types are not applied here; Workflow 02 handles later metadata repair.
if (!is.null(data_quality_repairs_path) && file.exists(data_quality_repairs_path)) {
  repairs <- read_jsonl(data_quality_repairs_path)
  for (r in repairs) {
    if (is.null(r$action) || !identical(as.character(r$action),"strip_abstract")) next
    if (is.null(r$source) || is.null(r$source_record_id)) {
      stop(sprintf("Human strip_abstract repair %s lacks immutable source identity",r$review_case_id),call.=FALSE)
    }
    key <- paste(as.character(r$source),as.character(r$source_record_id),sep=":")
    if (is.null(strip_by_key[[key]])) {
      strip_by_key[[key]] <- list(
        source=as.character(r$source),
        source_record_id=as.character(r$source_record_id),
        action="strip_abstract",
        reason=if(is.null(r$reason)) "human_data_quality_repair" else as.character(r$reason),
        original_abstract=NULL,
        supporting_review_case_ids=as.character(r$review_case_id),
        pair_keys=character(),
        decision_source="human_data_quality_repair"
      )
    } else {
      strip_by_key[[key]]$supporting_review_case_ids <- unique(c(
        strip_by_key[[key]]$supporting_review_case_ids,
        as.character(r$review_case_id)
      ))
      strip_by_key[[key]]$decision_source <- "llm_and_or_human_data_quality_repair"
    }
  }
}
strip_actions <- unname(strip_by_key)
strip_path <- file.path(output_dir,"abstract_strip_actions.jsonl")
strip_con <- file(strip_path,"wt",encoding="UTF-8")
if(length(strip_actions)) for(z in strip_actions) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),strip_con,useBytes=TRUE)
close(strip_con)

audit <- vector("list",length(llm))
for (i in seq_along(llm)) {
  a <- llm[[i]]
  key <- as.character(a$pair_key)
  if (!(key %in% pairs$pair_key)) stop(sprintf("Adjudication pair not found in combined state: %s",key),call.=FALSE)

  final_decision <- NULL
  provenance <- NULL
  if (identical(a$promotion,"duplicate") || identical(a$promotion,"not_duplicate")) {
    final_decision <- a$promotion
    provenance <- "llm"
  } else if (identical(a$promotion,"human_review")) {
    h <- human_by_id[[as.character(a$review_case_id)]]
    if (is.null(h)) stop(sprintf("Missing human decision for %s",a$review_case_id),call.=FALSE)
    if (is.null(h$decision) || !(h$decision %in% c("duplicate","not_duplicate"))) {
      stop(sprintf("Human decision for %s is not final",a$review_case_id),call.=FALSE)
    }
    final_decision <- h$decision
    provenance <- "human"
  } else stop(sprintf("Invalid promotion for %s",a$review_case_id),call.=FALSE)

  before <- pairs[.(key),rescored_classification]
  pairs[.(key),`:=`(
    rescored_classification=final_decision,
    rescored_rule=paste0("workflow01_",provenance,"_adjudication"),
    review_route="resolved_adjudication"
  )]
  audit[[i]] <- list(
    review_case_id=a$review_case_id,
    pair_key=key,
    before=before,
    after=final_decision,
    decision_source=provenance,
    model_decision=a$model_decision,
    model_confidence=a$model_confidence,
    model_rationale=a$model_rationale,
    human_decision=if(provenance=="human") human_by_id[[a$review_case_id]]$decision else NULL,
    human_rationale=if(provenance=="human") human_by_id[[a$review_case_id]]$rationale else NULL
  )
}

# Apply explicit, immutable source-identity split corrections before clustering.
# These corrections are narrowly scoped to known historical false merges. They
# prevent cross-group duplicate edges from re-forming those clusters and also
# provide deterministic target work IDs for the authorised split components.
explicit_split_cfg <- NULL
explicit_split_ids <- character()
explicit_key_group <- character()
explicit_key_target <- character()
explicit_key_correction <- character()
explicit_split_audit <- list()
if(!is.null(explicit_splits_path)){
  if(!file.exists(explicit_splits_path)) stop("Explicit cluster split registry not found",call.=FALSE)
  explicit_split_cfg <- fromJSON(explicit_splits_path,simplifyVector=FALSE)
  if(!identical(explicit_split_cfg$schema,"living-evidence-map-workflow01-explicit-cluster-splits-v1")) {
    stop("Unsupported explicit cluster split schema",call.=FALSE)
  }
  corrections <- explicit_split_cfg$corrections
  if(!length(corrections)) stop("Explicit cluster split registry contains no corrections",call.=FALSE)
  explicit_split_ids <- vapply(corrections,function(x)as.character(x$previous_cluster_id),character(1))
  if(anyDuplicated(explicit_split_ids)) stop("Explicit split registry repeats previous_cluster_id",call.=FALSE)

  meta_keys <- paste(meta$source,meta$source_record_id,sep="::")
  meta_key_to_idx <- setNames(meta$idx,meta_keys)
  all_declared_keys <- character()

  for(corr in corrections){
    old_id <- as.character(corr$previous_cluster_id)
    groups_cfg <- corr$groups
    group_names <- vapply(groups_cfg,function(g)as.character(g$group),character(1))
    if(length(groups_cfg)<2L || anyDuplicated(group_names)) stop(sprintf("Invalid explicit groups for %s",old_id),call.=FALSE)
    preserve_group <- as.character(corr$preserve_group)
    if(!(preserve_group %in% group_names)) stop(sprintf("Invalid preserve_group for %s",old_id),call.=FALSE)

    group_keys <- list()
    target_ids <- character()
    for(g in groups_cfg){
      gname <- as.character(g$group)
      keys <- vapply(g$members,function(m)paste(as.character(m$source),as.character(m$source_record_id),sep="::"),character(1))
      if(!length(keys) || anyDuplicated(keys)) stop(sprintf("Invalid members for %s/%s",old_id,gname),call.=FALSE)
      missing_keys <- setdiff(keys,meta_keys)
      if(length(missing_keys)) stop(sprintf("Explicit split %s/%s is missing declared manifestation(s): %s",old_id,gname,paste(missing_keys,collapse=",")),call.=FALSE)
      if(length(intersect(all_declared_keys,keys))) stop("A manifestation occurs in more than one explicit split correction",call.=FALSE)
      all_declared_keys <- c(all_declared_keys,keys)
      group_keys[[gname]] <- keys
      if(gname==preserve_group){
        target_ids[[gname]] <- old_id
      } else {
        digest_keys <- sub("::",":",sort(keys),fixed=TRUE)
        target_ids[[gname]] <- paste0("work-",substr(digest(paste(digest_keys,collapse="|"),algo="sha256",serialize=FALSE),1,16))
      }
      for(key in keys){
        explicit_key_group[[key]] <- paste(old_id,gname,sep="::")
        explicit_key_target[[key]] <- target_ids[[gname]]
        explicit_key_correction[[key]] <- old_id
      }
    }

    declared_idx <- unname(meta_key_to_idx[unlist(group_keys,use.names=FALSE)])
    rows <- which(pairs$record_i %in% declared_idx & pairs$record_j %in% declared_idx)
    overridden <- 0L
    if(length(rows)){
      key_i <- meta_keys[pairs$record_i[rows]]
      key_j <- meta_keys[pairs$record_j[rows]]
      gi <- unname(explicit_key_group[key_i])
      gj <- unname(explicit_key_group[key_j])
      cross <- rows[!is.na(gi) & !is.na(gj) & gi!=gj]
      if(length(cross)){
        overridden <- length(cross)
        pairs[cross,`:=`(
          rescored_classification="not_duplicate",
          rescored_rule="workflow01_explicit_cluster_split",
          review_route="resolved_explicit_correction"
        )]
      }
    }
    explicit_split_audit[[length(explicit_split_audit)+1L]] <- list(
      previous_cluster_id=old_id,
      reason=as.character(corr$reason),
      preserve_group=preserve_group,
      group_count=length(groups_cfg),
      declared_manifestations=length(unlist(group_keys,use.names=FALSE)),
      cross_group_pair_rows_overridden=overridden,
      target_cluster_ids=as.list(target_ids)
    )
  }
}

fwrite(pairs,file.path(output_dir,"final_pair_decisions.csv"))
if(length(explicit_split_audit)){
  con_split <- file(file.path(output_dir,"explicit_cluster_split_audit.jsonl"),"wt",encoding="UTF-8")
  for(z in explicit_split_audit) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),con_split,useBytes=TRUE)
  close(con_split)
}
audit_con <- file(file.path(output_dir,"adjudication_application_audit.jsonl"),"wt",encoding="UTF-8")
for (z in audit) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),audit_con,useBytes=TRUE)
close(audit_con)

parent <- seq_len(nrow(meta))
rank <- integer(nrow(meta))
find_root <- function(x) {
  while(parent[[x]]!=x) {
    parent[[x]] <<- parent[[parent[[x]]]]
    x <- parent[[x]]
  }
  x
}
union_nodes <- function(a,b) {
  ra <- find_root(a); rb <- find_root(b)
  if (ra==rb) return(invisible(NULL))
  if (rank[[ra]]<rank[[rb]]) parent[[ra]] <<- rb
  else if (rank[[ra]]>rank[[rb]]) parent[[rb]] <<- ra
  else {parent[[rb]] <<- ra; rank[[ra]] <<- rank[[ra]]+1L}
}
dup <- pairs[rescored_classification=="duplicate"]
if(nrow(dup)) for(i in seq_len(nrow(dup))) union_nodes(dup$record_i[[i]],dup$record_j[[i]])
roots <- vapply(seq_len(nrow(meta)),find_root,integer(1))
groups <- split(seq_len(nrow(meta)),roots)

previous <- NULL
previous_key_to_cluster <- character()
previous_cluster_first_idx <- integer()
if(!is.null(previous_cluster_map_path)){
  if(!file.exists(previous_cluster_map_path)) stop("Previous cluster map not found",call.=FALSE)
  previous <- fread(previous_cluster_map_path,na.strings=c("","NA"))
  req_prev <- c("source","source_record_id","cluster_id")
  if(length(setdiff(req_prev,names(previous)))) stop("Previous cluster map missing required columns",call.=FALSE)
  previous[,key:=paste(source,source_record_id,sep="::")]
  if(anyDuplicated(previous$key)) stop("Previous cluster map contains duplicate manifestation keys",call.=FALSE)
  current_keys <- paste(meta$source,meta$source_record_id,sep="::")
  missing_prev <- setdiff(previous$key,current_keys)
  if(length(missing_prev)) stop(sprintf("%d previous manifestations are absent from current state",length(missing_prev)),call.=FALSE)
  previous_key_to_cluster <- setNames(as.character(previous$cluster_id),previous$key)

  # Detect a split before assigning any persistent IDs. Each previous work must
  # map wholly to exactly one current union-find component. Checking this after
  # ID preservation is unsafe because two split components could both inherit
  # the same previous cluster_id and mask the split.
  current_key_to_root <- setNames(
    as.character(roots),
    paste(meta$source,meta$source_record_id,sep="::")
  )
  previous[,current_root:=unname(current_key_to_root[key])]
  if(any(is.na(previous$current_root))) {
    stop("Could not map every previous manifestation to a current union-find root",call.=FALSE)
  }
  previous_root_counts <- previous[,.(current_root_count=uniqueN(current_root)),by=cluster_id]
  split_ids_pre <- previous_root_counts[current_root_count>1L,cluster_id]
  unauthorised_split_ids <- setdiff(split_ids_pre,explicit_split_ids)
  if(length(unauthorised_split_ids)){
    stop(sprintf(
      "%d previous work IDs would split across multiple current clusters without an explicit correction: %s",
      length(unauthorised_split_ids),paste(unauthorised_split_ids,collapse=",")
    ),call.=FALSE)
  }
  if(length(intersect(split_ids_pre,explicit_split_ids))){
    authorised <- intersect(split_ids_pre,explicit_split_ids)
    for(old_id in authorised){
      prev_keys <- previous[cluster_id==old_id,key]
      declared <- names(explicit_key_correction)[explicit_key_correction==old_id]
      if(!setequal(prev_keys,declared)){
        stop(sprintf("Authorised split %s no longer matches the exact historical cluster membership",old_id),call.=FALSE)
      }
    }
  }

  if("idx" %in% names(previous)){
    previous_cluster_first_idx <- previous[,.(first_idx=min(idx)),by=cluster_id]
  } else {
    previous_cluster_first_idx <- previous[,.(first_idx=.I[1L]),by=cluster_id]
  }
}

cluster_rows <- vector("list",length(groups))
map_rows <- vector("list",length(groups))
alias_rows <- list()
previous_cluster_targets <- list()
k <- 0L
for(g in groups) {
  k <- k+1L
  keys_colon <- paste(meta$source[g],meta$source_record_id[g],sep=":")
  keys_lookup <- paste(meta$source[g],meta$source_record_id[g],sep="::")
  prior_ids <- if(length(previous_key_to_cluster)) unique(unname(previous_key_to_cluster[keys_lookup])) else character()
  prior_ids <- prior_ids[!is.na(prior_ids) & nzchar(prior_ids)]

  # A component containing manifestations from an explicit split may belong to
  # only one declared correction group. If a future record bridges two groups,
  # fail closed for review rather than silently re-merging the known false split.
  declared_group_labels <- unique(unname(explicit_key_group[keys_lookup]))
  declared_group_labels <- declared_group_labels[!is.na(declared_group_labels) & nzchar(declared_group_labels)]
  if(length(declared_group_labels)>1L){
    stop(sprintf(
      "Current duplicate component bridges explicitly separated W01 groups: %s",
      paste(declared_group_labels,collapse=",")
    ),call.=FALSE)
  }
  explicit_target <- NULL
  if(length(declared_group_labels)==1L){
    declared_keys_in_group <- keys_lookup[keys_lookup %in% names(explicit_key_group)]
    targets <- unique(unname(explicit_key_target[declared_keys_in_group]))
    targets <- targets[!is.na(targets) & nzchar(targets)]
    if(length(targets)!=1L) stop("Could not resolve one deterministic target ID for explicit split component",call.=FALSE)
    explicit_target <- targets[[1L]]
  }

  id_origin <- "new"
  retired_ids <- character()
  if(!is.null(explicit_target)){
    cid <- explicit_target
    if(cid %in% prior_ids){
      id_origin <- "preserved"
      retired_ids <- setdiff(prior_ids,cid)
    } else if(length(prior_ids)==1L && prior_ids[[1L]] %in% explicit_split_ids){
      # First authorised split from the historical false-merged work.
      id_origin <- if(cid %in% explicit_split_ids) "preserved" else "explicit_split"
    } else if(length(prior_ids)==0L){
      id_origin <- "explicit_split"
    } else {
      stop(sprintf(
        "Explicit split component target %s conflicts with prior work ID(s): %s",
        cid,paste(prior_ids,collapse=",")
      ),call.=FALSE)
    }
  } else if(length(prior_ids)==0L){
    cid <- paste0("work-",substr(digest(paste(sort(keys_colon),collapse="|"),algo="sha256",serialize=FALSE),1,16))
  } else if(length(prior_ids)==1L){
    cid <- prior_ids[[1L]]
    id_origin <- "preserved"
  } else {
    cand <- previous_cluster_first_idx[cluster_id %in% prior_ids]
    setorder(cand,first_idx,cluster_id)
    cid <- as.character(cand$cluster_id[[1L]])
    retired_ids <- setdiff(prior_ids,cid)
    id_origin <- "merged_existing"
    for(old_id in retired_ids){
      alias_rows[[length(alias_rows)+1L]] <- data.table(
        retired_cluster_id=old_id,
        surviving_cluster_id=cid,
        reason="duplicate_cluster_merge",
        workflow01_output_cluster_member_count=length(g)
      )
    }
  }

  if(length(prior_ids)){
    for(pid in prior_ids) previous_cluster_targets[[pid]] <- unique(c(previous_cluster_targets[[pid]],cid))
  }

  cluster_rows[[k]] <- list(
    cluster_id=cid,
    cluster_id_origin=id_origin,
    retired_cluster_ids=retired_ids,
    status=if(length(g)>1L)"reconciled" else "singleton",
    member_count=length(g),
    members=lapply(g,function(i)list(idx=meta$idx[[i]],source=meta$source[[i]],source_record_id=meta$source_record_id[[i]]))
  )
  map_rows[[k]] <- data.table(
    idx=g,source=meta$source[g],source_record_id=meta$source_record_id[g],
    cluster_id=cid,cluster_size=length(g),cluster_id_origin=id_origin
  )
}
map <- rbindlist(map_rows)
aliases <- if(length(alias_rows)) unique(rbindlist(alias_rows,use.names=TRUE,fill=TRUE)) else
  data.table(retired_cluster_id=character(),surviving_cluster_id=character(),reason=character(),workflow01_output_cluster_member_count=integer())
fwrite(aliases,file.path(output_dir,"cluster_id_aliases.csv"))
setorder(map,idx)
fwrite(map,file.path(output_dir,"manifestation_cluster_map.csv"))
con <- file(file.path(output_dir,"clusters.jsonl"),"wt",encoding="UTF-8")
for(z in cluster_rows) writeLines(toJSON(z,auto_unbox=TRUE,null="null"),con,useBytes=TRUE)
close(con)

remaining <- pairs[
  rescored_classification=="review" | review_route=="manual_review" | review_route=="workflow04_exclusion_candidate"
]
sizes <- map[,.(cluster_size=.N),by=cluster_id]
summary <- list(
  workflow="01_final_adjudicated_deduplication",
  status=if(nrow(remaining)==0L)"final" else "incomplete",
  source_manifestations=nrow(meta),
  total_pair_decisions=nrow(pairs),
  adjudicated_cases=length(llm),
  llm_final_decisions=sum(vapply(audit,function(x)identical(x$decision_source,"llm"),logical(1))),
  human_final_decisions=sum(vapply(audit,function(x)identical(x$decision_source,"human"),logical(1))),
  abstract_strip_actions=length(strip_actions),
  abstract_strip_actions_file="abstract_strip_actions.jsonl",
  workflow02_discovers_stripped_abstracts_via_normal_missing_abstract_scan=TRUE,
  unresolved_pair_decisions=nrow(remaining),
  automatic_duplicate_edges=nrow(dup),
  clusters=nrow(sizes),
  duplicate_clusters=sum(sizes$cluster_size>1L),
  singleton_clusters=sum(sizes$cluster_size==1L),
  manifestations_in_duplicate_clusters=sum(sizes$cluster_size[sizes$cluster_size>1L]),
  previous_cluster_map_supplied=!is.null(previous_cluster_map_path),
  preserved_work_ids=if(is.null(previous)) 0L else sum(unique(map[,.(cluster_id,cluster_id_origin)])$cluster_id_origin=="preserved"),
  merged_existing_work_ids=nrow(aliases),
  new_work_ids=sum(unique(map[,.(cluster_id,cluster_id_origin)])$cluster_id_origin %in% c("new","explicit_split")),
  explicit_cluster_split_corrections=length(explicit_split_audit),
  explicit_cluster_split_registry=if(is.null(explicit_splits_path)) NULL else basename(explicit_splits_path),
  explicit_cluster_split_audit_file=if(length(explicit_split_audit)) "explicit_cluster_split_audit.jsonl" else NULL,
  cluster_id_aliases_file="cluster_id_aliases.csv"
)
if (nrow(remaining)) {
  fwrite(remaining,file.path(output_dir,"unresolved_pairs.csv"))
  stop(sprintf("Finalisation blocked: %d unresolved pair decisions remain",nrow(remaining)),call.=FALSE)
}
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"summary.json"))
cat(sprintf("PASS: Workflow 01 finalised after adjudication: %d manifestations, %d clusters, %d adjudicated cases\n",
            nrow(meta),nrow(sizes),length(llm)))
