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
first_run <- identical(tolower(arg("--first-run","false")),"true")
old_decisions <- arg("--old-decisions")
new_decisions <- arg("--new-decisions")
metadata <- arg("--metadata")
output_dir <- arg("--output-dir")
if (any(vapply(list(old_decisions,new_decisions,metadata,output_dir),is.null,logical(1)))) {
  stop("Required: --old-decisions --new-decisions --metadata --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

new <- fread(new_decisions,na.strings=c("","NA"))
old <- if (first_run) copy(new[0]) else fread(old_decisions,na.strings=c("","NA"))
meta <- fread(metadata,na.strings=c("","NA"))
if (first_run && file.exists(old_decisions) && isTRUE(file.info(old_decisions)$size > 0L)) stop("First run must not supply historical decisions",call.=FALSE)
stopifnot(all(c("idx","source","source_record_id") %in% names(meta)))
stopifnot(identical(meta$idx,seq_len(nrow(meta))))

# Verify persisted identities before accepting positional candidate coordinates.
# Rows may be reordered between a paused classification and its resumption.
verify_pair_ids <- function(x,label) {
  fields <- c("source_id_i","source_record_id_i","source_id_j","source_record_id_j")
  present <- fields %in% names(x)
  if(!nrow(x)) return(invisible(TRUE))
  if(!any(present)) stop(label,": persisted decisions lack immutable source IDs",call.=FALSE)
  if(!all(present)) stop(label,": partial immutable identity columns",call.=FALSE)
  i<-as.integer(x$record_i); j<-as.integer(x$record_j)
  if(anyNA(i)||anyNA(j)||any(i<1L|i>nrow(meta)|j<1L|j>nrow(meta)|i==j))
    stop(label,": invalid positional coordinates",call.=FALSE)
  valid <- as.character(x$source_id_i)==as.character(meta$source[i]) &
    as.character(x$source_record_id_i)==as.character(meta$source_record_id[i]) &
    as.character(x$source_id_j)==as.character(meta$source[j]) &
    as.character(x$source_record_id_j)==as.character(meta$source_record_id[j])
  if(anyNA(valid)||!all(valid))
    stop(label,": immutable source identity mismatch; refusing positional decision assignment",call.=FALSE)
  invisible(TRUE)
}
verify_pair_ids(old,"Preserved decisions")
verify_pair_ids(new,"New decisions")
# A persisted pair key is based on the immutable source namespace and source ID.
# Numeric indices are used only to address the current in-memory metadata.
pair_key <- function(x) {
  if(!nrow(x)) return(character())
  i<-as.integer(x$record_i);j<-as.integer(x$record_j)
  if(anyNA(i)||anyNA(j)||any(i<1L|i>nrow(meta)|j<1L|j>nrow(meta)|i==j))
    stop("Invalid positional pair coordinates",call.=FALSE)
  identity<-function(src,id)paste0(nchar(src,type="bytes"),":",src,nchar(id,type="bytes"),":",id)
  a<-identity(as.character(meta$source[i]),as.character(meta$source_record_id[i]))
  b<-identity(as.character(meta$source[j]),as.character(meta$source_record_id[j]))
  low<-ifelse(a<=b,a,b);high<-ifelse(a<=b,b,a)
  vapply(seq_along(low),function(k)
    digest(paste0(nchar(low[[k]],type="bytes"),":",low[[k]],high[[k]]),
      algo="sha256",serialize=FALSE),character(1))
}
old[,pair_key:=pair_key(old)]
new[,pair_key:=pair_key(new)]
if (anyDuplicated(old$pair_key)) stop("Duplicate preserved pair key",call.=FALSE)

incremental_candidate_rows <- nrow(new)
duplicate_keys <- unique(new$pair_key[duplicated(new$pair_key) | duplicated(new$pair_key, fromLast=TRUE)])
duplicate_candidate_rows_removed <- 0L
if (length(duplicate_keys)) {
  audit <- new[pair_key %in% duplicate_keys]
  compare_cols <- setdiff(names(audit), c("source_row_index","pair_key"))
  distinct_rows <- unique(audit[, c("pair_key", compare_cols), with=FALSE])
  discordant <- distinct_rows[, .N, by=pair_key][N > 1L]
  if (nrow(discordant)) {
    stop(sprintf("Found %d discordant duplicate incremental pair keys", nrow(discordant)), call.=FALSE)
  }
  fwrite(audit, file.path(output_dir,"incremental_duplicate_pair_audit.csv"))
  before <- nrow(new)
  setorder(new, pair_key, source_row_index)
  new <- new[!duplicated(pair_key)]
  duplicate_candidate_rows_removed <- before - nrow(new)
  cat(sprintf(
    "PASS: collapsed %d exact duplicate candidate rows across %d pair keys; %d unique incremental pairs remain\n",
    duplicate_candidate_rows_removed, length(duplicate_keys), nrow(new)
  ))
}

if (anyDuplicated(new$pair_key)) stop("Duplicate incremental pair key remains after exact-duplicate collapse",call.=FALSE)
overlap <- intersect(old$pair_key,new$pair_key)
if (length(overlap)) stop(sprintf("Incremental scorer recomputed %d preserved pairs",length(overlap)),call.=FALSE)

all <- rbindlist(list(old,new),use.names=TRUE,fill=TRUE)
if (anyDuplicated(all$pair_key)) stop("Combined pair decisions are not unique",call.=FALSE)
fwrite(all,file.path(output_dir,"combined_pair_decisions.csv"))

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

dup <- all[rescored_classification=="duplicate"]
if (nrow(dup)) for(i in seq_len(nrow(dup))) union_nodes(dup$record_i[[i]],dup$record_j[[i]])
roots <- vapply(seq_len(nrow(meta)),find_root,integer(1))
groups <- split(seq_len(nrow(meta)),roots)

cluster_rows <- vector("list",length(groups))
map_rows <- vector("list",length(groups))
k <- 0L
for(g in groups) {
  k <- k+1L
  keys <- paste(meta$source[g],meta$source_record_id[g],sep=":")
  cid <- paste0("work-",substr(digest(paste(sort(keys),collapse="|"),algo="sha256",serialize=FALSE),1,16))
  cluster_rows[[k]] <- list(
    cluster_id=cid,
    status=if(length(g)>1L) "reconciled" else "singleton",
    member_count=length(g),
    members=lapply(g,function(i) list(
      idx=meta$idx[[i]],source=meta$source[[i]],source_record_id=meta$source_record_id[[i]]
    ))
  )
  map_rows[[k]] <- data.table(
    idx=g,source=meta$source[g],source_record_id=meta$source_record_id[g],
    cluster_id=cid,cluster_size=length(g)
  )
}
map <- rbindlist(map_rows)
setorder(map,idx)
fwrite(map,file.path(output_dir,"manifestation_cluster_map.csv"))
con <- file(file.path(output_dir,"clusters.jsonl"),"wt",encoding="UTF-8")
for(z in cluster_rows) writeLines(toJSON(z,auto_unbox=TRUE,null="null"),con)
close(con)

review <- all[review_route=="manual_review"]
fwrite(review,file.path(output_dir,"manual_review_pairs.csv"))
exclude <- all[review_route=="workflow04_exclusion_candidate"]
fwrite(exclude,file.path(output_dir,"workflow04_exclusion_candidates.csv"))

sizes <- map[,.(cluster_size=.N),by=cluster_id]
summary <- list(
  workflow="01_deduplication_full_five_source_extension",
  status="success",
  source_manifestations=nrow(meta),
  preserved_pair_decisions=nrow(old),
  incremental_candidate_rows=incremental_candidate_rows,
  incremental_pair_decisions=nrow(new),
  exact_duplicate_candidate_rows_removed=duplicate_candidate_rows_removed,
  exact_duplicate_pair_keys=length(duplicate_keys),
  total_candidate_pair_decisions=nrow(all),
  automatic_duplicate_edges=nrow(dup),
  manual_review_pairs=nrow(review),
  workflow04_exclusion_candidate_pairs=nrow(exclude),
  clusters=nrow(sizes),
  duplicate_clusters=sum(sizes$cluster_size>1L),
  singleton_clusters=sum(sizes$cluster_size==1L),
  manifestations_in_duplicate_clusters=sum(sizes$cluster_size[sizes$cluster_size>1L])
)
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE),file.path(output_dir,"summary.json"))
cat(sprintf("PASS: clustered %d manifestations from %d preserved + %d incremental pair decisions; manual review=%d\n",
            nrow(meta),nrow(old),nrow(new),nrow(review)))
