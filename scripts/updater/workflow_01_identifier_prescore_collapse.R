#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(stringdist)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

candidate_path <- arg("--candidates")
metadata_path <- arg("--metadata")
safe_path <- arg("--safe-edges")
output_dir <- arg("--output-dir")
if (any(vapply(list(candidate_path,metadata_path,safe_path,output_dir),is.null,logical(1)))) {
  stop("Required: --candidates --metadata --safe-edges --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

meta <- fread(metadata_path,na.strings=c("","NA"))
need_meta <- c("idx","source","source_record_id","title","title_norm","doi_norm","doi_family",
               "abstract_hash","author_norm","year","journal_norm","volume_norm","issue_norm","pages_norm")
if (length(setdiff(need_meta,names(meta)))) stop("Metadata missing required pre-score fields",call.=FALSE)
if (!identical(as.integer(meta$idx),seq_len(nrow(meta)))) stop("Metadata idx is not contiguous",call.=FALSE)
setkey(meta,idx)

cand <- fread(candidate_path,na.strings=c("","NA"))
if (!all(c("record_i","record_j","blocks") %in% names(cand))) stop("Candidate file missing record_i/record_j/blocks",call.=FALSE)
cand[,pair_key:=paste(pmin(record_i,record_j),pmax(record_i,record_j),sep="::")]
cand <- unique(cand,by="pair_key")
input_candidates <- nrow(cand)

safe <- fread(safe_path,na.strings=c("","NA"))
if (!all(c("idx_i","idx_j") %in% names(safe))) stop("Safe edge file missing idx_i/idx_j",call.=FALSE)
safe[,pair_key:=paste(pmin(idx_i,idx_j),pmax(idx_i,idx_j),sep="::")]
safe <- unique(safe,by="pair_key")

# Provisional components contain only validated safe identifier edges.
parent <- seq_len(nrow(meta))
find_root <- function(x) {
  while (parent[[x]] != x) {
    parent[[x]] <<- parent[[parent[[x]]]]
    x <- parent[[x]]
  }
  x
}
union_pair <- function(a,b) {
  ra <- find_root(a); rb <- find_root(b)
  if (ra==rb) return(invisible(NULL))
  if (ra<rb) parent[[rb]] <<- ra else parent[[ra]] <<- rb
}
if (nrow(safe)) for (i in seq_len(nrow(safe))) union_pair(safe$idx_i[[i]],safe$idx_j[[i]])
component <- vapply(seq_len(nrow(meta)),find_root,integer(1))
meta[,component:=component]
comp_map <- setNames(meta$component,meta$idx)

cand[,component_i:=as.integer(comp_map[as.character(record_i)])]
cand[,component_j:=as.integer(comp_map[as.character(record_j)])]
if (anyNA(cand$component_i)||anyNA(cand$component_j)) stop("Candidate failed component mapping",call.=FALSE)
cand[,internal_preresolved:=component_i==component_j]
internal_eliminated <- sum(cand$internal_preresolved)

external <- cand[internal_preresolved==FALSE]
external[,component_key:=paste(pmin(component_i,component_j),pmax(component_i,component_j),sep="::")]

has_block <- function(blocks,name) grepl(paste0("(^|;)",name,"(;|$)"),blocks,perl=TRUE)
first_author <- function(x) {
  if (is.na(x)||!nzchar(x)) return(NA_character_)
  strsplit(x,"|",fixed=TRUE)[[1L]][[1L]]
}
title_containment <- function(a,b) {
  if (is.na(a)||is.na(b)||!nzchar(a)||!nzchar(b)) return(FALSE)
  short <- if(nchar(a)<=nchar(b)) a else b
  long <- if(nchar(a)<=nchar(b)) b else a
  nchar(short)>=30L && grepl(short,long,fixed=TRUE)
}

# Calculate only inexpensive features available from normalised metadata.
ai <- meta[external$record_i]
bj <- meta[external$record_j]
external[,exact_same_doi:=!is.na(ai$doi_norm)&nzchar(ai$doi_norm)&
                           !is.na(bj$doi_norm)&nzchar(bj$doi_norm)&
                           ai$doi_norm==bj$doi_norm]
external[,exact_title:=!is.na(ai$title_norm)&!is.na(bj$title_norm)&ai$title_norm==bj$title_norm]
external[,exact_abstract:=!is.na(ai$abstract_hash)&!is.na(bj$abstract_hash)&ai$abstract_hash==bj$abstract_hash]
external[,title_similarity:=mapply(function(a,b) {
  if (is.na(a)||is.na(b)||!nzchar(a)||!nzchar(b)) return(NA_real_)
  as.numeric(stringsim(a,b,method="jw",p=0.1))
},ai$title_norm,bj$title_norm)]
external[,title_containment:=mapply(title_containment,ai$title_norm,bj$title_norm)]
fa_i <- vapply(ai$author_norm,first_author,character(1))
fa_j <- vapply(bj$author_norm,first_author,character(1))
external[,first_author_match:=!is.na(fa_i)&!is.na(fa_j)&fa_i==fa_j]
external[,year_diff:=fifelse(!is.na(ai$year)&!is.na(bj$year),abs(ai$year-bj$year),NA_integer_)]

# Frozen selector validated against the 503,245-pair W01 update. Abstract LCS,
# shingle containment and existing classifications are deliberately excluded.
external[,cheap_score :=
  120*has_block(blocks,"bramer_A") +
  110*has_block(blocks,"bramer_B") +
  100*exact_same_doi +
   90*(exact_title %in% TRUE) +
   80*(exact_abstract %in% TRUE) +
   40*(title_containment %in% TRUE) +
   50*fifelse(is.na(title_similarity),0,pmax(0,title_similarity)) +
   20*(first_author_match %in% TRUE) +
   15*(!is.na(year_diff)&year_diff<=1L) +
   10*has_block(blocks,"doi_family") +
   10*has_block(blocks,"abstract_hash")
]

setorder(external,component_key,-cheap_score,-title_similarity,record_i,record_j,na.last=TRUE)
selected <- external[, .SD[1L], by=component_key]
if (anyDuplicated(selected$component_key)) stop("Component relation selection is not unique",call.=FALSE)

out_pairs <- selected[,.(record_i,record_j,blocks)]
fwrite(out_pairs,file.path(output_dir,"all_candidate_pairs.csv"))
fwrite(meta[, !"component"],file.path(output_dir,"normalised_metadata.csv"))
fwrite(selected,file.path(output_dir,"representative_pair_selection.csv"))
fwrite(meta[,.(idx,source,source_record_id,component)],file.path(output_dir,"identifier_component_membership.csv"))

audit <- list(
  schema="living-evidence-map-workflow01-identifier-prescore-collapse-v1",
  status="success",
  manifestation_candidate_pairs=input_candidates,
  safe_identifier_edges=nrow(safe),
  provisional_components=uniqueN(meta$component),
  internal_candidate_pairs_eliminated=internal_eliminated,
  external_component_relations=uniqueN(external$component_key),
  pairs_selected_for_expensive_scoring=nrow(out_pairs),
  scoring_decisions_avoided=input_candidates-nrow(out_pairs),
  scoring_decision_reduction_percent=if(input_candidates) 100*(input_candidates-nrow(out_pairs))/input_candidates else 0,
  selector=list(
    expensive_abstract_lcs=FALSE,
    shingle_containment=FALSE,
    existing_classification=FALSE,
    features=c("blocking evidence","exact DOI","exact title","exact abstract hash",
               "title containment","Jaro-Winkler title similarity","first-author match","year compatibility")
  )
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"identifier_prescore_summary.json"))
cat(sprintf("PASS: collapsed %d W01 candidate pairs to %d component relations (%.2f%% fewer scoring decisions)\n",
            input_candidates,nrow(out_pairs),audit$scoring_decision_reduction_percent))
