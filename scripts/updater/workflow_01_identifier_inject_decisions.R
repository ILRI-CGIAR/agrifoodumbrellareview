#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

rescored_path <- arg("--rescored")
safe_path <- arg("--safe-edges")
output_path <- arg("--output")
audit_path <- arg("--audit")
if (any(vapply(list(rescored_path,safe_path,output_path,audit_path),is.null,logical(1)))) {
  stop("Required: --rescored --safe-edges --output --audit",call.=FALSE)
}

x <- fread(rescored_path,na.strings=c("","NA"))
safe <- fread(safe_path,na.strings=c("","NA"))
if (!all(c("record_i","record_j","rescored_classification","rescored_rule") %in% names(x))) {
  stop("Rescored input missing required fields",call.=FALSE)
}
if (!all(c("idx_i","idx_j") %in% names(safe))) stop("Safe edge input missing idx_i/idx_j",call.=FALSE)

pair_key <- function(a,b) paste(pmin(a,b),pmax(a,b),sep="::")
x[,pair_key:=pair_key(record_i,record_j)]
raw_rows <- nrow(x)
if ("source_row_index" %in% names(x)) setorder(x,pair_key,source_row_index)
x <- unique(x,by="pair_key")

safe[,pair_key:=pair_key(idx_i,idx_j)]
safe <- unique(safe,by="pair_key")
overlap <- intersect(x$pair_key,safe$pair_key)
if (length(overlap)) stop(sprintf("%d safe identifier edges overlap scored representative pairs",length(overlap)),call.=FALSE)

safe_rows <- data.table(
  record_i=as.integer(safe$idx_i),
  record_j=as.integer(safe$idx_j),
  classification="duplicate",
  rule="identifier_safe_preresolve",
  rescored_classification="duplicate",
  rescored_rule="identifier_safe_preresolve",
  review_route="resolved_identifier_assist",
  decision_changed=FALSE,
  identifier_assist=TRUE,
  identifier_families=safe$families,
  identifier_namespaces=safe$namespaces,
  identifier_shared_values=safe$shared_identifiers,
  identifier_title_similarity=safe$title_similarity,
  identifier_year_diff=safe$year_diff,
  pair_key=safe$pair_key
)
x[,identifier_assist:=FALSE]
combined <- rbindlist(list(x,safe_rows),use.names=TRUE,fill=TRUE)
if (anyDuplicated(combined$pair_key)) stop("Combined incremental decisions contain duplicate pair keys",call.=FALSE)
setorder(combined,record_i,record_j)
fwrite(combined[,!"pair_key"],output_path)

audit <- list(
  schema="living-evidence-map-workflow01-identifier-decision-injection-v1",
  status="success",
  rescored_rows_raw=raw_rows,
  rescored_unique_pairs=nrow(x),
  safe_identifier_duplicate_edges=nrow(safe_rows),
  pair_key_overlap=length(overlap),
  combined_incremental_decisions=nrow(combined),
  automatic_identifier_duplicates=nrow(safe_rows)
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),audit_path)
cat(sprintf("PASS: combined %d scored representative decisions with %d safe identifier duplicate edges\n",
            nrow(x),nrow(safe_rows)))
