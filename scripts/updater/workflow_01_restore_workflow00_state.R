#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

state_path <- arg("--state")
output_root <- arg("--output-root")
if (is.null(state_path) || is.null(output_root)) stop("Required: --state --output-root",call.=FALSE)
if (!file.exists(state_path)) stop("Workflow 00 state file not found",call.=FALSE)

validator <- "scripts/updater/workflow_00_validate_state.R"
legacy_restore <- "scripts/updater/workflow_01_restore_workflow00_from_zenodo.R"
if (!file.exists(validator) || !file.exists(legacy_restore)) stop("Required Workflow 00 restore utilities are missing",call.=FALSE)

rscript <- file.path(R.home("bin"),"Rscript")
status <- system2(rscript,c(validator,"--state",state_path))
if (status != 0L) stop("Workflow 00 state validation failed",call.=FALSE)

x <- fromJSON(state_path,simplifyVector=FALSE)
expected_sources <- names(x$sources)
if (is.null(expected_sources) || !length(expected_sources)) stop("Workflow 00 state contains no sources",call.=FALSE)
if (any(!nzchar(expected_sources)) || any(!grepl("^[a-z0-9][a-z0-9_-]*$",expected_sources))) {
  stop("Workflow 00 state contains an invalid source identifier",call.=FALSE)
}
if (anyDuplicated(expected_sources)) stop("Workflow 00 state contains duplicate source identifiers",call.=FALSE)
dir.create(output_root,recursive=TRUE,showWarnings=FALSE)

pointers <- vapply(expected_sources,function(src)as.character(x$sources[[src]]$archive_pointer),character(1))
for (pointer in unique(pointers)) {
  srcs <- expected_sources[pointers==pointer]
  source_arg <- paste(srcs,collapse=",")
  message(sprintf("Restoring %s from %s",source_arg,pointer))
  status <- system2(
    rscript,
    c(legacy_restore,"--pointer",pointer,"--sources",source_arg,"--output-root",output_root)
  )
  if (status != 0L) stop(sprintf("Workflow 00 archive restoration failed for %s",source_arg),call.=FALSE)
}

missing <- expected_sources[!dir.exists(file.path(output_root,expected_sources))]
if (length(missing)) stop(sprintf("Restored Workflow 00 state missing source directories: %s",paste(missing,collapse=", ")),call.=FALSE)

audit <- list(
  schema="living-evidence-map-workflow00-state-restore-audit-v1",
  status="success",
  state_id=x$state_id,
  search_version=x$search_version,
  sources=expected_sources,
  archive_pointers=unique(pointers),
  restored_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_root,"workflow00_state_restore_audit.json"))
cat(sprintf("PASS: restored complete Workflow 00 state %s with %d source(s): %s\n",
            x$state_id,length(expected_sources),paste(expected_sources,collapse=", ")))
