#!/usr/bin/env Rscript
# Read-only, fail-closed review initialization contract.
args <- commandArgs(trailingOnly=TRUE)
arg <- function(n, default="") {
 i <- match(n,args)
 if(is.na(i)) return(default)
 if(i==length(args)) stop("Missing value after ",n,call.=FALSE)
 args[[i+1L]]
}
raw <- tolower(trimws(arg("--first-run","false")))
if(!raw %in% c("true","false")) stop("first_run must be explicitly true or false",call.=FALSE)
first <- identical(raw,"true")
stage <- toupper(arg("--stage","W00"))
if(!stage %in% sprintf("W%02d",0:8)) stop("Invalid stage",call.=FALSE)
prior <- trimws(arg("--previous-pointer",""))
if(tolower(prior) %in% c("none","null","na")) prior <- ""
registry <- "docs/workflow08/zenodo_registry.csv"
baseline <- if(file.exists(registry)) {
 x <- read.csv(registry,stringsAsFactors=FALSE,check.names=FALSE)
 any(as.character(x$status)=="authoritative")
} else FALSE
if(stage=="W00") {
 if(first && baseline) stop("first_run=TRUE but an existing authoritative W08 baseline is present",call.=FALSE)
 if(!first && !baseline) stop("first_run=FALSE but no authoritative W08 baseline is present",call.=FALSE)
}
if(stage!="W00") {
  status_path <- "docs/current_run/current_run_status.json"
  if(!file.exists(status_path)) stop(stage,": current-run status is missing",call.=FALSE)
  if(!requireNamespace("jsonlite",quietly=TRUE)) stop("jsonlite is required for run-provenance validation",call.=FALSE)
  run <- jsonlite::fromJSON(status_path,simplifyVector=FALSE)
  recorded <- run$first_run
  if(is.null(recorded) || length(recorded)!=1L || !is.logical(recorded)) {
    stop(stage,": current-run status has no valid first_run flag",call.=FALSE)
  }
  if(!identical(recorded,first)) {
    stop(stage,": first_run disagrees with immutable W00 run provenance",call.=FALSE)
  }
  if(!identical(as.character(run$branch),"main")) stop(stage,": unexpected execution branch",call.=FALSE)
}
if(stage %in% c("W01","W02")) {
 if(first && nzchar(prior)) stop(stage,": first_run=TRUE forbids a previous-review pointer",call.=FALSE)
 if(!first && !nzchar(prior)) stop(stage,": first_run=FALSE requires a previous-review pointer",call.=FALSE)
 if(!first && !file.exists(prior)) stop(stage,": previous-review pointer does not exist: ",prior,call.=FALSE)
}
cat(sprintf("PASS: %s first_run=%s previous_state=%s\n",stage,toupper(raw),if(nzchar(prior))"provided" else "absent"))
