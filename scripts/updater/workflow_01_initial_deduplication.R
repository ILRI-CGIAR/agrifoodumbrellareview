#!/usr/bin/env Rscript
# First-ever W01 candidate preparation using the existing W01 algorithms.
# This does not classify, cluster, publish, or imply that W01 has completed.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(n,default=NULL) {
 i <- match(n,args);if(is.na(i))return(default)
 if(i==length(args))stop("Missing argument after ",n,call.=FALSE)
 args[[i+1L]]
}
manifest <- arg("--input-manifest")
output <- arg("--output-dir")
if(is.null(manifest)||is.null(output))stop("Required: --input-manifest --output-dir",call.=FALSE)
if(!file.exists(manifest))stop("Source manifest not found",call.=FALSE)
x<-fromJSON(manifest,simplifyVector=FALSE)
if(!identical(x$schema,"living-evidence-map-workflow01-source-inputs-v1") ||
 is.null(x$sources)||!length(x$sources))stop("Invalid W01 source manifest",call.=FALSE)
if(!is.null(arg("--previous-pointer")))stop("First-run candidate preparation forbids historical W01 pointer",call.=FALSE)
dir.create(output,recursive=TRUE,showWarnings=FALSE)
empty <- file.path(output,"empty_prior_state")
if(dir.exists(empty) && length(list.files(empty,all.files=TRUE,no..=TRUE)))
 stop("Refusing nonempty first-run prior-state directory",call.=FALSE)
dir.create(empty,showWarnings=FALSE)
union <- file.path(output,"union")
candidates <- file.path(output,"candidates")
run_r <- function(script,argv) {
 result <- system2(file.path(R.home("bin"),"Rscript"),c(shQuote(script),vapply(argv,shQuote,character(1))),stdout="",stderr="")
 if(!identical(as.integer(result),0L))stop("First-run stage failed: ",script,call.=FALSE)
}
run_r("scripts/updater/workflow_01_deduplication_build_union.R",
 c("--first-run","true","--old-dir",empty,"--input-manifest",manifest,
   "--expected-prior-manifestations","0","--output-dir",union))
run_r("scripts/updater/workflow_01_deduplication_incremental_candidates.R",
 c("--first-run","true","--input-manifest",file.path(union,"source_inputs.json"),
   "--output-dir",candidates,"--score-n","1"))
cat("PASS: first-run union and candidates prepared with explicit immutable source IDs.\n")
cat("NEXT: calibrated classification, human adjudication, clustering and full Zenodo baseline publication are separately required.\n")
