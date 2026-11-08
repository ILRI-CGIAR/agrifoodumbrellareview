#!/usr/bin/env Rscript
# Read-only ILRI baseline and source provenance preflight, safe for fresh R sessions.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k, default=NULL) {
  i <- match(k,args)
  if(is.na(i)) return(default)
  if(i == length(args)) stop("Missing value after ",k,call.=FALSE)
  args[[i+1L]]
}
root <- arg("--repo-root",".")
file_at <- function(path) file.path(root,path)
read_json <- function(path) {
  f <- file_at(path)
  if(!file.exists(f)) stop("Missing project baseline file: ",path,call.=FALSE)
  fromJSON(f,simplifyVector=FALSE)
}
expect <- function(ok,what) if(!isTRUE(ok))stop("ILRI PREFLIGHT FAIL: ",what,call.=FALSE)
sval <- function(x) if(is.null(x)||!length(x)) "" else as.character(x[[1L]])
num <- function(x) as.integer(x[[1L]])
sources <- c(scopus=10919L,lens=11241L,openalex=9105L,agricola=192L,
             wos=9338L,cab=4951L,proquest=528L,lilacs=87L)
manifest <- read_json("docs/workflow00/ilri_validated_artifacts_2026-11-07.json")
status <- read_json("docs/current_run/current_run_status.json")
w01 <- read_json("docs/deduplication/zenodo/run-42401289007.json")
w02 <- read_json("docs/enrichment/zenodo/run-42418084454.json")
lean <- read_json("docs/compaction/zenodo/run-42418084454.json")
w03 <- read_json("docs/publication_status/zenodo/run-42418518879.json")
expect(identical(manifest$status,"validated"),"search input must be validated")
expect(identical(manifest$branch,"migration/w00-w03"),"search provenance branch")
expect(setequal(names(status$search$sources),names(sources)),"source roster")
expect(num(manifest$expected_total_records)==sum(sources),"manifest total source records")
expect(num(w01$source_manifestations)==sum(sources),"W01 source manifestations")
for(s in names(sources)) {
  actual <- num(status$search$sources[[s]]$successfully_downloaded_results)
  expect(actual==sources[[s]],paste("source count",s,actual,"vs",sources[[s]]))
}
expect(num(w01$canonical_records)==21054L,"canonical count")
for(z in list(w01,w02,lean,w03))
  expect(identical(z$status,"published")&&identical(z$visibility,"restricted"),
         "all downstream pointers must be published and restricted")
expect(identical(sval(w02$upstream_workflow01_canonical_sha256),sval(w01$canonical_jsonl_sha256)),
       "W01 to W02 SHA handoff")
expect(identical(sval(lean$input_canonical_sha256),sval(w02$final_enriched_sha256)),
       "W02 to lean SHA handoff")
expect(identical(sval(w03$upstream_lean_canonical_sha256),sval(lean$lean_canonical_sha256)),
       "lean to W03 SHA handoff")
expect(num(lean$canonical_records)==21054L&&num(w03$records)==21054L,"W03 record count")
expect(num(w03$status_counts$retracted)==25L&&num(w03$exclude_from_workflow04)==25L,
       "W03 retraction count")
expect(sum(vapply(w03$status_counts,num,integer(1)))==21054L,"W03 classification totals")
expect(21054L-num(w03$exclude_from_workflow04)==21029L,"retained records count")
expect(identical(status$workflow03_pointer_path,
  "docs/publication_status/zenodo/run-42418518879.json"),"current-run W03 pointer")
ledger <- read_json("docs/workflow00/ilri_search_provenance_ledger.json")
expect(identical(ledger$schema,"ilri-search-provenance-ledger-v1"),"search provenance ledger schema")
entries <- ledger$sources
codes <- vapply(entries,function(z)sval(z$code),character(1))
expect(length(codes)==8L&&!anyDuplicated(codes)&&setequal(codes,names(sources)),
       "search provenance roster must contain exactly the eight ILRI sources")
for(e in entries){
  code <- sval(e$code)
  expect(num(e$retrieved_records)==sources[[code]],paste("search ledger count",code))
  if(nzchar(sval(e$search_string_path)))
    expect(file.exists(file_at(e$search_string_path)),paste("query file missing for",code))
  if(sval(e$zenodo_publication_status)=="reserved_draft_not_verified")
    expect(nzchar(sval(e$zenodo_record_doi)),paste("draft DOI absent for",code))
}
expect(num(ledger$total_source_records)==sum(sources),"search provenance total")
report_ready <- all(vapply(entries,function(z){
  nzchar(sval(z$search_string_path)) && isTRUE(z$search_fields_verified) &&
    length(z$searched_fields)>0L && !is.null(z$provider_reported_hits) &&
    nzchar(sval(z$search_executed_at_utc)) && nzchar(sval(z$exported_at_utc)) &&
    identical(sval(z$zenodo_publication_status),"published_verified")
},logical(1)))
cat(sprintf("SEARCH_REPORT_READY=%s\n",if(report_ready)"true" else "false"))
if(identical(tolower(arg("--require-report-ready","false")),"true"))
  expect(report_ready,"verbatim queries, dates, hit counts, search-field verification and published source archives are incomplete")

cat(sprintf("PASS: 8 source counts sum to %d, %d canonical, %d excluded, %d retained. All published handoff checksums agree.\n",
  sum(sources),num(w01$canonical_records),num(w03$exclude_from_workflow04),
  num(w01$canonical_records)-num(w03$exclude_from_workflow04)))
# This guard never asserts readiness for future searches. Verbatim queries and true
# export dates need their own verified source-level search receipt.
for(s in c("cab","proquest","lilacs")) {
  p <- file_at(sprintf("user_input/workflow00_ris_%s_ilri_2026.json",s))
  if(file.exists(p)) {
    z <- fromJSON(p,simplifyVector=FALSE)
    if(grepl("PLACEHOLDER",sval(z$search$notes),fixed=TRUE))
      cat(sprintf("NOTICE: %s original search/export date remains unverified.\n",s))
  }
}
