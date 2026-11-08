#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
config="tests/fixtures/workflow00_search_strategy.json"
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type full --config "$config" --output-dir "$tmp/full"
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type monthly --config "$config" --output-dir "$tmp/monthly"
W00_SELECT_SCOPUS=true W00_SELECT_WOS=true \
Rscript scripts/updater/workflow_00_resolve_source_selection.R \
  --run-type full --output "$tmp/selection.json"
Rscript scripts/updater/workflow_00_resolve_source_selection.R \
  --run-type monthly --saved "$tmp/selection.json" \
  --output "$tmp/monthly_selection.json"
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type monthly --config "$config" \
  --source-selection "$tmp/monthly_selection.json" --output-dir "$tmp/selected"
Rscript scripts/updater/write_workflow00_search_record.R \
  --source scopus --run-type full \
  --query 'TITLE-ABS-KEY("synthetic term")' \
  --reported-results 17 --downloaded-results 17 \
  --parent-run-id test-00001 --child-run-id test-00002 \
  --database-name Scopus --platform ScopusAPI \
  --search-fields title,abstract,keywords \
  --searched-at-utc 2026-01-02T10:00:00Z \
  --exported-at-utc 2026-01-02T10:25:00Z \
  --source-zenodo-status unverified --output-root "$tmp/records"
Rscript scripts/updater/write_workflow00_search_record.R \
  --source scopus --run-type full \
  --search-plan "$tmp/full/search_plan.json" \
  --reported-results 17 --downloaded-results 17 \
  --parent-run-id test-00001 --child-run-id test-00003 \
  --searched-at-utc 2026-01-02T10:00:00Z \
  --exported-at-utc 2026-01-02T10:25:00Z \
  --source-zenodo-status unverified --output-root "$tmp/from_plan"

TMP_TEST_DIR="$tmp" Rscript - <<'RS'
library(jsonlite)
root <- Sys.getenv("TMP_TEST_DIR")
load <- function(path)fromJSON(file.path(root,path),simplifyVector=FALSE)
full <- load("full/search_plan.json")
monthly <- load("monthly/search_plan.json")
selected <- load("selected/search_plan.json")
stopifnot(identical(full$schema,"evidence-search-plan-v2"),
          length(full$source_queries)==2L,
          identical(full$source_queries$scopus,'TITLE-ABS-KEY("synthetic term")'),
          identical(full$source_mode_support$scopus$expansion,FALSE),
          identical(monthly$source_definitions$scopus$window$from,"2026-01-01"),
          identical(monthly$source_definitions$wos$filter$modifiedTimeSpan$to,"2026-02-01"),
          length(selected$source_queries)==2L)
sel<-load("monthly_selection.json")
stopifnot(identical(sort(unlist(sel$selected_sources)),sort(c("scopus","wos"))),
          identical(sel$selection_origin,"saved_full_run"))
files<-list.files(file.path(root,"records"),pattern="\\.json$",recursive=TRUE,full.names=TRUE)
stopifnot(length(files)==1L)
planned_files<-list.files(file.path(root,"from_plan"),pattern="\\.json$",recursive=TRUE,full.names=TRUE)
stopifnot(length(planned_files)==1L)
planned_receipt<-fromJSON(planned_files[[1L]],simplifyVector=FALSE)
stopifnot(identical(planned_receipt$database_name,"Scopus"),
  identical(planned_receipt$search_string,'TITLE-ABS-KEY("synthetic term")'),
  identical(unlist(planned_receipt$searched_fields),c("title","abstract","keywords")))

receipt<-fromJSON(files[[1L]],simplifyVector=FALSE)
stopifnot(identical(receipt$database_name,"Scopus"),
  identical(receipt$search_executed_at_utc,"2026-01-02T10:00:00Z"),
  identical(receipt$exported_at_utc,"2026-01-02T10:25:00Z"),
  identical(unlist(receipt$searched_fields),c("title","abstract","keywords")),
  identical(receipt$source_archive$publication_status,"unverified"))
cat("PASS: topic-neutral full and monthly search planning; source selection; provenance receipt\n")
RS
# Negative check: an expansion without verified source-specific search strings
# must fail, rather than fabricate an expansion query.
if Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type expansion --config "$config" --output-dir "$tmp/invalid" 2>"$tmp/expected_error"; then
  echo "ERROR: expansion should fail without verified per-source query" >&2
  exit 1
fi
grep -q "no verified expansion query" "$tmp/expected_error"
echo "PASS: unsupported query mode fails closed"
