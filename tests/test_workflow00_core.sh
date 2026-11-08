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
Rscript scripts/updater/write_workflow00_search_record.R \
  --source wos --run-type full \
  --search-plan "$tmp/full/search_plan.json" \
  --reported-results 11 --downloaded-results 11 \
  --parent-run-id test-00001 --child-run-id test-00004 \
  --searched-at-utc 2026-01-02T11:00:00Z \
  --exported-at-utc 2026-01-02T11:10:00Z \
  --source-zenodo-status unverified --output-root "$tmp/from_plan"
Rscript scripts/updater/workflow_00_validate_search_receipts.R \
  --search-plan "$tmp/full/search_plan.json" \
  --receipts-dir "$tmp/from_plan" \
  --output "$tmp/source_report.json"
if Rscript scripts/updater/workflow_00_validate_search_receipts.R \
  --search-plan "$tmp/full/search_plan.json" \
  --receipts-dir "$tmp/from_plan" \
  --output "$tmp/invalid_source_report.json" \
  --require-published true 2>"$tmp/missing_publication"; then
  echo "ERROR: unpublished source must block final report" >&2
  exit 1
fi
grep -q "Final source methods require verified published source archive" "$tmp/missing_publication"


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
stopifnot(length(planned_files)==2L)
planned_receipts<-lapply(planned_files,fromJSON,simplifyVector=FALSE)
planned_receipt<-Filter(function(z)identical(z$source,"scopus"),planned_receipts)[[1L]]
stopifnot(identical(planned_receipt$database_name,"Scopus"),
  identical(planned_receipt$search_string,'TITLE-ABS-KEY("synthetic term")'),
  identical(unlist(planned_receipt$searched_fields),c("title","abstract","keywords")))

receipt<-fromJSON(files[[1L]],simplifyVector=FALSE)
stopifnot(identical(receipt$database_name,"Scopus"),
  identical(receipt$search_executed_at_utc,"2026-01-02T10:00:00Z"),
  identical(receipt$exported_at_utc,"2026-01-02T10:25:00Z"),
  identical(unlist(receipt$searched_fields),c("title","abstract","keywords")),
  identical(receipt$source_archive$publication_status,"unverified"))
source_report<-load("source_report.json")
stopifnot(identical(source_report$schema,"evidence-search-source-report-v1"),
  as.integer(source_report$downloaded_manifestations)==28L,
  length(source_report$source_searches)==2L,
  identical(source_report$report_ready,FALSE))
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

# Full synthetic W00 ingest -> search record -> source inventory -> archive
# Runs the real inherited RIS handler. NO provider, Zenodo, or write to repo.
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type full --config tests/fixtures/workflow00_integration.json \
  --output-dir "$tmp/offline/plan"
Rscript scripts/updater/workflow_00_execute_source.R \
  --search-plan "$tmp/offline/plan/search_plan.json" \
  --source fixture_db --output-root "$tmp/offline/harvests" --execute true
Rscript scripts/updater/workflow_00_record_source.R \
  --search-plan "$tmp/offline/plan/search_plan.json" \
  --source fixture_db --output-root "$tmp/offline" \
  --parent-run-id 999999
Rscript scripts/updater/workflow_00_validate_search_receipts.R \
  --search-plan "$tmp/offline/plan/search_plan.json" \
  --receipts-dir "$tmp/offline/search_records" \
  --output "$tmp/offline/plan/source_search_report.json"
Rscript scripts/updater/workflow_00_archive_to_zenodo.R \
  --staging-root "$tmp/offline" --run-id 999999 \
  --repository "generic-org/topic-agnostic-repo" \
  --run-type full --search-version fixture-v1 \
  --sources fixture_db \
  --zenodo-config tests/fixtures/zenodo_metadata.json \
  --output-dir "$tmp/archive" --validate-only true
TMP_TEST_DIR="$tmp" Rscript - <<'RS'
library(jsonlite)
root <- Sys.getenv("TMP_TEST_DIR")
src <- file.path(root,"offline/harvests/fixture_db/source_child")
m <- fromJSON(file.path(src,"manifest.json"))
stopifnot(identical(m$status,"success"),
  as.integer(m$records$unique_records_for_handover)==2L)
lines <- readLines(file.path(src,"handoff/records.jsonl"),warn=FALSE)
stopifnot(length(lines)==2L)
rec <- fromJSON(file.path(root,"offline/plan/source_search_report.json"))
stopifnot(as.integer(rec$source_count)==1L,
  as.integer(rec$downloaded_manifestations)==2L,
  as.integer(rec$total_provider_reported_hits)==2L,
  identical(rec$source_searches$source,"fixture_db"))
v <- fromJSON(file.path(root,"archive/offline_validation.json"))
stopifnot(identical(v$status,"validated_not_published"),
  identical(v$archive_prefix,"topic-agnostic-repo"))
arch <- list.files(file.path(root,"archive/archive_files"),pattern="harvest.tar.gz$",full.names=TRUE)
stopifnot(length(arch)==1L)
members <- utils::untar(arch[[1L]],list=TRUE)
stopifnot(any(grepl("source_child/handoff/records.jsonl",members,fixed=TRUE)))
cat("PASS: actual manual RIS ingestion, methods receipt, complete report, SHA-verified offline archive; no network writes\n")
RS

# The generic dispatcher must route every supported backend without credentials.
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type full --config tests/fixtures/backend_dispatch.json \
  --output-dir "$tmp/backends/plan"
for SOURCE in scopus openalex agricola wos pubmed ebsco_medline lens fixture_db; do
  Rscript scripts/updater/workflow_00_execute_source.R \
    --search-plan "$tmp/backends/plan/search_plan.json" \
    --source "$SOURCE" --output-root "$tmp/backends/harvests" --execute false
done
# The final source-side handoff is also consumed by W01.
test -s "$tmp/offline/harvests/fixture_db/source_child/records.jsonl"
# Verify no accidental API requests or files were created by dry-run dispatch.
test ! -d "$tmp/backends/harvests"
Rscript - <<'RS'
library(yaml)
x<-read_yaml(".github/workflows/workflow_00_production.yml")
stopifnot("workflow_call" %in% names(x$on),
          "workflow_dispatch" %in% names(x$on),
          !("push" %in% names(x$on)),
          "search" %in% names(x$jobs))
for(script in c(
  "scripts/updater/workflow_00_execute_source.R",
  "scripts/updater/workflow_00_record_source.R",
  "scripts/updater/workflow_00_archive_to_zenodo.R",
  "scripts/updater/workflow_01_restore_workflow00_from_zenodo.R",
  "scripts/updater/workflow_00_update_state.R"
))parse(file=script)
cat("PASS: all eight backends dry-run, W01 handoff path, core workflow YAML and scripts parse\n")
RS
