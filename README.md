# Agri-food umbrella review: reproducible bibliographic pipeline

R-based literature search, deduplication, metadata repair, retraction checks and export, developed for the ILRI-CGIAR project. Work only on `migration/w00-w03`; the original LivingEvidenceMap repository and this repository's `main` branch must not be changed by these pipeline refinements.

## Completed first-run benchmark (7–8 November 2026)

| Stage | Verified result | Restricted Zenodo DOI |
| --- | ---: | --- |
| Source manifestations across 8 databases | 46,361 | Search sources have separate, partly unverified deposits |
| Deduplicated canonical publications | 21,054 | [10.5281/zenodo.24042387](https://doi.org/10.5281/zenodo.24042387) |
| Enrichment patch for records requiring repair | 5,595 | [10.5281/zenodo.24044641](https://doi.org/10.5281/zenodo.24044641) |
| Compact enriched canonical checkpoint | 21,054 | [10.5281/zenodo.24044666](https://doi.org/10.5281/zenodo.24044666) |
| Publication-status assessments | 21,054 (25 retractions) | [10.5281/zenodo.24044731](https://doi.org/10.5281/zenodo.24044731) |
| Non-retracted publications for RIS export | 21,029 | **The client-ready RIS is not itself a Zenodo publication** |

The manual Web of Science/Scopus RIS abstract patch adds 234 abstracts by exact normalised-title agreement only. This patch was applied to the client RIS/export after the published enriched checkpoint, and must be applied again in a deliberately versioned export stage in future runs. The original published Zenodo checkpoint is immutable.

The detailed **eight-source source counts**, run IDs and publication pointers are in `docs/workflow00/ilri_validated_artifacts_2026-11-07.json` and `docs/current_run/current_run_status.json`. Verify their consistency from a fresh R session:

```r
install.packages("jsonlite")
system2("Rscript", "scripts/updater/ilri_validate_published_baseline.R")
```

The same checks run in `.github/workflows/ilri_baseline_provenance_checks.yml`. They are read-only: no API searches, Zenodo deposits or state changes.

## Refactoring plan, one stage at a time

1. **Search/acquisition**: one project-specific source roster; preserve verbatim database-specific strings, exact searched fields, real search and export timestamps, provider-reported hits, successfully retrieved records, native-ID deduplication, complete retrieval evidence, per-source immutable archive receipt, and a full coverage report. Reject placeholder dates in final client reports. Do not repurpose the original salmon-oriented generic search generator for this ILRI strategy.
2. **Deduplication**: reproducible incremental state and a reviewer-controlled ambiguity queue; source manifestations retained, so a merged publication lists every contributing database. Do not silently equate `no decision` with `not duplicate`.
3. **Metadata repair**: Europe PMC then Scopus with explicit DOI and title identity checks, idempotent batch resumes using input/code fingerprinting that excludes orchestration-only changes, auditable provider quarantine and decisions; allow separately sourced manual RIS abstract updates using strict normalised-title equality.
4. **Retraction check**: publication-status evidence, positive evidence precedence, explicit excluded work IDs and DOIs in summary; no unnecessary provider reruns to produce a new report.
5. **Export/reporting**: one command generating an RIS of non-retracted records, full multi-database provenance field(s), provenance CSV, search methods, abstract-repair and retraction reports in PDF, and machine-readable provenance receipts. Reconcile record counts and archive checksums before release.

## Safety and update policy

- Keep original first-run records, registries and DOI pointers unchanged as reference fixtures. Never mutate published deposits to backfill newer abstract repairs.
- Each stage must pass a dry-run fixture and validate its predecessor's signed inventory/manifest before any production invocation.
- No work beyond searching, deduplication, enrichment, status checks and export is configured for this repository. W04 is **not** part of this project.
- Do not trigger searches, repeated API enrichment, Zenodo publication or automatic downstream dispatch merely by committing documentation or temporary scripts.
- Stale migrated workflow definitions currently remain in the repository. Some refer to unavailable Shiny/Google Sheets features, to `main`, or to source inventories unrelated to this project: these must be reviewed individually before any recurring automation is enabled.

## Known W00 issues to resolve

The inherited `workflow_00_search_orchestrator.R` expects `user_input/workflow00_search_strategy.json`, which is absent from this ILRI branch and is not an appropriate representation of the existing large database-specific umbrella-review queries. `workflow_00_resolve_source_selection.R` includes an unrelated 19-source monthly-update assumption. Current manual registry files for CAB, ProQuest and LILACS mark their dates as **technical placeholders**, not verified export dates; their Zenodo DOIs are reserved draft identifiers, not confirmed publications. The existing `ilri_launch_original_w01.yml` is a historical first-run launcher, not a safe recurring scheduler.

Therefore, the next production-search implementation must be project-specific and fail closed when search syntax, native filtering, timestamps or archive metadata are incomplete.
