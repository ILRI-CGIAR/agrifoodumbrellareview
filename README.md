# Evidence synthesis: bibliographic acquisition and processing

This repository is being refactored into a **topic-independent, project-configured** pipeline for literature searching, deduplication, bibliographic metadata repair, publication-status checking, and final export.

**Refactoring branch:** `migration/w00-w03`. Do not make changes to `main` or the source LivingEvidenceMap repository.

## Pipeline architecture

1. **Search and acquisition**: project-specific, provider-valid Boolean strategies supplied as data, recorded along with fields searched, actual search/export dates, provider-reported hits, downloaded records, per-source native-ID reconciliation, and authenticated source archives.
2. **Deduplication**: one canonical work per publication, with every contributing database and original source identity preserved. Decisions requiring review remain auditable.
3. **Metadata repair**: API and manual abstract fills with DOI/title identity safeguards; preserve conflicting provider evidence and exact decisions.
4. **Publication status**: retraction/correction/withdrawal checks and a publication-status record for every canonical work.
5. **Export and reporting**: final included-record RIS (including multiple database provenance), provenance CSV, excluded-work audit, one consolidated methods/results PDF, and checksums.

The final-export format and validation rules are defined in `docs/architecture/final_export_contract.md`.

## Current refactor status

The **search query planner, source selection, and search receipt writer** are being made independent of topic or prior run IDs. The permanent smoke/regression tests are in `tests/test_workflow00_core.sh`, executed by `.github/workflows/core_pipeline_checks.yml`.

The generic search planner reads `user_input/workflow00_search_strategy.json`, with schema `evidence-search-strategy-v1`, and requires the **full verbatim executed query** and search-field definition for every selected database. Each update also requires its own provider-verified query/filter; the planner does not manufacture date logic. Example format: `tests/fixtures/workflow00_search_strategy.json`. The fixture is synthetic and not a valid substantive search strategy.

Example, from a fresh R session / shell:

```bash
Rscript scripts/updater/workflow_00_search_orchestrator.R \
  --run-type full \
  --config user_input/workflow00_search_strategy.json \
  --output-dir outputs/updater/workflow00_plan

bash tests/test_workflow00_core.sh
```

Neither example executes a remote search or publishes to Zenodo. Source-specific harvesters and the downstream workflows must still be integrated and validated before launching a new project's first full pipeline run.

## Historical production evidence

The branch retains immutable pointers and receipts for an earlier eight-database run, which successfully produced 46,361 original manifestations, 21,054 canonical works, and 25 retraction exclusions. These are **historical provenance, not inputs or expected counts for a new project**.

Published restricted Zenodo records for that earlier project: [deduplication](https://doi.org/10.5281/zenodo.24042387), [metadata repair](https://doi.org/10.5281/zenodo.24044641), [canonical handoff](https://doi.org/10.5281/zenodo.24044666), [publication-status layer](https://doi.org/10.5281/zenodo.24044731). Earlier source-specific draft DOI identifiers are not evidence of published deposits.

## Safety requirements

- Never use the earlier project's source counts, queries, dates, review IDs or DOI pointers as defaults for a new project.
- Preserve original source manifestations and use their identifiers, not abstract-repair providers, to determine database provenance.
- Do not infer original search dates from file upload, registry or GitHub execution timestamps.
- Published restricted Zenodo states are immutable. Producing an export does not authorise a new deposit.
- Run offline tests first. No expensive API calls or automatic downstream dispatch during refactoring.

**Work remaining:** consolidate source-specific acquisition and manual RIS ingestion, then deduplication, enrichment, retraction checking, and generic final reporting. The repository is **not yet approved as an end-to-end reusable production system**.
