# Agri-food umbrella review

R-based bibliographic search and preparation code, migrated from [LivingEvidenceMap](https://github.com/thesalmonandthetomato/LivingEvidenceMap/tree/workflow01-final-architecture).

## Scope

Only W00 (literature search and RIS ingestion), W01 (deduplication), W02 (bibliographic enrichment), and W03 (publication-status checking) are in scope. The source repository is read-only and has not been modified.

## Migration status (6 November 2026)

**Library code transferred; production workflow orchestration is not yet ready.**

- 82 transferred R scripts passed GitHub Actions R syntax parsing in run [42250699575](https://github.com/ILRI-CGIAR/agrifoodumbrellareview/actions/runs/42250699575).
- JSON source/configuration files passed syntax validation in that run.
- Inherited workflow definitions failed before any jobs executed and were removed.
- Temporary syntax-validation workflow was deleted after its successful run.
- No complete W00-W03 end-to-end run has passed, and no restricted Zenodo deposition has been verified.
- No salmon-specific search query or historical salmon duplicate decisions have been imported into active project configuration.

## Project inputs required before a production run

1. A project-specific search strategy in `user_input/workflow00_search_strategy.json` and matching Lens query configuration in `config/lens_search.json`.
2. Search/API credentials and a project-specific `ZENODO_ACCESS_TOKEN` as GitHub Actions secrets.
3. Optional manual RIS deposits/pointers for CAB Abstracts and ProQuest if those sources are included.
4. A production orchestration workflow limited to W00-W03, including review handling and verified restricted Zenodo handoffs.

The migrated R scripts are not, by themselves, a runnable production pipeline. Do not enable searches or Zenodo publishing until the outstanding prerequisites and execution tests pass.
