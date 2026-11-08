# Evidence synthesis bibliographic pipeline

**Working branch:** `migration/w00-w03`. The original LivingEvidenceMap repository and this repository's `main` branch are not modified.

The repository contains generic source search/acquisition, bibliographic deduplication, metadata repair and publication-status logic. The current refactoring consolidates their permanent workflows and removes historical project-specific recovery code.

## Search and acquisition: permanent entry point

`.github/workflows/workflow_00_production.yml` is the single W00 entry point. It accepts a **new search strategy for any subject**, with a different source-specific Boolean string and field definition for every selected database. The original generic source handlers remain intact:

- Scopus: `workflow_00b_scopus_ingestion.R`
- OpenAlex: `workflow_00c_openalex_ingestion.R`
- AGRICOLA via Europe PMC: `workflow_00d_agricola_ingestion.R`
- Web of Science Starter: `workflow_00e_wos_starter_ingestion.R`
- Europe PMC (including MED, ETH, CBA, preprints): `workflow_00f_europe_pmc_source_ingestion.R`
- EBSCO EIT: `workflow_00g_ebsco_ingestion.R`
- Lens: `workflow_00_lens_orchestrated_search.R`
- Manual RIS: `workflow_00_ris_ingestion.R`

Each provider handler is invoked through `workflow_00_execute_source.R`. Original source identifiers, raw responses, search timestamps and field definitions are preserved; a common source-receipt validation and restricted Zenodo archiver handle the output. API sources also record native-ID reconciliation using the inherited routine.

## New-project configuration

Each configuration must have its own stable `project_id`, which prevents mixing the source history of unrelated reviews.

See `user_input/workflow00_search_strategy.example.json`, `user_input/zenodo_config.example.json`, and `user_input/README.md`. Copy them to project-specific locations, replace all placeholders and provide **verbatim database-valid queries**. Manual sources require original RIS files, a source registry and verified search/export timestamps.

By default the workflow has `execute=false` and `publish=false`. A dry run validates plans and handler routing without making provider requests. **The live source calls and Zenodo publication require explicit user input.** No test triggers them automatically. Published source receipts are committed on the invoking branch, while search data remain in restricted Zenodo.

### Current validation and limitations

The full synthetic RIS-ingestion → W01-ready sidecars → database search receipts → archived source tree pathway passed GitHub Actions offline. All eight source handler dispatch routes passed no-network checks. These results **do not establish live API access or a successful full cross-database production harvest**: those require a separate explicitly authorised run with actual project queries.

**Full and incremental acquisition share the same permanent W00 workflow.** Fortnightly, monthly and expansion searches require a previously published W00 source archive from the **same explicit project_id**, supplied through `previous_w00_pointer_path`. The workflow restores this archive, reconciles exact source-native identifiers and archives only the current run with a cumulative ID registry; duplicate API and sidecar implementations have been consolidated. Adding sources to an existing search is done with `expansion`. Initial and update reconciliation, source-receipt audit, offline Zenodo package generation and W01-ready source inputs passed synthetic tests. **No live provider-API update or new Zenodo publication has been performed during this refactor.**

## Final output contract

After deduplication, metadata enrichment and publication-status checks, the eventual automatic export will produce:

- RIS with one retained record per work and **all contributing database sources**
- Source provenance CSV with source-native identifiers
- One consolidated PDF containing searches, dates, field scopes, hit counts, deduplication, automatic/manual abstract repair, exclusions with DOI lists and archived data DOI links
- Exclusion audit CSV and machine-readable checksum/count validation

See `docs/architecture/final_export_contract.md`. This final exporter is **not yet integrated**; W01–W03 still contain legacy review-specific code to be removed and corrected in the next stages.

Historical run pointers remain in `docs/` until downstream workflow refactoring is complete, but they are **not new-project defaults**. No previous published Zenodo record is rewritten by this refactoring.
