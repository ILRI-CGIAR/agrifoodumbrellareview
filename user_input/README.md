# Project-specific search and archive configuration

For a new unrelated project:

1. Copy `workflow00_search_strategy.example.json` to your project configuration path. Replace the project strategy version, every search string and the listed databases with the exact provider-specific searches you intend to run. Keep only databases that are actually being searched. The workflow never translates a Scopus string into another provider's syntax.
2. Copy `zenodo_config.example.json` to the project metadata path. Replace the project title and creators. Source deposits are restricted, and publication requires a separately selected `publish=true` action.
3. When searching a manually exported database, use `backend: "manual_ris"`. Supply `options.registry`, `options.ris_files`, and verified `search_executed_at_utc` and `exported_at_utc` timestamps. The registry must use `workflow00-source-registry-v1` and exactly match the uploaded RIS filenames.
4. Open the permanent `Workflow 00 - Search and source archive` workflow, supply both configuration file paths, and first use `execute=false`, `publish=false` to check routing. Only initiate live queries and Zenodo publication after explicit approval.

## Backend configuration

The `sources` object maps a unique lowercase source code to a source definition containing:

- `backend`: `scopus`, `openalex`, `agricola`, `wos`, `europe_pmc`, `ebsco`, `lens`, or `manual_ris`.
- `database_name` and `platform`: names to be preserved in search reports and record provenance.
- `search_fields`: the fields actually searched.
- `queries.full.query`: the **verbatim** search submitted to the named provider.
- `options`: source-specific parameters (manual RIS files/registry, EBSCO profile configuration, or a full Lens API query object). For Lens, `options.lens_api_query.query.bool.must[0].query_string.query` must match the supplied query string exactly.
- Optional `queries.monthly`, `queries.fortnightly`, and `queries.expansion` with explicitly verified retrieval windows. The script does not invent date filtering or search syntax.

The working R source entry points are `scripts/updater/workflow_00_search_orchestrator.R`, `workflow_00_execute_source.R`, `workflow_00_record_source.R`, `workflow_00_validate_search_receipts.R`, and the inherited source handler scripts.

**Limit:** The W00 production runner currently allows full first-run execution only. Live update/expansion runs intentionally fail preflight until cumulative native-ID reconciliation is validated. The final RIS/PDF report belongs to the later permanent export stage and is not part of the initial database retrieval.
