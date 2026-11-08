# Final export and client reporting contract

Status: **agreed interface, implementation pending integration into the permanent generic workflows**.

This contract applies to any project and any topic. No search terms, source identifiers, run IDs, fixed counts, historical DOI pointers or paths to a particular review are embedded in the exporting algorithm.

## Invocation and dependencies

The final output stage starts **after the publication-status check**. It is passed, from the current run's immutable manifest:

- A checksum-verified canonical JSONL containing all deduplicated works and their full `manifestation_refs` source identifiers.
- A checksum-verified publication-status JSONL with exactly one assessment per canonical `record_id`.
- An optional validated manual metadata-repair JSONL. Manual abstract recovery is enabled only for **100% equal normalised titles**, with original-source and decision provenance. It is never applied to excluded publications.
- Search receipt files for every source, specifying database/platform, verbatim executed search string (not a reconstructed query), field scope, search date and export timestamp, provider-reported hits, downloaded count, post-native-ID reconciliation count, source artifact and DOI/publication status.
- The deduplication and enrichment run manifests, including automatic and manual repairs, counts and publication pointers.

All paths and project metadata are read from configuration supplied **for the current project/run**. Exporting may run locally using restored inputs; it must not require any new bibliographic provider calls or a new Zenodo publication.

## Required outputs

1. `included_records.ris`: **one RIS record per canonical work**, excluding exactly those with `publication_status.exclude_from_workflow04 == true` (the field remains compatible even after the screening workflow is removed). Fields include stable `ID`, title, authors, year, journal, DOI, abstract, keywords and source identifiers. The RIS includes **all unique contributing database names** in a documented custom field (e.g., `DB`) and the full source manifestation identifiers in a separate repeated field (e.g., `N1`). RIS readers differ in custom-field support, so the separate provenance CSV remains authoritative.
2. `record_database_provenance.csv`: one row per included canonical work, stable `record_id`, a delimited list of **all database/platform names**, and full per-source manifestation identifiers. The database membership count must equal the number of unique source prefixes, not the number of merged manifestations.
3. `methods_and_results_report.pdf`: one client-readable PDF generated automatically from verified manifests, containing:
   - Database-by-database verbatim search strategies, searched fields, exact execution/export dates, reported hit counts, record counts and DOI links to genuine published Zenodo deposits. Label reserved or unverified source DOIs explicitly, and never present them as published.
   - Deduplication: retrieved source manifestations, canonical works, removed duplicates, ambiguous pairs and resolution decisions.
   - Abstract repair: automatic repair, quarantine/decision outcomes, manual exact-title-only recovery, provenance, remaining missing abstracts.
   - Publication-status checks: counts by status, all excluded work IDs and DOIs, and the inclusion count used for RIS.
   - Source archives and downstream published checkpoint DOI links, with explicit restricted/public access labels.
4. `excluded_publications.csv`: all work IDs, DOIs, status and evidence identifiers for exclusions, preserving traceability.
5. `export_validation.json`: every input SHA256, output SHA256, record counts, source-count reconciliation, repair counts, report completeness, generator version and warnings.

## Validation and fail-closed behaviour

- Every input must match its stated SHA256 and `record_id` membership. Duplicates, missing publication-status records, unexpected count differences, invalid/source-less provenance or inconsistent exclusion totals **fail**.
- Complete RIS and provenance CSV must have identical canonical work-ID sets.
- If any manual repair is applied, it must target an existing retained record with an empty abstract and a verified exact normalised title match. Reject fuzzy, prefix, DOI-only, near-match or title-discrepant imports. Flag ambiguous multiple provider abstracts for review before writing.
- Search-result provenance must **not** be inferred from the provider that supplied a repaired abstract: it comes only from original search manifestations.
- Report generation must never fabricate dates, search strings, field scopes, hit counts or published DOIs. For a client-final report, missing required search receipt fields **fail preflight**; a clearly marked draft report may be generated separately if explicitly requested.
- All included records and the PDF count must reconcile to `canonical_count - excluded_count`.
- An export must be reproducible from downloaded historical run manifests and data, without re-running searches, metadata APIs or retraction APIs.
- Published Zenodo records remain immutable. A new final export may be deposited to Zenodo **only by explicit request**.

## Architecture placement

- **Search/acquisition:** write a machine-readable, immutable source receipt for each database at retrieval time. Include verbatim query and real timestamps.
- **Deduplication:** retain per-work manifestation IDs after merging and preserve undecided pairs as independently reviewable, never silently accept them.
- **Metadata repair:** append abstract-repair and human-decision provenance to the run manifest; import manual RIS from configured inputs with exact-title gate.
- **Retraction checking:** emit one status per work and an explicit DOI/work-ID exclusion list. Run the final export step after this stage, not from the earlier deduplication-only RIS exporter.
- **Testing:** dry-run on small synthetic, topic-neutral fixtures with overlapping databases, multiple manifestations, exact-title manual repairs, conflicting title/DOI cases, retractions and missing metadata. Inspect output RIS round-trip and PDF content and compare SHA/count ledgers.

This is a specification for integrating permanent code during the workflow-by-workflow refactor; do not create an additional recovery launcher, hard-code current ILRI paths, modify prior published deposits or trigger real database calls to test it.
