# Project search inputs

For every new review, supply a **new, topic-specific configuration file** at
`user_input/workflow00_search_strategy.json`. Do not reuse queries from a
historical review.

The expected format is `evidence-search-strategy-v1`. A synthetic,
topic-neutral example is in `tests/fixtures/workflow00_search_strategy.json`.

For each selected database, define:

- `database_name`: human-readable database name to use in provenance.
- `platform`: the platform/API used for the search.
- `search_fields`: fields actually queried, e.g. title, abstract, keywords.
- `queries.full.query`: the **verbatim provider-specific Boolean query**, with the
  real date/publication restrictions included or separately captured as a
  provider-verified filter.
- `queries.monthly` or `queries.fortnightly`, if updates are needed: an
  independently verified provider-specific query and retrieval filter/window.
- `queries.expansion`, if expanding the original search: a fully specified
  provider-specific query, not an automatically combined search expression.

The planner writes `source_queries.json` and `search_plan.json`, and does
**not contact external databases**.

Each completed search must write an auditable receipt using
`scripts/updater/write_workflow00_search_record.R`, including the search plan,
provider-reported hit count, successfully downloaded count, source-native ID
reconciliation, actual search and export timestamps, and the source archive
publication status. A Zenodo DOI reservation is not equivalent to publication.

**Do not supply dummy dates or copy publication/retrieval counts from another
project.** Missing fields must be flagged, and client-final reports must fail
preflight if any mandatory provenance is unverified.

Original historic project search strings and manual source inventories remain
in the branch until migration of historic state into an archive is complete.
They must not be treated as defaults for new reviews.
