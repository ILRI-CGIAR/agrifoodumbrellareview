#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag), call.=FALSE)
  args[[i+1L]]
}
source <- arg("--source")
run_type <- arg("--run-type")
query <- arg("--query")
reported <- suppressWarnings(as.integer(arg("--reported-results")))
downloaded <- suppressWarnings(as.integer(arg("--downloaded-results")))
parent_run_id <- arg("--parent-run-id")
child_run_id <- arg("--child-run-id", Sys.getenv("GITHUB_RUN_ID","unknown"))
run_attempt <- arg("--run-attempt", Sys.getenv("GITHUB_RUN_ATTEMPT","1"))
artifact_name <- arg("--artifact-name","")
search_version <- arg("--search-version","")
additional_search_term <- arg("--additional-search-term","")
orchestrator_sha <- arg("--orchestrator-sha", Sys.getenv("GITHUB_SHA",""))
source_handler_sha <- arg("--source-handler-sha", Sys.getenv("GITHUB_SHA",""))
window_from <- arg("--window-from","")
window_to <- arg("--window-to","")
known_native_results <- suppressWarnings(as.integer(arg("--known-native-results","")))
new_native_results <- suppressWarnings(as.integer(arg("--new-native-results","")))
source_manifest <- arg("--source-manifest","")
output_root <- arg("--output-root","outputs/updater/search_record")

req <- c(source,run_type,query,parent_run_id)
if (any(vapply(req,function(x)is.null(x)||!nzchar(x),logical(1)))) stop("source, run-type, query and parent-run-id are required",call.=FALSE)
if (is.na(reported) || reported < 0L) stop("reported-results must be >= 0",call.=FALSE)
if (is.na(downloaded) || downloaded < 0L) stop("downloaded-results must be >= 0",call.=FALSE)
if (!(run_type %in% c("full","fortnightly","monthly","expansion"))) stop("invalid run-type",call.=FALSE)

folder <- switch(run_type, full="full_search", fortnightly="fortnightly_update", monthly="monthly_update", expansion="ad_hoc")
stamp <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H-%M-%SZ")
safe_source <- gsub("[^a-z0-9_-]+","_",tolower(source))
base <- sprintf("%s_%s_parent-%s_child-%s_attempt-%s",stamp,safe_source,parent_run_id,child_run_id,run_attempt)
dir <- file.path(output_root,folder)
dir.create(dir,recursive=TRUE,showWarnings=FALSE)

database_scope <- NULL
if (nzchar(source_manifest)) {
  if (!file.exists(source_manifest)) stop(sprintf("source manifest not found: %s", source_manifest), call.=FALSE)
  source_manifest_obj <- fromJSON(source_manifest, simplifyVector=FALSE)
  database_scope <- source_manifest_obj$database_scope
}

record <- list(
  schema_version="1.2",
  recorded_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  source=source,
  run_type=run_type,
  search_version=if(nzchar(search_version)) search_version else NULL,
  additional_search_term=if(nzchar(additional_search_term)) additional_search_term else NULL,
  search_string=query,
  database_scope=database_scope,
  search_window=if(nzchar(window_from)||nzchar(window_to)) list(
    from=if(nzchar(window_from)) window_from else NULL,
    to=if(nzchar(window_to)) window_to else NULL
  ) else NULL,
  reported_search_results=reported,
  successfully_downloaded_results=downloaded,
  complete_download=identical(reported,downloaded),
  native_id_reconciliation=if(!is.na(new_native_results) || !is.na(known_native_results)) list(
    already_known=if(!is.na(known_native_results)) known_native_results else NULL,
    new=if(!is.na(new_native_results)) new_native_results else NULL,
    matching="exact source-native identifier",
    records_passed_downstream=if(!is.na(new_native_results)) new_native_results else NULL
  ) else NULL,
  github=list(
    repository=Sys.getenv("GITHUB_REPOSITORY",""),
    parent_workflow_run_id=parent_run_id,
    child_workflow_run_id=child_run_id,
    run_attempt=run_attempt,
    ref=Sys.getenv("GITHUB_REF_NAME",""),
    triggering_sha=Sys.getenv("GITHUB_SHA",""),
    orchestrator_sha=if(nzchar(orchestrator_sha)) orchestrator_sha else NULL,
    source_handler_sha=if(nzchar(source_handler_sha)) source_handler_sha else NULL
  ),
  artifact_name=if(nzchar(artifact_name)) artifact_name else NULL
)

json_path <- file.path(dir,paste0(base,".json"))
md_path <- file.path(dir,paste0(base,".md"))
writeLines(toJSON(record,auto_unbox=TRUE,pretty=TRUE,null="null"),json_path)

md <- c(
  sprintf("# Search record: %s", source),
  "",
  sprintf("- **Date (UTC):** %s", record$recorded_at_utc),
  sprintf("- **Run type:** %s", run_type),
  sprintf("- **Search version:** %s", if(nzchar(search_version)) search_version else "not supplied"),
  sprintf("- **Source:** %s", source),
  sprintf("- **Results reported by source:** %d", reported),
  sprintf("- **Results successfully downloaded:** %d", downloaded),
  sprintf("- **Complete download:** %s", if(reported==downloaded) "yes" else "no"),
  sprintf("- **Search window:** %s", if(nzchar(window_from)||nzchar(window_to)) paste0(window_from," to ",window_to) else "not applicable"),
  sprintf("- **Already-known native IDs:** %s", if(!is.na(known_native_results)) known_native_results else "not applicable"),
  sprintf("- **New native IDs passed downstream:** %s", if(!is.na(new_native_results)) new_native_results else "not applicable"),
  sprintf("- **Parent workflow run:** %s", parent_run_id),
  sprintf("- **Child workflow run:** %s (attempt %s)", child_run_id, run_attempt),
  sprintf("- **Harvest artifact:** %s", if(nzchar(artifact_name)) artifact_name else "not supplied"),
  sprintf("- **Orchestrator commit:** %s", if(nzchar(orchestrator_sha)) orchestrator_sha else "not supplied"),
  sprintf("- **Source-handler commit:** %s", if(nzchar(source_handler_sha)) source_handler_sha else "not supplied"),
  "",
  "## Search string",
  "",
  "```text",
  query,
  "```"
)

if (!is.null(database_scope)) {
  value_or <- function(x, fallback="not recorded") {
    if (is.null(x) || length(x) == 0L || !nzchar(as.character(x[[1L]]))) fallback else as.character(x[[1L]])
  }
  dbs <- database_scope$databases_searched
  db_lines <- character()
  if (!is.null(dbs) && length(dbs)) {
    db_lines <- vapply(dbs, function(x) {
      code <- if (is.null(x$code)) "" else as.character(x$code)
      name <- if (is.null(x$name)) "" else as.character(x$name)
      if (nzchar(code)) sprintf("- %s (code: %s)", name, code) else sprintf("- %s", name)
    }, character(1))
  }
  md <- c(
    md,
    "",
    "## Database scope",
    "",
    sprintf("- **Platform:** %s", value_or(database_scope$platform)),
    sprintf("- **API product:** %s", value_or(database_scope$api_product)),
    sprintf("- **API database parameter:** %s", value_or(database_scope$database_parameter)),
    "- **Databases searched:**",
    db_lines,
    sprintf("- **Selection method:** %s", value_or(database_scope$selection_method)),
    sprintf("- **Coverage recorded at (UTC):** %s", value_or(database_scope$recorded_at_utc)),
    sprintf("- **Coverage note:** %s", value_or(database_scope$institutional_coverage_note)),
    sprintf("- **Core Collection editions:** %s", value_or(database_scope$core_collection_editions))
  )
}

if (nzchar(additional_search_term)) {
  md <- c(md,"","## Ad hoc expansion","","The immutable species block was unchanged.", sprintf("Additional search term: `%s`",additional_search_term))
}
writeLines(md,md_path)
cat(sprintf("SEARCH_RECORD_JSON=%s\nSEARCH_RECORD_MD=%s\n",json_path,md_path))