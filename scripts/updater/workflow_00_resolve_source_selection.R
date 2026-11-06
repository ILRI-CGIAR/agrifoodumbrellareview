#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

run_type <- arg("--run-type")
saved_path <- arg("--saved","data/workflow00/source_selection.json")
output_path <- arg("--output","outputs/updater/workflow00_plan/source_selection.json")
ebsco_config <- arg("--ebsco-config","config/workflow00_ebsco_sources.json")
group_config <- arg("--group-config","config/workflow00_source_groups.json")
search_registry <- arg("--search-registry","docs/search_record/zenodo_registry.csv")

if (!(run_type %in% c("full","fortnightly","monthly","expansion"))) stop("Invalid run type",call.=FALSE)

ebsco_cfg <- fromJSON(ebsco_config,simplifyVector=FALSE)
group_cfg <- fromJSON(group_config,simplifyVector=FALSE)
if (is.null(group_cfg$groups) || !length(group_cfg$groups)) stop("Workflow 00 source-group catalogue contains no groups",call.=FALSE)

ebsco_sources <- names(ebsco_cfg$sources)
base_sources <- c("lens","scopus","openalex","agricola","pubmed","ethos","cba","epmc_preprints","wos")
supported <- unique(c(base_sources,ebsco_sources))

all_group_sources <- unique(unlist(lapply(group_cfg$groups,function(z) unlist(z$sources,use.names=FALSE)),use.names=FALSE))
unknown_group_sources <- setdiff(all_group_sources,supported)
if (length(unknown_group_sources)) {
  stop(sprintf("Source-group catalogue contains unsupported sources: %s",paste(unknown_group_sources,collapse=", ")),call.=FALSE)
}

env_group_name <- function(group) paste0("W00_GROUP_",toupper(gsub("[^A-Za-z0-9]","_",group)))
selected_groups_from_env <- function() {
  g <- names(group_cfg$groups)
  g[vapply(g,function(x) identical(tolower(Sys.getenv(env_group_name(x))),"true"),logical(1))]
}

parse_source_list <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character())
  z <- trimws(unlist(strsplit(x,",",fixed=TRUE),use.names=FALSE))
  unique(z[nzchar(z)])
}

legacy_env_name <- function(src) paste0("W00_SELECT_",toupper(gsub("[^A-Za-z0-9]","_",src)))
selected_legacy_sources <- function() {
  supported[vapply(supported,function(src) identical(tolower(Sys.getenv(legacy_env_name(src))),"true"),logical(1))]
}

if (run_type=="full") {
  selected_groups <- selected_groups_from_env()
  additional_sources <- parse_source_list(Sys.getenv("W00_ADDITIONAL_SOURCES"))
  excluded_sources <- parse_source_list(Sys.getenv("W00_EXCLUDE_SOURCES"))

  bad_add <- setdiff(additional_sources,supported)
  bad_exclude <- setdiff(excluded_sources,supported)
  if (length(bad_add)) stop(sprintf("Additional source(s) unsupported: %s",paste(bad_add,collapse=", ")),call.=FALSE)
  if (length(bad_exclude)) stop(sprintf("Excluded source(s) unsupported: %s",paste(bad_exclude,collapse=", ")),call.=FALSE)

  grouped_sources <- unique(unlist(lapply(selected_groups,function(g) unlist(group_cfg$groups[[g]]$sources,use.names=FALSE)),use.names=FALSE))
  selected <- unique(c(grouped_sources,additional_sources))
  selected <- selected[!(selected %in% excluded_sources)]
  selected <- supported[supported %in% selected]

  legacy_selected <- selected_legacy_sources()
  if (!length(selected) && length(legacy_selected)) {
    selected <- legacy_selected
    origin <- "legacy_full_run_inputs"
  } else {
    origin <- "full_run_group_inputs"
  }
  if (!length(selected)) stop("Select at least one database group or additional source for a full Workflow 00 run",call.=FALSE)
  baseline_run_id <- Sys.getenv("GITHUB_RUN_ID")
} else {
  if (!file.exists(saved_path)) {
    stop(sprintf("%s requires a saved full-run source selection at %s",run_type,saved_path),call.=FALSE)
  }
  saved <- fromJSON(saved_path,simplifyVector=FALSE)
  if (!identical(saved$schema,"living-evidence-map-workflow00-source-selection-v1")) {
    stop("Unsupported saved Workflow 00 source-selection schema",call.=FALSE)
  }
  selected <- trimws(as.character(unlist(saved$selected_sources,use.names=FALSE)))
  selected <- selected[nzchar(selected)]
  unknown <- setdiff(selected,supported)
  if (length(unknown)) stop(sprintf("Saved source selection contains unsupported sources: %s",paste(unknown,collapse=", ")),call.=FALSE)
  if (!length(selected)) stop("Saved source selection is empty",call.=FALSE)

  selected_groups <- if (is.null(saved$selected_groups)) character() else as.character(unlist(saved$selected_groups,use.names=FALSE))
  additional_sources <- if (is.null(saved$additional_sources)) character() else as.character(unlist(saved$additional_sources,use.names=FALSE))
  excluded_sources <- if (is.null(saved$excluded_sources)) character() else as.character(unlist(saved$excluded_sources,use.names=FALSE))
  origin <- "saved_full_run"
  baseline_run_id <- as.character(saved$baseline_run_id)

  if (run_type == "monthly") {
    # Use the accepted current update's actual API source roster, not the
    # possibly partial saved selection from the October full-run test.
    status_path <- "docs/current_run/current_run_status.json"
    if (!file.exists(status_path)) stop("Monthly run requires current-run source provenance",call.=FALSE)
    status <- fromJSON(status_path,simplifyVector=FALSE)
    if (!identical(as.character(status$progress$W10$status),"complete")) stop("Previous pipeline update is not marked complete",call.=FALSE)
    api_sources <- intersect(names(status$search$sources),supported)
    if (length(api_sources)!=19L || anyDuplicated(api_sources)) {
      stop(sprintf("Expected exactly 19 previously searched API sources, found %d",length(api_sources)),call.=FALSE)
    }
    selected <- supported[supported %in% api_sources]
    selected_groups <- character()
    additional_sources <- selected
    excluded_sources <- character()
    origin <- "previous_complete_update_api_sources"
  }

  if (run_type == "expansion") {
    # Expansion covers every API database with a previously published W00 search,
    # not merely the selection from the last (possibly partial) full run.
    if (!file.exists(search_registry)) stop(sprintf("W00 search registry missing: %s",search_registry),call.=FALSE)
    events <- read.csv(search_registry,stringsAsFactors=FALSE,check.names=FALSE)
    needed <- c("sources","zenodo_record_id","published_at_utc")
    if (!all(needed %in% names(events))) stop("W00 search registry missing required columns",call.=FALSE)
    completed <- nzchar(trimws(events$zenodo_record_id)) & nzchar(trimws(events$published_at_utc))
    if (!any(completed)) stop("W00 search registry has no published search events",call.=FALSE)
    previous <- unique(trimws(unlist(strsplit(paste(events$sources[completed],collapse=";"),";",fixed=TRUE))))
    previous <- previous[nzchar(previous)]
    unknown <- setdiff(previous,supported)
    if (length(unknown)) stop(sprintf("Published W00 registry contains unsupported source(s): %s",paste(unknown,collapse=", ")),call.=FALSE)
    # EThOS has not had a baseline search: it needs its own full run.
    if ("ethos" %in% previous) stop("EThOS already appears in the published W00 registry; reassess full-vs-expansion search plan",call.=FALSE)
    # Explicit first-time additions are searched using the original full query.
    new_full_sources <- parse_source_list(Sys.getenv("W00_NEW_FULL_SOURCES"))
    if (length(setdiff(new_full_sources,"ethos"))) stop("Only EThOS is currently authorised as a first-time full source in an expansion",call.=FALSE)
    if (any(new_full_sources %in% previous)) stop("A declared first-time full source already has a published W00 search",call.=FALSE)
    selected <- supported[supported %in% unique(c(selected,previous,new_full_sources))]
    origin <- "cumulative_published_w00_search_registry"
  }
}

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
out <- list(
  schema="living-evidence-map-workflow00-source-selection-v1",
  run_type=run_type,
  selection_origin=origin,
  baseline_run_id=baseline_run_id,
  selected_groups=unname(selected_groups),
  additional_sources=unname(additional_sources),
  excluded_sources=unname(excluded_sources),
  selected_sources=unname(selected),
  selected_source_count=length(selected),
  new_full_sources=if(run_type=="expansion") unname(new_full_sources) else character(),
  generated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(out,auto_unbox=TRUE,pretty=TRUE,null="null"),output_path)

gh <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh)) {
  con <- file(gh,"at")
  on.exit(close(con),add=TRUE)
  cat(sprintf("selected_sources_csv=%s\n",paste(selected,collapse=",")),file=con)
  cat("selected_sources_json<<EOF\n",file=con)
  cat(toJSON(unname(selected),auto_unbox=TRUE),file=con)
  cat("\nEOF\n",file=con)
}
message(sprintf("PASS: Workflow 00 %s source selection: %s",run_type,paste(selected,collapse=", ")))
