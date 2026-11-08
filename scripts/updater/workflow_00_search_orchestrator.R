#!/usr/bin/env Rscript
# Topic-independent query planner. A source-specific, executed Boolean query is
# supplied for each selected database; no concepts are inferred from its content.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(key, default=NULL) {
  i <- match(key,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop("Missing value after ",key,call.=FALSE)
  args[[i+1L]]
}
required_string <- function(x,label) {
  if(is.null(x)||length(x)!=1L||is.na(x)||!nzchar(trimws(x)))
    stop("Missing or invalid ",label,call.=FALSE)
  as.character(x)
}
run_type <- arg("--run-type")
if(!(run_type %in% c("full","fortnightly","monthly","expansion")))
  stop("--run-type must be full, fortnightly, monthly, or expansion",call.=FALSE)
config_path <- arg("--config","user_input/workflow00_search_strategy.json")
out <- arg("--output-dir","outputs/updater/workflow00_plan")
selection_path <- arg("--source-selection","")
if(!file.exists(config_path)) stop("Missing search strategy: ",config_path,call.=FALSE)
cfg <- fromJSON(config_path,simplifyVector=FALSE)
if(!identical(cfg$schema,"evidence-search-strategy-v1"))
  stop("Search strategy schema must be evidence-search-strategy-v1",call.=FALSE)
version <- required_string(cfg$search_version,"search_version")
if(!is.list(cfg$sources)||is.null(names(cfg$sources))||!length(cfg$sources))
  stop("Configure a named 'sources' mapping with one entry per source",call.=FALSE)
selected <- names(cfg$sources)
if(nzchar(selection_path)){
  if(!file.exists(selection_path))stop("Missing selected-source state: ",selection_path,call.=FALSE)
  sel <- fromJSON(selection_path,simplifyVector=FALSE)
  if(!identical(sel$schema,"living-evidence-map-workflow00-source-selection-v1"))
    stop("Unsupported source-selection schema",call.=FALSE)
  selected <- unlist(sel$selected_sources,use.names=FALSE)
}
if(!length(selected)||anyDuplicated(selected)||any(!selected %in% names(cfg$sources)))
  stop("Selected sources must be unique and explicitly configured",call.=FALSE)
strategy_for <- function(source){
  z <- cfg$sources[[source]]
  if(!is.list(z))stop("Expected a search definition for ",source,call.=FALSE)
  scope <- unlist(z$search_fields,use.names=FALSE)
  if(!length(scope)||any(!nzchar(scope)))stop(source,": search_fields are required",call.=FALSE)
  db <- required_string(z$database_name,paste0(source," database_name"))
  platform <- required_string(z$platform,paste0(source," platform"))
  # Update/expansion syntax is intentionally provided verbatim by the user:
  # backend-specific date/query semantics cannot be guessed by this planner.
  mode <- z$queries[[run_type]]
  if(is.null(mode))stop(source,": no verified ",run_type," query",call.=FALSE)
  query <- required_string(mode$query,paste0(source," ",run_type," query"))
  filter <- mode$filter
  if(is.null(filter))filter <- list()
  if(!is.list(filter))stop(source,": filter must be an object",call.=FALSE)
  # The optional date bounds document the window. They do not change queries.
  if(run_type %in% c("fortnightly","monthly") &&
     is.null(mode$filter) && is.null(mode$window))
    stop(source,": update needs an explicit verified filter/window",call.=FALSE)
  list(query=query,fields=as.character(scope),database=db,platform=platform,
       filter=filter,window=mode$window,notes=mode$notes)
}
defs <- lapply(selected,strategy_for);names(defs)<-selected
queries <- lapply(defs,function(z)z$query)
# Consumers of the original script expect source_queries.json and search_plan.json.
plan <- list(
  schema="evidence-search-plan-v2",
  workflow="00",
  status="planned",
  created_at=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  run_type=run_type,
  search_version=version,
  source_queries=queries,
  source_definitions=defs,
  source_mode_support=setNames(lapply(defs,function(z)list(
      full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE)),selected),
  source_update_methods=if(run_type %in% c("fortnightly","monthly"))
    lapply(defs,function(z)list(field_scope=z$fields,
       retrieval_filter=z$filter,window_rule=z$window,
       limitation=z$notes)) else NULL,
  selected_sources=as.list(selected),
  strategy_path=config_path
)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(plan,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(out,"search_plan.json"))
writeLines(toJSON(queries,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(out,"source_queries.json"))
cat(sprintf("PASS: planned %d explicitly configured %s searches; no external requests\n",
            length(selected),run_type))
