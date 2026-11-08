#!/usr/bin/env Rscript
# Topic-neutral W00 executor: dispatch the inherited source-specific R harvesters.
# Never executes a provider request without --execute true.
suppressPackageStartupMessages(library(jsonlite))
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k,d=NULL){i<-match(k,args);if(is.na(i))return(d);if(i==length(args))stop("Missing ",k,call.=FALSE);args[[i+1L]]}
sval <- function(x,d=""){if(is.null(x)||!length(x))return(d);as.character(x[[1L]])}
plan_path <- arg("--search-plan")
source <- arg("--source")
root <- arg("--output-root","outputs/workflow00/harvests")
execute <- identical(tolower(arg("--execute","false")),"true")
if(is.null(plan_path)||!file.exists(plan_path)||is.null(source))stop("--search-plan and --source required",call.=FALSE)
plan <- fromJSON(plan_path,simplifyVector=FALSE)
if(!identical(plan$schema,"evidence-search-plan-v2"))stop("Invalid search plan",call.=FALSE)
def <- plan$source_definitions[[source]]
if(is.null(def)||!grepl("^[a-z0-9][a-z0-9_-]*$",source))stop("Source not in plan",call.=FALSE)
query <- sval(def$query)
if(!nzchar(query))stop("Empty source query",call.=FALSE)
backend <- sval(def$backend,source)
if(startsWith(source,"ebsco_") && backend==source)backend <- "ebsco"
if(backend %in% c("pubmed","ethos","cba","epmc_preprints"))backend <- "europe_pmc"
if(!backend %in% c("scopus","openalex","agricola","wos","europe_pmc","ebsco","lens","manual_ris"))
  stop("Unsupported backend: ",backend,call.=FALSE)
options <- def$options
if(is.null(options)) options <- list()
dest <- file.path(root,source,"source_child")
cli <- c("--output-dir",dest)
script <- NULL
if(backend=="scopus"){
  script <- "workflow_00b_scopus_ingestion.R"
  cli <- c("--query",query,cli)
} else if(backend=="openalex"){
  script <- "workflow_00c_openalex_ingestion.R"
  cli <- c("--query",query,"--max-records","all",cli)
  filter <- sval(def$filter$api_filter)
  if(nzchar(filter))cli <- c(cli,"--filter",filter)
} else if(backend=="agricola"){
  script <- "workflow_00d_agricola_ingestion.R"
  cli <- c("--query",query,"--max-records","all",cli)
} else if(backend=="wos"){
  script <- "workflow_00e_wos_starter_ingestion.R"
  cli <- c("--query",query,cli)
  span <- sval(def$filter$modified_time_span)
  if(nzchar(span))cli <- c(cli,"--modified-time-span",span)
} else if(backend=="europe_pmc"){
  script <- "workflow_00f_europe_pmc_source_ingestion.R"
  code <- c(pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[source]]
  code <- sval(options$europe_pmc_source_code,sval(code))
  if(!code %in% c("MED","ETH","CBA","PPR"))stop("Invalid Europe PMC source code",call.=FALSE)
  coll <- sval(options$collection,source)
  cli <- c("--query",query,"--source-code",code,"--source-slug",source,
           "--source-collection",coll,"--max-records","all",cli)
} else if(backend=="ebsco"){
  script <- "workflow_00g_ebsco_ingestion.R"
  conf <- sval(options$ebsco_config,"config/workflow00_ebsco_sources.json")
  cli <- c("--source",source,"--query",query,"--config",conf,"--max-records","all",cli)
} else if(backend=="manual_ris"){
  script <- "workflow_00_ris_ingestion.R"
  registry <- sval(options$registry)
  files <- unlist(options$ris_files,use.names=FALSE)
  if(!nzchar(registry)||!length(files))stop("Manual RIS requires options.registry and ris_files",call.=FALSE)
  cli <- c(as.vector(rbind(rep("--ris",length(files)),files)),"--registry",registry,cli)
} else if(backend=="lens"){
  script <- "workflow_00_lens_orchestrated_search.R"
  # The inherited Lens handler accepts a complete API query object, not a generic
  # query string. No unchecked translation from Scopus or another API is performed.
  api_query <- options$lens_api_query
  if(is.null(api_query))stop("Lens backend requires options.lens_api_query",call.=FALSE)
  actual <- tryCatch(api_query$query$bool$must[[1L]]$query_string$query,error=function(e)NULL)
  if(!identical(sval(actual),query))stop("Lens API payload must match search plan's verbatim query",call.=FALSE)
  if(execute){
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    writeLines(toJSON(list(api_query=api_query),auto_unbox=TRUE,pretty=TRUE,null="null"),
               file.path(dest,"lens_search_config.json"))
  }
  cli <- c("--config",file.path(dest,"lens_search_config.json"),
    "--output",file.path(dest,"records.jsonl"),
    "--manifest",file.path(dest,"manifest.json"),
    "--audit-dir",file.path(dest,"audit"))
}
path <- file.path("scripts/updater",script)
if(!file.exists(path))stop("Missing inherited handler: ",path,call.=FALSE)
if(!execute){
  cat(sprintf("PASS DRY RUN: source=%s backend=%s handler=%s; exact query supplied, no API calls\n",
              source,backend,path))
  quit(save="no",status=0L)
}
if(backend=="manual_ris"){
  check <- c(sval(options$registry),unlist(options$ris_files,use.names=FALSE))
  if(any(!file.exists(check)))stop("Manual source inputs missing: ",paste(check[!file.exists(check)],collapse=", "))
}
dir.create(dest,recursive=TRUE,showWarnings=FALSE)
started <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
writeLines(toJSON(list(source=source,backend=backend,started_at_utc=started,
  query=query,search_version=plan$search_version),auto_unbox=TRUE,pretty=TRUE),
  file.path(dest,"execution_metadata.json"))
runner <- file.path(R.home("bin"),"Rscript")
status <- system2(runner,args=shQuote(c(path,cli)),stdout="",stderr="")
if(!identical(as.integer(status),0L))stop("W00 handler failed: ",source,call.=FALSE)
if(!file.exists(file.path(dest,"manifest.json")))stop("Handler completed without manifest: ",source,call.=FALSE)
m <- fromJSON(file.path(dest,"manifest.json"),simplifyVector=FALSE)
if(!identical(sval(m$status,"success"),"success"))stop("Handler did not complete successfully: ",source,call.=FALSE)
finished <- format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
writeLines(toJSON(list(source=source,backend=backend,started_at_utc=started,
  completed_at_utc=finished,query=query,search_version=plan$search_version),
  auto_unbox=TRUE,pretty=TRUE),file.path(dest,"execution_metadata.json"))
cat(sprintf("PASS: W00 source=%s backend=%s completed with original handler\n",source,backend))
