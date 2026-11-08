#!/usr/bin/env Rscript
# Single topic-independent W00 backend dispatcher. Uses the original ingestion
# scripts, never translates or rewrites a database query.
suppressPackageStartupMessages({library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,d=NULL){i<-match(k,args);if(is.na(i))return(d);if(i==length(args))stop("Missing ",k);args[[i+1L]]}
plan_file<-arg("--plan");root<-arg("--output-root")
mode<-arg("--mode","fetch")
if(is.null(plan_file)||is.null(root)||!mode %in% c("fetch","prepared"))
  stop("Required --plan, --output-root and --mode fetch|prepared",call.=FALSE)
if(!file.exists(plan_file))stop("Search plan absent: ",plan_file,call.=FALSE)
p<-fromJSON(plan_file,simplifyVector=FALSE)
if(!identical(p$schema,"evidence-search-plan-v2"))stop("Invalid search plan schema",call.=FALSE)
sources<-names(p$source_definitions)
if(!length(sources)||anyDuplicated(sources)||!identical(sources,names(p$source_queries)))
  stop("Invalid planned sources",call.=FALSE)
`%||%`<-function(a,b)if(is.null(a))b else a
scalar<-function(x,default="")if(is.null(x)||!length(x))default else as.character(x[[1L]])
read_lines<-function(f){if(!file.exists(f))return(character());trimws(readLines(f,warn=FALSE,encoding="UTF-8"))}
sha<-function(f)digest(file=f,algo="sha256",serialize=FALSE)
run<-function(script,params){
  if(!file.exists(script))stop("Missing backend: ",script,call.=FALSE)
  cmd<-c(shQuote(script),unlist(lapply(names(params),function(k){
    v<-params[[k]]
    if(is.null(v)||!length(v))return(character())
    c(shQuote(k),shQuote(as.character(v)))
  }),use.names=FALSE))
  status<-system2(file.path(R.home("bin"),"Rscript"),args=cmd)
  if(!identical(as.integer(status),0L))stop("Backend failed: ",script,call.=FALSE)
}
run_receipt<-function(params)run("scripts/updater/write_workflow00_search_record.R",params)
for(source in sources){
  d<-p$source_definitions[[source]]
  backend<-scalar(d$backend,source)
  query<-scalar(d$query)
  if(!nzchar(query))stop("Empty query: ",source,call.=FALSE)
  opt<-d$options
  if(is.null(opt))opt<-list()
  filter<-d$filter
  if(is.null(filter))filter<-list()
  directory<-file.path(root,"harvests",source)
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  manifest<-file.path(directory,"manifest.json")
  if(mode=="fetch"){
    if(backend=="scopus"){
      run("scripts/updater/workflow_00b_scopus_ingestion.R",
          list("--query"=query,"--max-records"=NULL,
               "--output-dir"=directory))
    }else if(backend=="openalex"){
      run("scripts/updater/workflow_00c_openalex_ingestion.R",
          list("--query"=query,"--filter"=scalar(opt$api_filter),
               "--max-records"="all","--output-dir"=directory))
    }else if(backend=="agricola"){
      run("scripts/updater/workflow_00d_agricola_ingestion.R",
          list("--query"=query,"--max-records"="all","--output-dir"=directory))
    }else if(backend=="wos"){
      span<-scalar(filter$modifiedTimeSpan,scalar(opt$modified_time_span))
      run("scripts/updater/workflow_00e_wos_starter_ingestion.R",
          list("--query"=query,"--modified-time-span"=span,
               "--output-dir"=directory))
    }else if(backend %in% c("pubmed","ethos","cba","epmc_preprints")){
      code<-c(pubmed="MED",ethos="ETH",cba="CBA",epmc_preprints="PPR")[[backend]]
      run("scripts/updater/workflow_00f_europe_pmc_source_ingestion.R",
          list("--query"=query,"--source-code"=code,"--source-slug"=source,
               "--source-collection"=scalar(d$database), "--max-records"="all",
               "--output-dir"=directory))
    }else if(backend=="lens"){
      cfg<-file.path(directory,"lens_api_config.json")
      # The payload is explicit and provider-specific, not derived from text.
      payload<-opt$lens_query_payload
      if(is.null(payload)||is.null(payload$query$bool$must[[1L]]$query_string$query) ||
         !identical(as.character(payload$query$bool$must[[1L]]$query_string$query),query))
        stop("Lens requires matching explicit options.lens_query_payload",call.=FALSE)
      writeLines(toJSON(list(api_query=payload),auto_unbox=TRUE,pretty=TRUE,null="null"),cfg)
      run("scripts/updater/workflow_00_lens_orchestrated_search.R",
          list("--config"=cfg,"--output"=file.path(directory,"records.jsonl"),
               "--manifest"=manifest,"--audit-dir"=file.path(directory,"audit")))
    }else if(backend=="ebsco" || startsWith(backend,"ebsco_")){
      run("scripts/updater/workflow_00g_ebsco_ingestion.R",
          list("--source"=source,"--query"=query,"--output-dir"=directory))
    }else if(backend=="manual_ris"){
      paths<-unlist(opt$ris_files,use.names=FALSE)
      registry<-scalar(opt$registry)
      if(!length(paths)||!file.exists(registry)||any(!file.exists(paths)))
        stop("Manual RIS requires staged file paths and registry: ",source,call.=FALSE)
      params<-list("--registry"=registry,"--output-dir"=directory)
      extra<-unlist(lapply(paths,function(path)c("--ris"=path)),use.names=FALSE)
      cmd<-c(shQuote("scripts/updater/workflow_00_ris_ingestion.R"),
             shQuote("--registry"),shQuote(registry),shQuote("--output-dir"),shQuote(directory),
             unlist(lapply(paths,function(path)c(shQuote("--ris"),shQuote(path))),use.names=FALSE))
      status<-system2(file.path(R.home("bin"),"Rscript"),args=cmd)
      if(status!=0L)stop("Manual RIS ingestion failed: ",source,call.=FALSE)
    }else stop("Unsupported backend: ",backend,call.=FALSE)
  }
  if(!file.exists(manifest))stop("Missing staged backend manifest for ",source,call.=FALSE)
  x<-fromJSON(manifest,simplifyVector=FALSE)
  if(!identical(scalar(x$status),"success"))
    stop("Backend manifest not successful: ",source,call.=FALSE)
  recorded_query<-scalar(x$query,scalar(x$base_query,scalar(x$query_string)))
  if(nzchar(recorded_query)&&!identical(recorded_query,query))
    stop("Backend executed query mismatch: ",source,call.=FALSE)
  expected<-suppressWarnings(as.integer(x$reported_total %||% x$unpartitioned_total %||% x$total_results_reported_first_page %||%
     x$total_results_reported_by_openalex_first_page %||% x$reported_search_results %||%
     x$lens_reported_total %||% x$records$reported_search_results %||% x$search$reported_results))
  retrieved<-suppressWarnings(as.integer(x$records_retrieved %||% x$records$unique_records_for_handover %||% x$entries_retrieved %||%
     x$unique_records_written %||% x$raw_records_retrieved %||%
     x$records_retrieved %||% x$records_parsed %||% x$validation$unique_records))
  if(length(expected)!=1L||is.na(expected)||length(retrieved)!=1L||is.na(retrieved)||
     expected<0L||retrieved<0L||retrieved>expected)
    stop("Backend manifest lacks valid hit/download counts: ",source,call.=FALSE)
  if(mode=="fetch"){
    dir.create(file.path(root,"search_records"),recursive=TRUE,showWarnings=FALSE)
    launched<-scalar(x$started_at,scalar(x$created_at,scalar(x$search_started_at)))
    finished<-scalar(x$completed_at,scalar(x$generated_at_utc))
    if(!nzchar(launched))launched<-format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
    if(!nzchar(finished))finished<-format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
    run_receipt(list("--source"=source,"--run-type"=p$run_type,
      "--search-plan"=plan_file,"--reported-results"=as.character(expected),
      "--downloaded-results"=as.character(retrieved),
      "--parent-run-id"=Sys.getenv("GITHUB_RUN_ID","local"),
      "--child-run-id"=Sys.getenv("GITHUB_RUN_ID","local"),
      "--searched-at-utc"=launched,"--exported-at-utc"=finished,
      "--output-root"=file.path(root,"search_records")))
  }
  cat(sprintf("PASS W00 source=%s backend=%s reported=%d retrieved=%d sha256=%s\n",
              source,backend,expected,retrieved,sha(manifest)))
}
cat(sprintf("PASS W00: %d selected source backends; mode=%s\n",length(sources),mode))
