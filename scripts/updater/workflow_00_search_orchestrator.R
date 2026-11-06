#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag), call.=FALSE)
  args[[i+1L]]
}

run_type <- arg("--run-type")
additional_search_term <- trimws(arg("--additional-search-term",""))
config_path <- arg("--config","user_input/workflow00_search_strategy.json")
ebsco_config_path <- arg("--ebsco-config","config/workflow00_ebsco_sources.json")
output_dir <- arg("--output-dir","outputs/updater/workflow00_plan")

if (!(run_type %in% c("full","fortnightly","monthly","expansion"))) {
  stop("--run-type must be full, fortnightly, monthly, or expansion", call.=FALSE)
}

cfg <- fromJSON(config_path, simplifyVector=TRUE)
ebsco_cfg <- fromJSON(ebsco_config_path, simplifyVector=FALSE)
if (is.null(ebsco_cfg$sources) || !length(ebsco_cfg$sources)) stop("EBSCO source catalogue contains no sources", call.=FALSE)

if (run_type=="expansion" && !nzchar(additional_search_term)) {
  stop("Expansion mode requires --additional-search-term", call.=FALSE)
}
if (run_type!="expansion" && nzchar(additional_search_term)) {
  stop("--additional-search-term is only allowed in expansion mode", call.=FALSE)
}

species_terms <- unname(cfg$species_terms)
farm_terms <- unname(cfg$farm_terms)
effective_farm_terms <- if (run_type=="expansion") additional_search_term else farm_terms

join_or <- function(x) paste(x,collapse=" OR ")
species <- join_or(species_terms)
farm <- join_or(effective_farm_terms)
old_farm <- join_or(farm_terms)

lens_block <- function(terms) {
  sprintf("(title:(%s) OR abstract:(%s) OR keyword:(%s))",terms,terms,terms)
}
lens_query <- if (run_type=="expansion") {
  sprintf("(%s AND %s AND NOT %s)", lens_block(species), lens_block(farm), lens_block(old_farm))
} else {
  sprintf("(%s AND %s)", lens_block(species), lens_block(farm))
}

scopus_query <- if (run_type=="expansion") {
  sprintf("TITLE-ABS-KEY((%s) AND (%s)) AND NOT TITLE-ABS-KEY(%s)", species, farm, old_farm)
} else {
  sprintf("TITLE-ABS-KEY((%s) AND (%s))", species, farm)
}

oa_quote <- function(x) {
  # preserve already quoted phrases, quote individual OQL terms otherwise
  x <- gsub('^"|"$','',x)
  paste0('"',x,'"')
}
oa_species <- paste(vapply(species_terms,oa_quote,character(1)),collapse=" or ")
oa_farm_terms <- if (run_type=="expansion") additional_search_term else farm_terms
oa_farm <- paste(vapply(oa_farm_terms,oa_quote,character(1)),collapse=" or ")
oa_old_farm <- paste(vapply(farm_terms,oa_quote,character(1)),collapse=" or ")
openalex_query <- if (run_type=="expansion") {
  sprintf("works where title/abstract has ((%s) and (%s) and not (%s))",oa_species,oa_farm,oa_old_farm)
} else {
  sprintf("works where title/abstract has ((%s) and (%s))",oa_species,oa_farm)
}

europe_pmc_query <- function(source_code) {
  if (run_type=="expansion") {
    sprintf("SRC:%s AND TITLE_ABS:((%s) AND (%s)) AND NOT TITLE_ABS:(%s)",source_code,species,farm,old_farm)
  } else {
    sprintf("SRC:%s AND TITLE_ABS:((%s) AND (%s))",source_code,species,farm)
  }
}
agricola_query <- europe_pmc_query("AGR")
pubmed_query <- europe_pmc_query("MED")
# For a first-time EThOS search accompanying a concept expansion, search the
# complete established strategy rather than only the new expansion concept.
ethos_query <- if (run_type=="expansion") {
  sprintf("SRC:ETH AND TITLE_ABS:((%s) AND (%s))",species,old_farm)
} else europe_pmc_query("ETH")
cba_query <- europe_pmc_query("CBA")
epmc_preprints_query <- europe_pmc_query("PPR")

wos_component <- function(tag, farm_terms_string) {
  sprintf("%s=((%s) AND (%s))",tag,species,farm_terms_string)
}
wos_new_block <- paste0(
  "(",wos_component("TI",farm),") OR (",
  wos_component("AB",farm),") OR (",
  wos_component("AK",farm),")"
)
wos_old_block <- paste0(
  "(TI=(",old_farm,")) OR ",
  "(AB=(",old_farm,")) OR ",
  "(AK=(",old_farm,"))"
)
wos_query <- if (run_type=="expansion") {
  paste0("(",wos_new_block,") NOT (",wos_old_block,")")
} else {
  wos_new_block
}

ebsco_block <- function(fields, terms) {
  paste0("(",paste(vapply(fields,function(tag) sprintf("%s (%s)",tag,terms),character(1)),collapse=" OR "),")")
}
ebsco_queries <- lapply(names(ebsco_cfg$sources),function(src_name) {
  fields <- unlist(ebsco_cfg$sources[[src_name]]$search_fields,use.names=FALSE)
  if (!length(fields)) stop(sprintf("EBSCO source %s has no configured search_fields",src_name),call.=FALSE)
  new_q <- sprintf("(%s AND %s)",ebsco_block(fields,species),ebsco_block(fields,farm))
  if (run_type=="expansion") {
    sprintf("(%s AND NOT %s)",new_q,ebsco_block(fields,old_farm))
  } else new_q
})
names(ebsco_queries) <- names(ebsco_cfg$sources)

# Fortnightly date windows are source-specific. Only fields whose day-level semantics
# have been explicitly verified are encoded here.
today <- as.Date(arg("--to-date",as.character(Sys.Date())))
if(is.na(today)) stop("Invalid --to-date",call.=FALSE)
from_date <- if(run_type=="monthly") seq(today,by="-1 month",length.out=2L)[[2L]] - 5L else today - 14L
explicit_from <- arg("--from-date","")
if(nzchar(explicit_from)){
  from_date <- as.Date(explicit_from)
  if(is.na(from_date)) stop("Invalid --from-date",call.=FALSE)
}
if(from_date > today) stop("from-date must not exceed to-date",call.=FALSE)
if (run_type %in% c("fortnightly","monthly")) {
  lens_query <- sprintf("(%s) AND created:[%s TO %s]",
                        lens_query,format(from_date,"%Y-%m-%d"),format(today,"%Y-%m-%d"))
  scopus_query <- sprintf("(%s) AND ORIG-LOAD-DATE AFT %s",
                          scopus_query,format(from_date,"%Y%m%d"))
  first_year <- as.integer(format(from_date,"%Y"))
  next_year <- as.integer(format(today,"%Y")) + 1L
  openalex_query <- sprintf("%s and year >= (%d) and year <= (%d)",
                            openalex_query,first_year,next_year)
  agricola_query <- sprintf("(%s) AND FIRST_PDATE:[%s TO %s]",
                            agricola_query,format(from_date,"%Y-%m-%d"),format(today,"%Y-%m-%d"))
  creation_window <- sprintf("CREATION_DATE:[%s TO %s]",format(from_date,"%Y-%m-%d"),format(today,"%Y-%m-%d"))
  pubmed_query <- sprintf("(%s) AND %s",pubmed_query,creation_window)
  ethos_query <- sprintf("(%s) AND %s",ethos_query,creation_window)
  cba_query <- sprintf("(%s) AND %s",cba_query,creation_window)
  epmc_preprints_query <- sprintf("(%s) AND %s",epmc_preprints_query,creation_window)
}

queries <- c(list(
  lens=lens_query,
  scopus=scopus_query,
  openalex=openalex_query,
  agricola=agricola_query,
  pubmed=pubmed_query,
  ethos=ethos_query,
  cba=cba_query,
  epmc_preprints=epmc_preprints_query,
  wos=wos_query
), ebsco_queries)

update_methods <- list(
  lens=list(
    retrieval_filter="Lens created date",
    field_scope=c("title","abstract","keyword"),
    window_rule="created date from 14 days before run date through run date",
    limitation="Uses Lens created/indexing metadata; exact native Lens ID reconciliation removes already-known records."
  ),
  scopus=list(
    retrieval_filter="Scopus ORIG-LOAD-DATE",
    field_scope=c("title","abstract","keywords"),
    window_rule="ORIG-LOAD-DATE after 14 days before run date",
    limitation="Uses Scopus load-date metadata; exact EID reconciliation removes already-known records."
  ),
  openalex=list(
    retrieval_filter="publication year",
    field_scope=c("title","abstract"),
    window_rule="current publication year through following publication year",
    limitation="Deliberate workaround because suitable OpenAlex date filtering is not used in this workflow; title/abstract-only scope prevents full-text searching. Exact OpenAlex Work ID reconciliation retains only records not previously harvested."
  ),
  agricola=list(
    retrieval_filter="Europe PMC FIRST_PDATE restricted to AGRICOLA source",
    field_scope=c("title","abstract"),
    window_rule="first publication date from 14 days before run date through run date",
    limitation="Provider/API constraint: FIRST_PDATE is a publication-date filter rather than a true indexing-date filter. Exact AGR source+ID reconciliation removes already-known records."
  ),
  pubmed=list(
    retrieval_filter="Europe PMC CREATION_DATE restricted to MED source",
    field_scope=c("title","abstract"),
    window_rule="Europe PMC database-entry date from 14 days before run date through run date",
    limitation="Exact MED source+ID reconciliation removes already-known records; bibliographic overlap with other databases is retained for Workflow 01 deduplication."
  ),
  ethos=list(
    retrieval_filter="Europe PMC CREATION_DATE restricted to ETH source",
    field_scope=c("title","abstract"),
    window_rule="Europe PMC database-entry date from 14 days before run date through run date",
    limitation="Exact ETH source+ID reconciliation removes already-known records; bibliographic overlap with other databases is retained for Workflow 01 deduplication."
  ),
  cba=list(
    retrieval_filter="Europe PMC CREATION_DATE restricted to CBA source",
    field_scope=c("title","abstract"),
    window_rule="Europe PMC database-entry date from 14 days before run date through run date",
    limitation="Exact CBA source+ID reconciliation removes already-known records; bibliographic overlap with other databases is retained for Workflow 01 deduplication."
  ),
  epmc_preprints=list(
    retrieval_filter="Europe PMC CREATION_DATE restricted to PPR source",
    field_scope=c("title","abstract"),
    window_rule="Europe PMC database-entry date from 14 days before run date through run date",
    limitation="Europe PMC preprint records only (SRC:PPR). Exact PPR source+ID reconciliation removes already-known records; published versions remain available independently for Workflow 01 deduplication."
  ),
  wos=list(
    retrieval_filter="WoS Starter modifiedTimeSpan",
    field_scope=c("title","abstract","author_keywords"),
    window_rule="modifiedTimeSpan from 14 days before run date through run date",
    limitation="Date window is passed as an API parameter rather than embedded in the query string; exact WoS UID reconciliation removes already-known records."
  )
)

support <- list(
  lens=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  scopus=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  openalex=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  agricola=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  pubmed=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  ethos=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  cba=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  epmc_preprints=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE),
  wos=list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE)
)

for (src_name in names(ebsco_cfg$sources)) {
  support[[src_name]] <- list(full=TRUE,fortnightly=TRUE,monthly=TRUE,expansion=TRUE)
  update_methods[[src_name]] <- list(
    retrieval_filter="none; complete EBSCO query is re-harvested",
    field_scope=unlist(ebsco_cfg$sources[[src_name]]$search_fields,use.names=FALSE),
    window_rule="complete query rerun; exact EBSCO accession-number reconciliation retains only unseen manifestations",
    limitation="No reliable EBSCO indexing-date filter has been verified for this profile; complete re-harvest prioritises recall and provenance over API efficiency."
  )
}

dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
plan <- list(
  workflow="00",
  status="planned",
  created_at=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  run_type=run_type,
  search_version=cfg$search_version,
  immutable_species_terms=species_terms,
  base_farm_terms=farm_terms,
  additional_search_term=if(nzchar(additional_search_term)) additional_search_term else NULL,
  effective_farm_terms=effective_farm_terms,
  expansion_policy=list(
    target="farm_terms_only",
    species_terms_mutable=FALSE,
    farm_terms_replaceable=FALSE,
    retrieval_rule=if(run_type=="expansion") "immutable species block AND new farm term AND NOT existing farm block; exact native source-ID reconciliation removes already-known manifestations before Workflow 01" else NULL
  ),
  fortnightly_window=if(run_type=="fortnightly") list(from=as.character(from_date),to=as.character(today)) else NULL,
  monthly_window=if(run_type=="monthly") list(from=as.character(from_date),to=as.character(today),overlap_days=5L) else NULL,
  source_queries=queries,
  source_update_methods=if(run_type %in% c("fortnightly","monthly")) update_methods else NULL,
  source_mode_support=support
)

writeLines(toJSON(plan,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"search_plan.json"))
writeLines(toJSON(queries,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"source_queries.json"))
message(sprintf("PASS: Workflow 00 plan created for run_type=%s",run_type))
