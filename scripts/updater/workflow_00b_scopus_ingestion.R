#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag))
  args[[i + 1L]]
}

base_query <- arg("--query")
page_size <- as.integer(arg("--page-size", "200"))
view <- toupper(arg("--view", "STANDARD"))
output_dir <- arg("--output-dir", "outputs/updater/scopus_full_search")
base_url <- arg("--base-url", "https://api.elsevier.com/content/search/scopus")

if (is.null(base_query) || !nzchar(trimws(base_query))) stop("--query is required")
if (is.na(page_size) || page_size < 1L) stop("--page-size must be >= 1")
if (view == "STANDARD" && page_size > 200L) stop("--page-size cannot exceed 200 for STANDARD")
if (view != "STANDARD" && page_size > 25L) stop("--page-size cannot exceed 25 for COMPLETE/other restricted views")
if (!(view %in% c("STANDARD","COMPLETE"))) stop("--view must be STANDARD or COMPLETE")

api_key <- trimws(Sys.getenv("SCOPUS_API_TOKEN", unset=""))
inst_token <- trimws(Sys.getenv("SCOPUS_INSTTOKEN", unset=""))
if (!nzchar(api_key)) stop("SCOPUS_API_TOKEN is required")
if (!nzchar(inst_token)) stop("SCOPUS_INSTTOKEN is required")

`%||%` <- function(x,y) if (is.null(x)) y else x
now_utc <- function() format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ")
scalar_text <- function(x, default=NA_character_) {
  if (is.null(x) || length(x)==0L) return(default)
  y <- as.character(x[[1L]])
  if (!nzchar(y)) default else y
}
scalar_int <- function(x, default=NA_integer_) {
  y <- suppressWarnings(as.integer(scalar_text(x, NA_character_)))
  if (is.na(y)) default else y
}

root <- normalizePath(output_dir, mustWork=FALSE)
raw_dir <- file.path(root,"raw")
headers_dir <- file.path(root,"headers")
dir.create(raw_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(headers_dir, recursive=TRUE, showWarnings=FALSE)
manifest_path <- file.path(root,"manifest.json")
checkpoint_path <- file.path(root,"checkpoint.json")
validation_path <- file.path(root,"validation.json")
partitions_path <- file.path(root,"partitions.csv")

write_json <- function(x,path) {
  tmp <- paste0(path,".tmp")
  writeLines(toJSON(x, auto_unbox=TRUE, pretty=TRUE, null="null", na="null", digits=NA), tmp, useBytes=TRUE)
  if (!file.rename(tmp,path)) stop(sprintf("Could not atomically write %s",path))
}

request_page <- function(query,start,count) {
  last_error <- NULL
  for (attempt in seq_len(6L)) {
    req <- request(base_url) |>
      req_headers(
        Accept="application/json",
        `X-ELS-APIKey`=api_key,
        `X-ELS-Insttoken`=inst_token,
        `User-Agent`="LivingEvidenceMap Scopus ingestion"
      ) |>
      req_url_query(
        query=query,
        start=start,
        count=count,
        view=view,
        sort="+coverDate,+creator,+publicationName",
        suppressNavLinks="false"
      ) |>
      req_error(is_error=function(resp) FALSE)
    resp <- tryCatch(req_perform(req), error=identity)
    if (!inherits(resp,"error")) {
      status <- resp_status(resp)
      if (status < 400L) return(resp)
      body <- tryCatch(resp_body_string(resp), error=function(e) "")
      last_error <- sprintf("Scopus HTTP %d: %s",status,substr(body,1L,1200L))
      retryable <- status==429L || status>=500L
      if (!retryable) stop(last_error)
    } else last_error <- conditionMessage(resp)
    if (attempt<6L) Sys.sleep(min(60,2^(attempt-1L)))
  }
  stop(sprintf("Scopus request failed after 6 attempts: %s",last_error))
}

count_query <- function(query) {
  resp <- request_page(query,0L,1L)
  parsed <- fromJSON(resp_body_string(resp), simplifyVector=FALSE)
  sr <- parsed[["search-results"]]
  if (is.null(sr)) stop("Missing search-results while counting partition")
  scalar_int(sr[["opensearch:totalResults"]])
}

started_at <- now_utc()
overall_total <- count_query(base_query)
if (is.na(overall_total)) stop("Could not determine unpartitioned Scopus total")

# Use direct offset pagination when the query result is within Scopus's 5,000-result
# offset ceiling. This avoids dozens of unnecessary year-count requests for small
# incremental LOAD-DATE updates. Larger/full searches retain mutually exclusive
# PUBYEAR partitioning.
if (overall_total <= 5000L) {
  parts <- data.frame(
    partition_id="all_results",
    year=NA_integer_,
    query=base_query,
    expected_n=overall_total,
    stringsAsFactors=FALSE
  )
  pagination_mode <- "direct_start_count"
} else {
  current_year <- as.integer(format(Sys.Date(), "%Y")) + 1L
  year_counts <- list()
  for (yr in seq(1900L,current_year)) {
    q <- sprintf("(%s) AND PUBYEAR = %d", base_query, yr)
    n <- count_query(q)
    if (!is.na(n) && n > 0L) {
      if (n > 5000L) stop(sprintf("Year %d alone has %d results, exceeding offset ceiling; additional partitioning required",yr,n))
      year_counts[[length(year_counts)+1L]] <- data.frame(
        partition_id=sprintf("year_%04d",yr),
        year=yr,
        query=q,
        expected_n=n,
        stringsAsFactors=FALSE
      )
    }
  }

  parts <- do.call(rbind,year_counts)
  if (is.null(parts) || nrow(parts)==0L) stop("No year partitions found")

  sum_year <- sum(parts$expected_n)
  residual_n <- overall_total - sum_year
  if (residual_n < 0L) stop(sprintf("Year partition counts (%d) exceed unpartitioned total (%d)",sum_year,overall_total))
  if (residual_n > 0L) {
    stop(sprintf("Year partitions sum to %d but unpartitioned total is %d; %d records are not covered by PUBYEAR partitions",sum_year,overall_total,residual_n))
  }
  pagination_mode <- "year_partitioned_start_count"
}

write.csv(parts,partitions_path,row.names=FALSE)

manifest <- list(
  workflow="00b_scopus_ingestion",
  implementation_language="R",
  status="running",
  started_at=started_at,
  endpoint=base_url,
  base_query=base_query,
  view=view,
  pagination_mode=pagination_mode,
  sort="+coverDate,+creator,+publicationName",
  unpartitioned_total=overall_total,
  partitions=nrow(parts),
  requested_page_size=page_size,
  raw_response_preservation=TRUE,
  checkpoint_after_every_page=TRUE,
  canonicalisation_performed=FALSE,
  canonical_json_modified=FALSE,
  downstream_processing_performed=FALSE
)
write_json(manifest,manifest_path)

page_global <- 0L
retrieved <- 0L
page_summaries <- list()

for (pi in seq_len(nrow(parts))) {
  pq <- parts$query[[pi]]
  expected <- parts$expected_n[[pi]]
  start <- 0L
  partition_retrieved <- 0L

  while (partition_retrieved < expected) {
    requested_count <- min(page_size, expected-partition_retrieved)
    page_global <- page_global + 1L
    message(sprintf("Partition %s page %d: start=%d count=%d",parts$partition_id[[pi]],page_global,start,requested_count))
    resp <- request_page(pq,start,requested_count)
    body_text <- resp_body_string(resp)
    headers <- resp_headers(resp)
    parsed <- fromJSON(body_text,simplifyVector=FALSE)
    sr <- parsed[["search-results"]]
    entries <- sr[["entry"]] %||% list()
    n_entries <- length(entries)
    if (n_entries==0L) stop(sprintf("Unexpected zero-result page in %s at start=%d",parts$partition_id[[pi]],start))

    raw_path <- file.path(raw_dir,sprintf("response_%06d.json",page_global))
    con <- file(raw_path,"wb"); writeBin(charToRaw(body_text),con); close(con)
    header_path <- file.path(headers_dir,sprintf("response_%06d_headers.json",page_global))
    write_json(as.list(unclass(headers)),header_path)

    page_summaries[[length(page_summaries)+1L]] <- list(
      global_page=page_global,
      partition_id=parts$partition_id[[pi]],
      year=parts$year[[pi]],
      start=start,
      requested_count=requested_count,
      returned_entries=n_entries,
      expected_partition_total=expected,
      request_id=scalar_text(headers[["x-els-reqid"]],NULL),
      rate_limit_remaining=scalar_text(headers[["x-ratelimit-remaining"]],NULL),
      raw_file=file.path("raw",basename(raw_path))
    )

    partition_retrieved <- partition_retrieved+n_entries
    retrieved <- retrieved+n_entries
    start <- start+n_entries

    write_json(list(
      workflow="00b_scopus_ingestion",
      status="running",
      updated_at=now_utc(),
      overall_total=overall_total,
      current_partition=parts$partition_id[[pi]],
      current_partition_expected=expected,
      current_partition_retrieved=partition_retrieved,
      partitions_completed=pi-1L + as.integer(partition_retrieved>=expected),
      pages_completed=page_global,
      entries_retrieved=retrieved,
      canonical_json_modified=FALSE
    ),checkpoint_path)
  }

  if (partition_retrieved != expected) stop(sprintf("Partition %s expected %d but harvested %d",parts$partition_id[[pi]],expected,partition_retrieved))
}

raw_files <- sort(list.files(raw_dir,pattern="^response_[0-9]{6}\\.json$",full.names=TRUE))
eids <- character()
scopus_ids <- character()
recount <- 0L
for (rf in raw_files) {
  x <- fromJSON(rf,simplifyVector=FALSE)
  entries <- x[["search-results"]][["entry"]] %||% list()
  recount <- recount+length(entries)
  for (entry in entries) {
    eid <- scalar_text(entry[["eid"]],"")
    sid <- sub("^SCOPUS_ID:","",scalar_text(entry[["dc:identifier"]],""))
    if (!nzchar(eid)) stop(sprintf("Missing Scopus EID in %s",basename(rf)))
    eids <- c(eids,eid)
    if (nzchar(sid)) scopus_ids <- c(scopus_ids,sid)
  }
}

dup_eids <- unique(eids[duplicated(eids)])
dup_sids <- unique(scopus_ids[duplicated(scopus_ids)])
validation <- list(
  workflow="00b_scopus_ingestion",
  validated_at=now_utc(),
  unpartitioned_total=overall_total,
  partition_expected_total=sum(parts$expected_n),
  records_recounted_from_raw=recount,
  unique_scopus_eids=length(unique(eids)),
  duplicate_scopus_eids_n=length(dup_eids),
  duplicate_scopus_ids_n=length(dup_sids),
  count_matches_unpartitioned_total=identical(recount,overall_total),
  unique_eids_match_unpartitioned_total=identical(length(unique(eids)),overall_total),
  canonical_json_modified=FALSE
)
write_json(validation,validation_path)

if (length(dup_eids)>0L) stop(sprintf("Validation failure: %d duplicate Scopus EIDs; offset pagination is not stable enough under the configured sort",length(dup_eids)))
if (length(unique(eids)) != overall_total) stop(sprintf("Validation failure: only %d unique Scopus EIDs for unpartitioned total %d",length(unique(eids)),overall_total))
if (length(dup_sids)>0L) stop(sprintf("Validation failure: %d duplicate Scopus IDs",length(dup_sids)))
if (!identical(recount,overall_total)) stop(sprintf("Validation failure: harvested %d but unpartitioned total is %d",recount,overall_total))

manifest$status <- "success"
manifest$completed_at <- now_utc()
manifest$entries_retrieved <- recount
manifest$pages_retrieved <- page_global
manifest$validation <- validation
manifest$pages <- page_summaries
manifest$output <- list(
  raw_responses="raw/response_*.json",
  response_headers="headers/response_*_headers.json",
  partitions="partitions.csv",
  checkpoint="checkpoint.json",
  validation="validation.json",
  manifest="manifest.json"
)
write_json(manifest,manifest_path)
write_json(list(
  workflow="00b_scopus_ingestion",
  status="success",
  completed_at=now_utc(),
  entries_retrieved=recount,
  pages_completed=page_global,
  canonical_json_modified=FALSE
),checkpoint_path)

message(sprintf("PASS: Scopus harvest complete; %d records across %d pages (%s).",recount,page_global,pagination_mode))
