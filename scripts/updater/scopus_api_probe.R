#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite)})
`%||%` <- function(a,b) if (is.null(a)) b else a
query_path <- "user_input/scopus_search_2015_onwards.txt"
stopifnot(file.exists(query_path))
query <- trimws(paste(readLines(query_path, warn = FALSE, encoding = "UTF-8"), collapse = " "))
if (!nzchar(query)) stop("Scopus query file is empty")
key <- Sys.getenv("SCOPUS_API_TOKEN")
if (!nzchar(key)) stop("SCOPUS_API_TOKEN is not configured in this repository. No API request attempted.")
inst <- Sys.getenv("SCOPUS_INSTTOKEN")
req <- request("https://api.elsevier.com/content/search/scopus") |>
  req_headers(`X-ELS-APIKey` = key, Accept = "application/json")
if (nzchar(inst)) req <- req |> req_headers(`X-ELS-Insttoken` = inst)
req <- req |> req_url_query(query = query, count = 1, view = "STANDARD") |> req_timeout(120)
res <- req_perform(req, error = FALSE)
status <- resp_status(res)
body <- tryCatch(resp_body_json(res, simplifyVector = FALSE), error = function(e) list())
total <- body[["search-results"]][["opensearch:totalResults"]]
errors <- body[["service-error"]] %||% body[["error-response"]]
report <- list(status = status, query_characters = nchar(query), year_min = 2015,
               total_results = total, error = errors)
dir.create("outputs/scopus", recursive = TRUE, showWarnings = FALSE)
write_json(report, "outputs/scopus/search_probe.json", pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(sprintf("HTTP %s, Scopus total results: %sn", status, if (is.null(total)) "unavailable" else as.character(total)))
if (status >= 400) stop(sprintf("Scopus API returned HTTP %s; inspect probe artifact for details", status))
if (is.null(total)) stop("Scopus response has no total-results field")
`%||%` <- function(a,b) if (is.null(a)) b else a
