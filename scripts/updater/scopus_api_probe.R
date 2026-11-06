#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })
or_else <- function(a,b) if (is.null(a)) b else a
split_top <- function(s, operator) {
  needle <- paste0(" ", operator, " ")
  chars <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- 0L; quoted <- FALSE; start <- 1L; i <- 1L; out <- character()
  while (i <= length(chars)) {
    c <- chars[[i]]
    if (identical(c, '"')) quoted <- !quoted
    if (!quoted) {
      if (identical(c, "(")) depth <- depth + 1L
      if (identical(c, ")")) depth <- depth - 1L
      if (depth == 0L && i + nchar(needle) - 1L <= length(chars) &&
          identical(paste(chars[i:(i+nchar(needle)-1L)], collapse=""), needle)) {
        out <- c(out, trimws(substr(s, start, i - 1L)))
        i <- i + nchar(needle)
        start <- i
        next
      }
    }
    i <- i + 1L
  }
  c(out, trimws(substr(s, start, nchar(s))))
}
unwrap <- function(s) {
  s <- trimws(s)
  repeat {
    if (!startsWith(s, "(") || !endsWith(s, ")")) return(s)
    chars <- strsplit(s, "", fixed=TRUE)[[1]]
    depth <- 0L; quoted <- FALSE; closes_early <- FALSE
    for (i in seq_along(chars)) {
      if (chars[[i]] == '"') quoted <- !quoted
      if (!quoted) {
        if (chars[[i]] == "(") depth <- depth + 1L
        if (chars[[i]] == ")") depth <- depth - 1L
        if (depth == 0L && i < length(chars)) { closes_early <- TRUE; break }
      }
    }
    if (closes_early) return(s)
    s <- trimws(substr(s, 2L, nchar(s)-1L))
  }
}
query_text <- paste(readLines("user_input/scopus_search_2015_onwards.txt", warn=FALSE, encoding="UTF-8"), collapse=" ")
query_text <- trimws(gsub("[[:space:]]+", " ", query_text))
query_text <- sub(" AND PUBYEAR > 2014$", "", query_text, fixed=FALSE)
base <- unwrap(query_text)
main <- split_top(base, "AND")
if (length(main) != 3L) stop(sprintf("Expected 3 main AND concepts; found %d", length(main)))
climate <- split_top(unwrap(main[[1L]]), "OR")
review <- unwrap(main[[2L]])
agrifood <- split_top(unwrap(main[[3L]]), "OR")
if (length(climate) != 5L || length(agrifood) != 7L) {
  stop(sprintf("Expected 5 climate and 7 agri-food groups; found %d and %d", length(climate), length(agrifood)))
}
queries <- unlist(lapply(climate, function(c) vapply(agrifood, function(a)
  paste0("(",c,") AND (",review,") AND (",a,") AND PUBYEAR > 2014"),
  character(1))), use.names=FALSE)
limit <- suppressWarnings(as.integer(Sys.getenv("SCOPUS_PROBE_LIMIT", "1")))
if (is.na(limit) || limit < 1L || limit > length(queries)) stop("Invalid probe limit")
key <- Sys.getenv("SCOPUS_API_TOKEN")
if (!nzchar(key)) stop("SCOPUS_API_TOKEN missing")
inst <- Sys.getenv("SCOPUS_INSTTOKEN")
dir.create("outputs/scopus", recursive=TRUE, showWarnings=FALSE)
results <- list()
for (i in seq_len(limit)) {
  query <- queries[[i]]
  req <- request("https://api.elsevier.com/content/search/scopus") |>
    req_headers(\`X-ELS-APIKey\`=key, Accept="application/json")
  if (nzchar(inst)) req <- req |> req_headers(\`X-ELS-Insttoken\`=inst)
  req <- req |> req_url_query(query=query, count=1, view="STANDARD") |>
    req_timeout(90) |> req_error(is_error=function(resp) FALSE)
  resp <- tryCatch(req_perform(req), error=identity)
  if (inherits(resp,"error")) {
    results[[i]] <- list(index=i, length=nchar(query), transport_error=conditionMessage(resp))
  } else {
    status <- resp_status(resp)
    body <- tryCatch(resp_body_json(resp,simplifyVector=FALSE),error=function(e) list())
    total <- body[["search-results"]][["opensearch:totalResults"]]
    results[[i]] <- list(index=i, length=nchar(query), status=status,
                         total_results=total,
                         error=or_else(body[["service-error"]],or_else(body[["error-response"]],body[["error"]])))
  }
  cat(sprintf("Subquery %d/%d: %s; matches=%s\n", i, length(queries),
    if (is.null(results[[i]]$status)) "transport error" else paste0("HTTP ",results[[i]]$status),
    or_else(results[[i]]$total_results,"unavailable")))
  if (is.null(results[[i]]$status) || results[[i]]$status >= 400 ||
      is.null(results[[i]]$total_results)) break
}
write_json(list(subqueries_total=length(queries), subqueries_attempted=length(results),
                year_min=2015, total_results_are_overlapping=TRUE,
                result=results), "outputs/scopus/search_probe.json",
           pretty=TRUE,auto_unbox=TRUE,null="null")
if (length(results)!=limit || any(vapply(results,function(z)
   is.null(z$status) || z$status >= 400 || is.null(z$total_results),logical(1)))) {
  stop("At least one Scopus subquery failed; see diagnostic artifact")
}
cat("PASS: all attempted Scopus API subqueries returned a count\n")
