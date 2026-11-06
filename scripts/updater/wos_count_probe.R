#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite)})
source_file <- "user_input/scopus_search_2015_onwards.txt"
raw <- paste(readLines(source_file,warn=FALSE,encoding="UTF-8"),collapse=" ")
raw <- gsub("[[:space:]]+"," ",trimws(raw))
if(!grepl(" AND PUBYEAR > 2014$",raw)) stop("Unexpected Scopus year filter")
raw <- sub(" AND PUBYEAR > 2014$","",raw)
raw <- gsub("\\\\[*]","*",raw)
if(grepl("\\\\[*]",raw))stop("Escaped wildcards remain")
raw <- gsub("TITLE-ABS-KEY[[:space:]]*\\(","TS=(",raw)
raw <- gsub("[[:space:]]+W/([0-9]+)[[:space:]]+"," NEAR/\\1 ",raw)
if(grepl("TITLE-ABS-KEY|[[:space:]]W/[0-9]+",raw))stop("Scopus-specific operator remains")
q <- paste0("(",raw,") AND PY=(2015-2026)")
chars <- strsplit(q,"",fixed=TRUE)[[1L]]
depth<-0L;min_depth<-0L;quoted<-FALSE
for(ch in chars){
  if(ch=='"')quoted<-!quoted
  if(!quoted&&ch=="(")depth<-depth+1L
  if(!quoted&&ch==")"){depth<-depth-1L;min_depth<-min(min_depth,depth)}
}
if(quoted||depth!=0L||min_depth<0L)stop("Unbalanced WoS query")
dir.create("outputs/wos",recursive=TRUE,showWarnings=FALSE)
writeLines(q,"outputs/wos/translated_query.txt",useBytes=TRUE)
key<-Sys.getenv("WOS_STARTER_API")
if(!nzchar(key)) {
  write_json(list(status="missing_secret",name="WOS_STARTER_API",query_characters=nchar(q)),
             "outputs/wos/count_probe.json",pretty=TRUE,auto_unbox=TRUE)
  stop("WOS_STARTER_API is missing in this repository; no requests made")
}
api <- "https://api.clarivate.com/apis/wos-starter/v1/documents"
do_query <- function(label,query){
  req<-request(api) |> req_headers("X-ApiKey"=key,Accept="application/json") |>
    req_url_query(db="WOS",q=query,limit=1,page=1) |>
    req_timeout(120) |> req_error(is_error=function(resp)FALSE)
  response<-tryCatch(req_perform(req),error=identity)
  if(inherits(response,"error"))return(list(label=label,error=conditionMessage(response)))
  status<-resp_status(response)
  body<-tryCatch(resp_body_json(response,simplifyVector=FALSE),error=function(e)NULL)
  result<-list(label=label,http=status,query_length=nchar(query),
               total=body$metadata$total,
               error=if(status>=400L)substr(tryCatch(resp_body_string(response),error=function(e)""),1L,1800L)else NULL)
  cat(sprintf("%s | HTTP %s | total %s | %d characters\n",label,status,
              if(is.null(result$total))"NA" else as.character(result$total),nchar(query)))
  result
}
checks<-list(
  smoke=do_query("smoke", 'TS=("climate change") AND PY=(2015-2026)'),
  concept=do_query("concept", 'TS=("climate change") AND TS=("systematic review") AND TS=(agricultur*) AND PY=(2015-2026)'))
if(any(vapply(checks,function(x)is.null(x$http)||x$http!=200L,logical(1)))){
  write_json(list(status="basic_query_failed",results=checks),"outputs/wos/count_probe.json",pretty=TRUE,auto_unbox=TRUE,null="null")
  stop("WoS basic syntax or API authentication test failed; full search not attempted")
}
checks$full<-do_query("full",q)
write_json(list(status=if(isTRUE(checks$full$http==200L)&&!is.null(checks$full$total))"success"else"full_query_failed",
                note="WoS TS covers Title, Abstract, Author Keywords and Keywords Plus. Scopus W/n converted to WoS NEAR/n (unordered), so syntax is not perfectly identical.",
                results=checks),"outputs/wos/count_probe.json",pretty=TRUE,auto_unbox=TRUE,null="null")
if(is.null(checks$full$http)||checks$full$http!=200L||is.null(checks$full$total))stop("Full Web of Science API query not accepted")
cat(sprintf("PASS: Full WoS count-only search, hits=%s\n",checks$full$total))
