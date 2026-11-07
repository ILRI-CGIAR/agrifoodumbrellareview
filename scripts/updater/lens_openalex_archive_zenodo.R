#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
token<-Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token))stop("ZENODO_ACCESS_TOKEN missing: cannot safely archive bibliographic metadata")
args<-commandArgs(trailingOnly=TRUE)
i<-match("--provider",args)
provider<-if(is.na(i))stop("--provider required") else args[[i+1L]]
stopifnot(provider %in% c("lens","openalex"))
root<-file.path("outputs",provider)
report<-fromJSON(file.path(root,"dedup_summary.json"),simplifyVector=FALSE)
if(!is.finite(as.numeric(report$raw_component_hits))||as.numeric(report$raw_component_hits)<0L)stop("Invalid component hit total")
if(!is.finite(as.numeric(report$unique_source_ids))||as.numeric(report$unique_source_ids)<0L||as.numeric(report$unique_source_ids)>as.numeric(report$raw_component_hits))stop("Invalid unique source ID count")
files<-c("unique_records.jsonl","source_id_provenance.jsonl","dedup_summary.json")
for(f in files)if(!file.exists(file.path(root,f)))stop(paste("Missing",f))
archive_dir<-file.path(root,"archive");dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)
manifest<-list(schema=paste0("agrifood-",provider,"-dataset-excluded-dedup-v1"),date_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  repository=Sys.getenv("GITHUB_REPOSITORY"),github_run_id=Sys.getenv("GITHUB_RUN_ID"),
  search_from_year=2015,summary=report,
  files=lapply(files,function(f)list(filename=f,size=unname(file.info(file.path(root,f))$size),
    sha256=digest(file=file.path(root,f),algo="sha256",serialize=FALSE))))
write_json(manifest,file.path(archive_dir,"manifest.json"),pretty=TRUE,auto_unbox=TRUE,null="null")
tar_path<-file.path(normalizePath(archive_dir,mustWork=TRUE),"scholarly_2015_dataset_excluded_dedup.tar.gz")
wd<-getwd();setwd(root)
utils::tar(normalizePath(tar_path,mustWork=FALSE),files=files,compression="gzip",tar="internal")
setwd(wd)
if(!file.exists(tar_path)||file.info(tar_path)$size<=0L)stop("Archive failed")
api<-"https://zenodo.org/api/deposit/depositions"
auth<-function(r) r |> req_headers(Authorization=paste("Bearer",token))
perform<-function(r,accepted,label){
  resp<-r |> req_timeout(600) |> req_error(is_error=function(resp)FALSE) |> req_perform()
  status<-resp_status(resp)
  if(!(status %in% accepted))stop(sprintf("Zenodo %s HTTP %d: %s",label,status,
    substr(tryCatch(resp_body_string(resp),error=function(e)""),1,700)))
  resp_body_json(resp,simplifyVector=FALSE)
}
created<-perform(auth(request(api) |> req_method("POST") |> req_body_raw(charToRaw("{}"),type="application/json")) ,201L,"draft creation")
id<-as.character(created$id);bucket<-as.character(created$links$bucket)
metadata<-list(metadata=list(
  title=sprintf("Agri-food umbrella review: %s 2015+ scholarly records excluding datasets | GitHub run %s",provider,Sys.getenv("GITHUB_RUN_ID")),
  upload_type="dataset",publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=sprintf("<p>Restricted scholarly bibliographic metadata from %s (2015 onwards, with dataset-type works excluded at the API level). Includes internal source-ID deduplication, partition provenance and checksums. Title, abstract and keywords were searched. Query translation limitations are recorded in the manifest. No public redistribution of bulk source metadata is authorised.</p>",provider),
  creators=list(list(name="Haddaway, Neal")),
  access_right="restricted",
  access_conditions="Bibliographic metadata are subject to bibliographic source licensing restrictions and are retained for authorised research purposes only.",
  keywords=list("agri-food","climate change","systematic review","scholarly bibliographic","deduplication")
))
perform(auth(request(paste0(api,"/",id)) |> req_method("PUT") |> req_body_json(metadata,auto_unbox=TRUE)),200L,"metadata update")
for(path in c(tar_path,file.path(archive_dir,"manifest.json"))){
  filename<-basename(path);success<-FALSE
  for(attempt in 1:4){
    resp<-auth(request(paste0(bucket,"/",URLencode(filename,reserved=TRUE))) |> req_method("PUT") |>
      req_body_file(path) |> req_headers(Expect="") |> req_timeout(1200) |>
      req_error(is_error=function(resp)FALSE)) |> req_perform()
    status<-resp_status(resp)
    if(status %in% c(200L,201L)){success<-TRUE;break}
    if(!(status %in% c(429L,500L,502L,503L,504L)))stop(sprintf("Zenodo upload HTTP %d",status))
    Sys.sleep(2^attempt)
  }
  if(!success)stop(paste("Zenodo upload failed:",filename))
}
published<-perform(auth(request(paste0(api,"/",id,"/actions/publish")) |> req_method("POST")),c(200L,201L,202L),"publish")
record_id<-as.character(if(is.null(published$record_id))published$id else published$record_id)
receipt<-list(status="published",visibility="restricted",zenodo_record_id=record_id,github_run_id=Sys.getenv("GITHUB_RUN_ID"),
  raw_component_hits=report$raw_component_hits,unique_source_ids=report$unique_source_ids,
  archive_sha256=digest(file=tar_path,algo="sha256"))
write_json(receipt,file.path(root,"zenodo_receipt.json"),pretty=TRUE,auto_unbox=TRUE)
cat(sprintf("PASS restricted Zenodo record %s: %d raw occurrences, %d unique source record IDs\n",record_id,report$raw_component_hits,report$unique_source_ids))
