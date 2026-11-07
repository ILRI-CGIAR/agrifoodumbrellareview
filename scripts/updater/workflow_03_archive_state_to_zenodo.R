#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})

zenodo_config <- jsonlite::fromJSON("user_input/zenodo_config.json",simplifyVector=FALSE)
zenodo_creators <- zenodo_config$creators
zenodo_access_right <- as.character(zenodo_config$access_right)
if(is.null(zenodo_creators)||!length(zenodo_creators)||any(!vapply(zenodo_creators,function(z)!is.null(z$name)&&length(z$name)==1L&&nzchar(trimws(as.character(z$name))),logical(1))))stop("Configure at least one Zenodo creator",call.=FALSE)
if(length(zenodo_access_right)!=1L||!identical(zenodo_access_right,"restricted"))stop("W00-W08 archives require restricted Zenodo deposits",call.=FALSE)
args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
state_dir<-normalizePath(arg("--state-dir"),mustWork=TRUE)
source_run_id<-arg("--source-run-id")
publication_run_id<-arg("--publication-run-id")
repository<-arg("--repository")
output_dir<-arg("--output-dir")
upstream_sha<-tolower(arg("--upstream-lean-sha256",""))
expected_records<-suppressWarnings(as.integer(arg("--expected-records","")))
if(any(vapply(list(source_run_id,publication_run_id,repository,output_dir),is.null,logical(1)))) stop("Required: --state-dir --source-run-id --publication-run-id --repository --output-dir --upstream-lean-sha256 --expected-records",call.=FALSE)
if(!nzchar(upstream_sha)) stop("--upstream-lean-sha256 required",call.=FALSE)
if(is.na(expected_records)||expected_records<1L) stop("--expected-records must be a positive integer",call.=FALSE)

status_path<-file.path(state_dir,"publication_status.jsonl")
report_path<-file.path(state_dir,"report.json")
audit_path<-file.path(state_dir,"api_audit.jsonl")
if(!file.exists(status_path)||!file.exists(report_path)) stop("Workflow 03 state missing publication_status.jsonl or report.json",call.=FALSE)
r<-fromJSON(report_path,simplifyVector=FALSE)
if(!identical(r$status,"PASS")||as.integer(r$records_scanned)!=expected_records) stop("Workflow 03 report is not a validated full run for the expected input population",call.=FALSE)
if(as.integer(r$evidence_conflicts)!=0L) stop("Workflow 03 state contains unresolved evidence conflicts",call.=FALSE)

lines<-readLines(status_path,warn=FALSE,encoding="UTF-8");lines<-lines[nzchar(trimws(lines))]
if(length(lines)!=expected_records) stop(sprintf("Workflow 03 sparse layer must contain %d records",expected_records),call.=FALSE)
ids<-vapply(lines,function(z)as.character(fromJSON(z,simplifyVector=FALSE)$record_id),character(1))
if(any(!nzchar(ids))||anyDuplicated(ids)) stop("Workflow 03 sparse layer has missing/duplicate record_id",call.=FALSE)
layer_sha<-digest(file=status_path,algo="sha256",serialize=FALSE)

token<-Sys.getenv("ZENODO_ACCESS_TOKEN");if(!nzchar(token)) stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE);output_dir<-normalizePath(output_dir,mustWork=TRUE)
archive_dir<-file.path(output_dir,"archive_files");dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)

all_paths<-list.files(state_dir,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if(length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))
archive_name<-sprintf("LivingEvidenceMap_workflow03_run-%s_state.tar.gz",source_run_id)
archive_path<-file.path(archive_dir,archive_name)
old<-setwd(dirname(state_dir));on.exit(setwd(old),add=TRUE)
utils::tar(archive_path,files=basename(state_dir),compression="gzip",tar="internal")
setwd(old);on.exit(NULL,add=FALSE)
if(!file.exists(archive_path)||file.info(archive_path)$size<=0) stop("Failed to create Workflow 03 archive",call.=FALSE)

manifest<-list(
  schema="living-evidence-map-workflow03-state-archive-v1",
  workflow="03",
  state="publication_status",
  source_github_run_id=as.character(source_run_id),
  publication_github_run_id=as.character(publication_run_id),
  source_github_run_url=sprintf("https://github.com/%s/actions/runs/%s",repository,source_run_id),
  repository=repository,
  upstream_lean_canonical_sha256=upstream_sha,
  publication_status_sha256=layer_sha,
  records=length(lines),
  status_counts=r$code_counts,
  exclude_from_workflow04=as.integer(r$exclude_from_workflow04),
  evidence_conflicts=as.integer(r$evidence_conflicts),
  multiple_positive_signals=as.integer(r$multiple_positive_signals),
  openalex_lookup=r$openalex_lookup,
  checked_at=r$checked_at,
  file_visibility="restricted",
  files=list(state_archive=list(filename=archive_name,bytes=unname(file.info(archive_path)$size),sha256=digest(file=archive_path,algo="sha256",serialize=FALSE)))
)
manifest_name<-sprintf("LivingEvidenceMap_workflow03_run-%s_manifest.json",source_run_id)
manifest_path<-file.path(archive_dir,manifest_name)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),manifest_path,useBytes=TRUE)

api<-"https://zenodo.org/api/deposit/depositions"
auth<-function(req) req|>req_headers(Authorization=paste("Bearer",token))
perform<-function(req,expected,label,timeout=600){
  resp<-req|>req_timeout(timeout)|>req_error(is_error=function(resp)FALSE)|>req_perform()
  st<-resp_status(resp)
  if(!(st%in%expected)){body<-tryCatch(resp_body_string(resp),error=function(e)"");stop(sprintf("Zenodo %s HTTP %d: %s",label,st,body),call.=FALSE)}
  resp
}
metadata<-list(metadata=list(
  title=sprintf("Living Evidence Map Workflow 03 publication-status state | run %s",source_run_id),
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=paste0("<p>Sparse publication-status surveillance state for Living Evidence Map Workflow 03.</p>",
    "<p>The layer is keyed by stable canonical record_id and is applied to lean canonical SHA-256 <code>",upstream_sha,"</code>.</p>",
    "<p>Records: ",length(lines),"; excluded from Workflow 04: ",as.integer(r$exclude_from_workflow04),"; unresolved evidence conflicts: ",as.integer(r$evidence_conflicts),".</p>"),
  creators=zenodo_creators,
  access_right=zenodo_access_right,
  access_conditions="Files contain bibliographic identifiers and provider-derived publication-status metadata whose redistribution may be restricted by source terms.",
  keywords=list("Living Evidence Map","Workflow 03","publication status","retraction surveillance","OpenAlex",paste0("LivingEvidenceMap-workflow03-run-",source_run_id))
))
created<-perform(request(api)|>req_method("POST")|>auth()|>req_headers("Content-Type"="application/json")|>req_body_raw(charToRaw("{}"),type="application/json"),201L,"draft creation",60)|>resp_body_json(simplifyVector=FALSE)
dep_id<-as.character(created$id);bucket<-as.character(created$links$bucket)
perform(request(paste0(api,"/",dep_id))|>req_method("PUT")|>auth()|>req_headers("Content-Type"="application/json")|>req_body_json(metadata,auto_unbox=TRUE),200L,"metadata update",60)

paths<-c(archive_path,manifest_path);uploaded<-vector("list",length(paths))
for(i in seq_along(paths)){
  p<-paths[[i]];fn<-basename(p);ok<-NULL
  for(attempt in seq_len(5L)){
    resp<-request(paste0(bucket,"/",URLencode(fn,reserved=TRUE)))|>req_method("PUT")|>auth()|>req_headers(Expect="")|>req_body_file(p)|>req_timeout(1800)|>req_error(is_error=function(resp)FALSE)|>req_perform()
    st<-resp_status(resp)
    if(st%in%c(200L,201L)){ok<-resp;break}
    if(!(st%in%c(429L,500L,502L,503L,504L))||attempt==5L) stop(sprintf("Workflow 03 archive upload failed for %s HTTP %d",fn,st),call.=FALSE)
    Sys.sleep(min(60,5*2^(attempt-1L)))
  }
  uploaded[[i]]<-resp_body_json(ok,simplifyVector=FALSE)
}
published<-perform(request(paste0(api,"/",dep_id,"/actions/publish"))|>req_method("POST")|>auth(),c(200L,201L,202L),"publish",120)|>resp_body_json(simplifyVector=FALSE)
record_id<-as.character(if(is.null(published$record_id)) published$id else published$record_id)
receipt<-list(
  status="published",workflow="03",state="publication_status",
  source_github_run_id=as.character(source_run_id),
  publication_github_run_id=as.character(publication_run_id),
  source_github_run_url=sprintf("https://github.com/%s/actions/runs/%s",repository,source_run_id),
  upstream_lean_canonical_sha256=upstream_sha,
  publication_status_sha256=layer_sha,
  records=length(lines),status_counts=r$code_counts,
  exclude_from_workflow04=as.integer(r$exclude_from_workflow04),
  evidence_conflicts=as.integer(r$evidence_conflicts),
  multiple_positive_signals=as.integer(r$multiple_positive_signals),
  openalex_lookup=r$openalex_lookup,checked_at=r$checked_at,
  zenodo_record_id=record_id,zenodo_deposition_id=dep_id,
  doi=if(is.null(published$doi)) NA_character_ else published$doi,
  record_url=if(!is.null(published$links$html)) published$links$html else paste0("https://zenodo.org/records/",record_id),
  visibility="restricted",
  archive_files=lapply(seq_along(paths),function(i){p<-paths[[i]];z<-uploaded[[i]];list(filename=basename(p),bytes=unname(file.info(p)$size),sha256=digest(file=p,algo="sha256",serialize=FALSE),zenodo_checksum=if(is.null(z$checksum))NULL else z$checksum)}),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),file.path(output_dir,"zenodo_receipt.json"),useBytes=TRUE)
cat(sprintf("PASS: published restricted Workflow 03 state as Zenodo record %s; records=%d excluded=%d\n",record_id,length(lines),as.integer(r$exclude_from_workflow04)))
