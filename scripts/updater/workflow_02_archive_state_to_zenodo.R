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
run_id<-arg("--run-id"); repository<-arg("--repository"); output_dir<-arg("--output-dir")
workflow01_sha<-arg("--workflow01-canonical-sha256")
previous_pointer<-arg("--previous-pointer","")
if(any(vapply(list(run_id,repository,output_dir,workflow01_sha),is.null,logical(1)))) stop("Required: --state-dir --run-id --repository --output-dir --workflow01-canonical-sha256",call.=FALSE)
previous_state<-if(nzchar(previous_pointer)){
  if(!file.exists(previous_pointer)) stop("Previous Workflow 02 pointer not found",call.=FALSE)
  fromJSON(previous_pointer,simplifyVector=FALSE)
}else NULL
patch_path<-file.path(state_dir,"cumulative_patch.jsonl")
report_path<-file.path(state_dir,"enrichment_report.json")
state_manifest_path<-file.path(state_dir,"state_manifest.json")
if(!file.exists(patch_path)||!file.exists(report_path)||!file.exists(state_manifest_path)) stop("Workflow 02 state missing cumulative_patch.jsonl, enrichment_report.json or state_manifest.json",call.=FALSE)
state_manifest<-fromJSON(state_manifest_path,simplifyVector=FALSE)
final_enriched_sha<-tolower(as.character(state_manifest$final_enriched_sha256))
if(!nzchar(final_enriched_sha)) stop("Workflow 02 state manifest missing final_enriched_sha256",call.=FALSE)
token<-Sys.getenv("ZENODO_ACCESS_TOKEN");if(!nzchar(token))stop("ZENODO_ACCESS_TOKEN is not set",call.=FALSE)
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE); output_dir<-normalizePath(output_dir,mustWork=TRUE)
archive_dir<-file.path(output_dir,"archive_files");dir.create(archive_dir,recursive=TRUE,showWarnings=FALSE)

all_paths<-list.files(state_dir,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE)
if(length(all_paths)) suppressWarnings(Sys.setFileTime(all_paths,as.POSIXct("2000-01-01",tz="UTC")))
archive_name<-sprintf("LivingEvidenceMap_workflow02_run-%s_state.tar.gz",run_id)
archive_path<-file.path(archive_dir,archive_name)
old<-setwd(dirname(state_dir));on.exit(setwd(old),add=TRUE)
utils::tar(archive_path,files=basename(state_dir),compression="gzip",tar="internal")
setwd(old);on.exit(NULL,add=FALSE)
if(!file.exists(archive_path)||file.info(archive_path)$size<=0)stop("Failed to create Workflow 02 archive",call.=FALSE)

run_url<-sprintf("https://github.com/%s/actions/runs/%s",repository,run_id)
er<-fromJSON(report_path,simplifyVector=FALSE)
patch_lines<-readLines(patch_path,warn=FALSE,encoding="UTF-8");patch_lines<-patch_lines[nzchar(trimws(patch_lines))]
manifest<-list(
  schema="living-evidence-map-workflow02-state-archive-v1",
  workflow="02",
  state="enrichment_patch",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  repository=repository,
  upstream_workflow01_canonical_sha256=workflow01_sha,
  previous_workflow02=if(is.null(previous_state)) NULL else list(
    github_run_id=as.character(previous_state$github_run_id),
    zenodo_record_id=as.character(previous_state$zenodo_record_id),
    manifest_sha256=as.character(previous_state$manifest_sha256)
  ),
  cumulative_patch_records=length(patch_lines),
  final_enriched_sha256=final_enriched_sha,
  enrichment_counts=er$counts,
  file_visibility="restricted",
  files=list(state_archive=list(
    filename=archive_name,
    bytes=unname(file.info(archive_path)$size),
    sha256=digest(file=archive_path,algo="sha256",serialize=FALSE)
  ))
)
manifest_name<-sprintf("LivingEvidenceMap_workflow02_run-%s_manifest.json",run_id)
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
  title=sprintf("Living Evidence Map Workflow 02 metadata enrichment state | run %s",run_id),
  upload_type="dataset",
  publication_date=format(Sys.Date(),"%Y-%m-%d"),
  description=paste0(
    "<p>Sparse metadata enrichment/repair state for Living Evidence Map Workflow 02.</p>",
    "<p>The archive stores only Workflow 02 enrichment patches, provider audit, retry state and reports. ",
    "It is applied to upstream Workflow 01 canonical JSONL SHA-256 <code>",workflow01_sha,"</code>.</p>",
    "<p>Cumulative patch records: ",length(patch_lines),".</p>"
  ),
  creators=zenodo_creators,
  access_right=zenodo_access_right,
  access_conditions="Files contain bibliographic metadata and provider-derived enrichment whose redistribution may be restricted by source terms.",
  keywords=list("Living Evidence Map","Workflow 02","metadata enrichment","Europe PMC","Scopus",paste0("LivingEvidenceMap-workflow02-run-",run_id))
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
    if(!(st%in%c(429L,500L,502L,503L,504L))||attempt==5L)stop(sprintf("Workflow 02 archive upload failed for %s HTTP %d",fn,st),call.=FALSE)
    Sys.sleep(min(60,5*2^(attempt-1L)))
  }
  uploaded[[i]]<-resp_body_json(ok,simplifyVector=FALSE)
}
published<-perform(request(paste0(api,"/",dep_id,"/actions/publish"))|>req_method("POST")|>auth(),c(200L,201L,202L),"publish",120)|>resp_body_json(simplifyVector=FALSE)
record_id<-as.character(if(is.null(published$record_id))published$id else published$record_id)
receipt<-list(
  status="published",workflow="02",state="enrichment_patch",
  github_run_id=as.character(run_id),github_run_url=run_url,
  upstream_workflow01_canonical_sha256=workflow01_sha,
  previous_workflow02=if(is.null(previous_state)) NULL else list(
    github_run_id=as.character(previous_state$github_run_id),
    zenodo_record_id=as.character(previous_state$zenodo_record_id),
    manifest_sha256=as.character(previous_state$manifest_sha256)
  ),
  cumulative_patch_records=length(patch_lines),
  final_enriched_sha256=final_enriched_sha,
  zenodo_record_id=record_id,zenodo_deposition_id=dep_id,
  doi=if(is.null(published$doi))NA_character_ else published$doi,
  record_url=if(!is.null(published$links$html))published$links$html else paste0("https://zenodo.org/records/",record_id),
  visibility="restricted",
  archive_files=lapply(seq_along(paths),function(i){
    p<-paths[[i]];z<-uploaded[[i]]
    list(filename=basename(p),bytes=unname(file.info(p)$size),sha256=digest(file=p,algo="sha256",serialize=FALSE),
         zenodo_checksum=if(is.null(z$checksum))NULL else z$checksum)
  }),
  manifest_sha256=digest(file=manifest_path,algo="sha256",serialize=FALSE),
  published_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(receipt,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),file.path(output_dir,"zenodo_receipt.json"),useBytes=TRUE)
cat(sprintf("PASS: published restricted Workflow 02 state as Zenodo record %s; cumulative patches=%d\n",record_id,length(patch_lines)))
