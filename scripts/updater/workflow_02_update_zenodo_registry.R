#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))

args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
receipt<-arg("--receipt");registry<-arg("--registry");pointer_dir<-arg("--pointer-dir")
if(any(vapply(list(receipt,registry,pointer_dir),is.null,logical(1)))) stop("Required: --receipt --registry --pointer-dir",call.=FALSE)
x<-fromJSON(receipt,simplifyVector=FALSE)
if(!identical(x$status,"published")||!identical(x$workflow,"02")) stop("Invalid Workflow 02 receipt",call.=FALSE)
dir.create(dirname(registry),recursive=TRUE,showWarnings=FALSE);dir.create(pointer_dir,recursive=TRUE,showWarnings=FALSE)
prev<-x$previous_workflow02
row<-data.frame(
  github_run_id=as.character(x$github_run_id),
  state=as.character(x$state),
  upstream_workflow01_canonical_sha256=as.character(x$upstream_workflow01_canonical_sha256),
  cumulative_patch_records=as.integer(x$cumulative_patch_records),
  previous_workflow02_run_id=if(is.null(prev)) NA_character_ else as.character(prev$github_run_id),
  previous_workflow02_zenodo_record_id=if(is.null(prev)) NA_character_ else as.character(prev$zenodo_record_id),
  zenodo_record_id=as.character(x$zenodo_record_id),
  doi=as.character(x$doi),
  record_url=as.character(x$record_url),
  visibility=as.character(x$visibility),
  manifest_sha256=as.character(x$manifest_sha256),
  published_at_utc=as.character(x$published_at_utc),
  stringsAsFactors=FALSE
)
if(file.exists(registry)){
  old<-read.csv(registry,stringsAsFactors=FALSE,check.names=FALSE)
  old<-old[as.character(old$github_run_id)!=row$github_run_id,,drop=FALSE]
  cols<-union(names(old),names(row))
  for(nm in setdiff(cols,names(old))) old[[nm]]<-NA
  for(nm in setdiff(cols,names(row))) row[[nm]]<-NA
  out<-rbind(old[,cols,drop=FALSE],row[,cols,drop=FALSE])
}else out<-row
out<-out[order(suppressWarnings(as.numeric(out$github_run_id))),,drop=FALSE]
write.csv(out,registry,row.names=FALSE,na="")
pointer<-file.path(pointer_dir,paste0("run-",row$github_run_id,".json"))
if(!file.copy(receipt,pointer,overwrite=TRUE)) stop("Failed to write Workflow 02 pointer",call.=FALSE)
cat(sprintf("PASS: registered Workflow 02 Zenodo state %s for run %s\n",row$zenodo_record_id,row$github_run_id))
