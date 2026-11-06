#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite);library(digest)})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag,default=NULL){i<-match(flag,args);if(is.na(i))return(default);if(i==length(args))stop(sprintf("Missing value after %s",flag),call.=FALSE);args[[i+1L]]}
pointer<-arg("--pointer"); out<-arg("--output-dir")
if(is.null(pointer)||is.null(out)) stop("Required: --pointer --output-dir",call.=FALSE)
x<-fromJSON(pointer,simplifyVector=FALSE)
if(!identical(x$status,"published")||!identical(x$workflow,"02")||!identical(x$visibility,"restricted")) stop("Invalid Workflow 02 pointer",call.=FALSE)
token<-Sys.getenv("ZENODO_ACCESS_TOKEN");if(!nzchar(token))stop("ZENODO_ACCESS_TOKEN required",call.=FALSE)
dep_id<-as.character(x$zenodo_deposition_id)
api<-paste0("https://zenodo.org/api/deposit/depositions/",dep_id)
auth<-function(req) req |> req_headers(Authorization=paste("Bearer",token))
dep<-request(api)|>auth()|>req_timeout(120)|>req_perform()|>resp_body_json(simplifyVector=FALSE)
if(!isTRUE(dep$submitted)) stop("Workflow 02 Zenodo deposition is not published",call.=FALSE)
bucket<-as.character(dep$links$bucket)
files<-x$archive_files
if(is.data.frame(files)) files<-lapply(seq_len(nrow(files)),function(i)as.list(files[i,,drop=FALSE]))
if(is.list(files)&&!is.null(files$filename)) files<-list(files)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
dl<-file.path(out,".download");dir.create(dl,showWarnings=FALSE)
for(z in files){
  fn<-as.character(z$filename); dest<-file.path(dl,fn)
  r<-request(paste0(sub("/$","",bucket),"/",URLencode(fn,reserved=TRUE)))|>req_method("GET")|>auth()|>req_timeout(1800)|>req_error(is_error=function(resp)FALSE)|>req_perform(path=dest)
  if(resp_status(r)!=200L) stop(sprintf("Download failed for %s",fn),call.=FALSE)
  if(unname(file.info(dest)$size)!=as.numeric(z$bytes)) stop(sprintf("Byte mismatch for %s",fn),call.=FALSE)
  if(tolower(digest(file=dest,algo="sha256",serialize=FALSE))!=tolower(as.character(z$sha256))) stop(sprintf("SHA mismatch for %s",fn),call.=FALSE)
}
tar<-list.files(dl,pattern="\\.tar\\.gz$",full.names=TRUE)
if(length(tar)!=1L) stop("Expected one Workflow 02 state archive",call.=FALSE)
utils::untar(tar,exdir=out)
cat(normalizePath(out,mustWork=TRUE),"
")
