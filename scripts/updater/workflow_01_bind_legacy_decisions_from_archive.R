#!/usr/bin/env Rscript
# Convert pre-identity W01 pair decisions using THEIR ORIGINAL archived map.
# Never bind an old numeric index against a newly assembled source order.
suppressPackageStartupMessages(library(data.table))
args<-commandArgs(trailingOnly=TRUE)
arg<-function(flag) {
 i<-match(flag,args)
 if(is.na(i)||i==length(args))stop("Required: ",flag,call.=FALSE)
 args[[i+1L]]
}
path<-arg("--decisions")
map_path<-arg("--archived-manifestation-map")
out<-arg("--output")
map<-fread(map_path,na.strings=c("","NA"))
stopifnot(all(c("idx","source","source_record_id")%in%names(map)))
setorder(map,idx)
if(!identical(as.integer(map$idx),seq_len(nrow(map))))
 stop("Historical manifestation map has shifted/non-contiguous indices",call.=FALSE)
if(anyNA(map$source)||anyNA(map$source_record_id)||
 any(!nzchar(trimws(as.character(map$source))))||
 any(!nzchar(trimws(as.character(map$source_record_id)))))
 stop("Historical manifestation map has invalid source identities",call.=FALSE)
if(anyDuplicated(paste(map$source,map$source_record_id,sep="\u001f")))
 stop("Historical manifestation map has duplicated source identity",call.=FALSE)
d<-fread(path,na.strings=c("","NA"))
if(nrow(d)) {
 if(!all(c("record_i","record_j")%in%names(d)))stop("Legacy pair coordinates missing",call.=FALSE)
 i<-as.integer(d$record_i);j<-as.integer(d$record_j)
 if(anyNA(i)||anyNA(j)||any(i<1L|i>nrow(map)|j<1L|j>nrow(map)|i==j))
  stop("Legacy pair coordinates invalid against archived mapping",call.=FALSE)
 fields<-list(source_id_i=as.character(map$source[i]),
 source_record_id_i=as.character(map$source_record_id[i]),
 source_id_j=as.character(map$source[j]),
 source_record_id_j=as.character(map$source_record_id[j]))
 for(nm in names(fields)) {
  if(nm %in% names(d) && (anyNA(d[[nm]])||any(d[[nm]]!=fields[[nm]])))
   stop("Historical decision conflicts with archived source identity: ",nm,call.=FALSE)
  d[,(nm):=fields[[nm]]]
 }
}
dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
fwrite(d,out)
cat(sprintf("PASS: mapped %d historic decisions to archival source IDs\n",nrow(d)))
