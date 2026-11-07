#!/usr/bin/env Rscript
# Stable source identities, not positional indices, are authoritative W01 identities.
suppressPackageStartupMessages({library(data.table);library(digest);library(jsonlite)})
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
 i <- match(flag,args);if(is.na(i))return(default)
 if(i==length(args))stop("Missing value for ",flag,call.=FALSE)
 args[[i+1L]]
}
metadata <- arg("--metadata")
decisions <- arg("--decisions")
out <- arg("--output")
if(any(vapply(list(metadata,decisions,out),is.null,logical(1))))
 stop("Required: --metadata --decisions --output",call.=FALSE)
meta <- fread(metadata,na.strings=c("","NA"))
required <- c("idx","source","source_record_id")
if(length(setdiff(required,names(meta))))stop("Missing source-identity columns",call.=FALSE)
if(!identical(as.integer(meta$idx),seq_len(nrow(meta))))stop("Index is not contiguous",call.=FALSE)
for(nm in c("source","source_record_id")) {
 if(anyNA(meta[[nm]])||any(!nzchar(trimws(meta[[nm]]))))
  stop("Missing immutable ",nm,call.=FALSE)
}
# Length-prefixed components avoid ambiguities when IDs contain separators.
identity <- function(source,id) paste0(nchar(source,type="bytes"),":",source,nchar(id,type="bytes"),":",id)
ids <- identity(as.character(meta$source),as.character(meta$source_record_id))
if(anyDuplicated(ids))stop("Duplicate source identity",call.=FALSE)
d <- fread(decisions,na.strings=c("","NA"))
if(!all(c("record_i","record_j")%in%names(d)))stop("Decision indices absent",call.=FALSE)
i <- as.integer(d$record_i);j <- as.integer(d$record_j)
if(anyNA(i)||anyNA(j)||any(i<1L|i>length(ids)|j<1L|j>length(ids)|i==j))
 stop("Invalid pair indices",call.=FALSE)
# Decisions crossing a persistence or adjudication boundary MUST already carry
# their source identities. Never derive them from a potentially reordered table.
identity_cols <- c("source_id_i","source_record_id_i","source_id_j","source_record_id_j")
if(!all(identity_cols %in% names(d)))
 stop("Pair decisions lack persisted source IDs; refusing positional reconstruction",call.=FALSE)
if(anyNA(d[,..identity_cols]))
 stop("Pair decisions contain missing source identity",call.=FALSE)
stored_i <- identity(as.character(d$source_id_i),as.character(d$source_record_id_i))
stored_j <- identity(as.character(d$source_id_j),as.character(d$source_record_id_j))
key_i <- ids[i];key_j <- ids[j]
if(any(stored_i!=key_i | stored_j!=key_j)) {
 stop("Pair index-to-source-ID mismatch: source ordering changed; refusing incorrect decision assignment",call.=FALSE)
}
first <- ifelse(key_i<=key_j,key_i,key_j)
second <- ifelse(key_i<=key_j,key_j,key_i)
d[, `:=`(source_id_i=as.character(meta$source[i]),
 source_record_id_i=as.character(meta$source_record_id[i]),
 source_id_j=as.character(meta$source[j]),
 source_record_id_j=as.character(meta$source_record_id[j]),
 stable_pair_key=vapply(seq_along(first),function(k)
  digest(paste0(nchar(first[[k]],type="bytes"),":",first[[k]],second[[k]]),algo="sha256",serialize=FALSE),
  character(1)))]
if(anyDuplicated(d$stable_pair_key)) stop("Repeated stable pair identity",call.=FALSE)
if("pair_key" %in% names(d)) d[,pair_key:=NULL]
dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
fwrite(d,out)
cat(sprintf("PASS: %d pairs bound to explicit source IDs; positional indices not persisted as pair keys\\n",nrow(d)))
