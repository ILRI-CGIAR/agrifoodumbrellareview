#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

previous_repairs <- arg("--previous-repairs",NULL)
new_repairs <- arg("--new-repairs")
previous_strips <- arg("--previous-strips",NULL)
new_strips <- arg("--new-strips")
output_repairs <- arg("--output-repairs")
output_strips <- arg("--output-strips")
if(any(vapply(list(new_repairs,new_strips,output_repairs,output_strips),is.null,logical(1)))) {
  stop("Required: --new-repairs --new-strips --output-repairs --output-strips",call.=FALSE)
}

read_nonempty <- function(path){
  if(is.null(path)||!file.exists(path)) return(character())
  x <- readLines(path,warn=FALSE,encoding="UTF-8")
  x[nzchar(trimws(x))]
}
write_lines <- function(x,path){
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  if(length(x)) writeLines(x,path,useBytes=TRUE) else file.create(path)
}
merge_keyed <- function(previous,new,key_fun,label){
  p <- read_nonempty(previous); n <- read_nonempty(new)
  pk <- if(length(p)) vapply(p,key_fun,character(1)) else character()
  nk <- if(length(n)) vapply(n,key_fun,character(1)) else character()
  if(anyDuplicated(pk)) stop(sprintf("Previous %s ledger has duplicate keys",label),call.=FALSE)
  if(anyDuplicated(nk)) stop(sprintf("New %s ledger has duplicate keys",label),call.=FALSE)
  store <- setNames(as.list(p),pk)
  if(length(n)) for(i in seq_along(n)) store[[nk[[i]]]] <- n[[i]]
  keys <- sort(names(store))
  if(length(keys)) unlist(store[keys],use.names=FALSE) else character()
}
repair_key <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  paste(as.character(z$review_case_id),as.character(z$source),
        as.character(z$source_record_id),as.character(z$action),sep="|")
}
strip_key <- function(line){
  z <- fromJSON(line,simplifyVector=FALSE)
  paste(as.character(z$source),as.character(z$source_record_id),sep="::")
}

repairs <- merge_keyed(previous_repairs,new_repairs,repair_key,"repair")
strips <- merge_keyed(previous_strips,new_strips,strip_key,"strip-action")
write_lines(repairs,output_repairs)
write_lines(strips,output_strips)

cat(sprintf("PASS: cumulative Workflow 01 ledgers: %d data-quality repairs, %d abstract-strip actions\n",
            length(repairs),length(strips)))
