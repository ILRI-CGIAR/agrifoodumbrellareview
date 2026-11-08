#!/usr/bin/env Rscript
# Generic first-run and update guard. Neither project, topic, branch nor later
# workflow stages are hard-coded into new source or archive initialization.
args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default="") {
  i<-match(flag,args)
  if(is.na(i))return(default)
  if(i==length(args))stop("Missing value after ",flag,call.=FALSE)
  args[[i+1L]]
}
raw <- tolower(trimws(arg("--first-run","false")))
if(!raw %in% c("true","false"))stop("--first-run must be true or false",call.=FALSE)
first <- identical(raw,"true")
stage <- toupper(trimws(arg("--stage","W00")))
if(!stage %in% sprintf("W%02d",0:3))stop("Unsupported stage: ",stage,call.=FALSE)
previous <- trimws(arg("--previous-pointer",""))
if(tolower(previous) %in% c("none","na","null"))previous <- ""
if(stage %in% c("W00","W01","W02")){
  if(first && nzchar(previous))
    stop(stage,": a new project must not provide a previous-stage pointer",call.=FALSE)
  if(!first && !nzchar(previous))
    stop(stage,": update requires an explicit previous-stage pointer",call.=FALSE)
  if(!first && !file.exists(previous))
    stop(stage,": previous-stage pointer does not exist: ",previous,call.=FALSE)
}
# W03 takes its current canonical/lean pointer in its own workflow interface,
# and verifies it independently. It does not depend on a historical W08 state.
cat(sprintf("PASS: %s %s; prior=%s\n",stage,
            if(first)"initial" else "update",
            if(nzchar(previous))"validated" else "not applicable"))
