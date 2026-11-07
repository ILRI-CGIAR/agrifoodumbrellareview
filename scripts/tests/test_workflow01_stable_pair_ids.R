#!/usr/bin/env Rscript
# Regression: reordered source records must NEVER reassign duplicate decisions.
suppressPackageStartupMessages({library(data.table);library(jsonlite)})
root<-tempfile("w01-stable-id-regression-")
dir.create(root,recursive=TRUE)
on.exit(unlink(root,recursive=TRUE),add=TRUE)
meta<-data.table(idx=1:3,source=c("lens","scopus","lens"),
 source_record_id=c("L1","S1","L2"))
decision<-data.table(record_i=1L,record_j=2L,
 source_id_i="lens",source_record_id_i="L1",
 source_id_j="scopus",source_record_id_j="S1",classification="duplicate")
meta_path<-file.path(root,"meta.csv");dpath<-file.path(root,"pairs.csv")
fwrite(meta,meta_path);fwrite(decision,dpath)
cmd<-file.path("scripts","updater","workflow_01_bind_stable_pair_ids.R")
run<-function(m,expect_success) {
 fwrite(m,meta_path)
 out<-file.path(root,"validated.csv")
 p<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
   c(shQuote(cmd),"--metadata",shQuote(meta_path),"--decisions",shQuote(dpath),
     "--output",shQuote(out)),stdout=TRUE,stderr=TRUE))
 status<-attr(p,"status");if(is.null(status))status<-0L
 if(identical(status==0L,expect_success))return(invisible(TRUE))
 stop(paste(c("Unexpected W01 identity validation outcome",p),collapse="\n"))
}
run(meta,TRUE)
swapped<-copy(meta);swapped[c(1L,3L)]<-swapped[c(3L,1L)]
swapped[,idx:=1:3]
run(swapped,FALSE)
# Missing source IDs must not be silently filled using current row indices.
fwrite(decision[,.(record_i,record_j,classification)],dpath)
run(meta,FALSE)
cat("PASS: W01 explicit-identity decisions resist row reordering and missing identity\n")
