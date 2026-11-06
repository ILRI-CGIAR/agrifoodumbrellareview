#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(jsonlite)})
token<-Sys.getenv("ZENODO_ACCESS_TOKEN")
if(!nzchar(token))stop("Missing Zenodo access token")
items<-list(lens="24023958",openalex="24023961")
out<-list()
dir.create("outputs/metadata_correction",recursive=TRUE,showWarnings=FALSE)
request_auth<-function(url,method="GET",body=NULL){
 req<-request(url)|>req_headers(Authorization=paste("Bearer",token),Accept="application/json")
 if(method!="GET")req<-req|>req_method(method)
 if(!is.null(body))req<-req|>req_body_json(body,auto_unbox=TRUE)
 resp<-req|>req_timeout(90)|>req_error(is_error=function(z)FALSE)|>req_perform()
 list(status=resp_status(resp),value=tryCatch(resp_body_json(resp,simplifyVector=FALSE),error=function(e)list()),
      error=substr(tryCatch(resp_body_string(resp),error=function(e)""),1L,600L))
}
for(provider in names(items)){
 id<-items[[provider]]
 base<-paste0("https://zenodo.org/api/deposit/depositions/",id)
 before<-request_auth(base)
 if(before$status!=200L)stop(sprintf("Cannot read %s deposition %s HTTP %d",provider,id,before$status))
 if(before$value$metadata$access_right!="restricted")stop("Refusing to edit non-restricted record")
 meta<-before$value$metadata
 meta$title<-sprintf("Agri-food umbrella review: %s scholarly records, 2015 onwards, datasets excluded",if(provider=="lens")"Lens" else "OpenAlex")
 meta$description<-sprintf("<p>Restricted source bibliographic metadata retrieved from the %s scholarly API using a 2015-onwards climate, evidence-synthesis and agri-food search. Publication-type dataset records were explicitly excluded from the API searches; source IDs were internally deduplicated and original search-component provenance preserved. Four Scopus ordered-proximity operators were approximated with Boolean AND; for OpenAlex, Scopus truncation characters were removed. These are methodological translation differences. Restricted source metadata are not licensed for bulk public redistribution.</p>",
   if(provider=="lens")"Lens" else "OpenAlex")
 unlocked<-FALSE
 for(attempt in 1:8){
   edit<-request_auth(paste0(base,"/actions/edit"),"POST")
   if(edit$status%in%c(200L,201L,202L)){unlocked<-TRUE;break}
   if(edit$status!=409L)stop(sprintf("Zenodo %s edit HTTP %d: %s",provider,edit$status,edit$error))
   Sys.sleep(15)
 }
 if(!unlocked)stop(sprintf("Zenodo %s is still integrating; cannot edit metadata now",provider))
 modified<-request_auth(base,"PUT",list(metadata=meta))
 if(modified$status!=200L)stop(sprintf("Zenodo %s metadata PUT failed HTTP %d: %s",provider,modified$status,modified$error))
 published<-request_auth(paste0(base,"/actions/publish"),"POST")
 if(!(published$status%in%c(200L,201L,202L)))stop(sprintf("Zenodo %s metadata republish failed HTTP %d: %s",provider,published$status,published$error))
 out[[provider]]<-list(record=id,status="corrected_and_republished",title=meta$title,visibility="restricted")
 cat(sprintf("PASS %s Zenodo record %s metadata corrected and republished\n",provider,id))
 write_json(out,"outputs/metadata_correction/receipt.json",auto_unbox=TRUE,pretty=TRUE)
}
