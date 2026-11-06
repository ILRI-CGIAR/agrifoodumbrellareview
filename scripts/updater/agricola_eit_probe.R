#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(httr2);library(xml2);library(jsonlite)})
dir.create("outputs/agricola",recursive=TRUE,showWarnings=FALSE)
path<-"outputs/agricola/api_entitlement_probe.json"
report<-list(database="AGRICOLA",provider="EBSCOhost EIT",db_code="agr",workflow="count-only authentication and entitlement",api_requested=FALSE)
finish<-function(status,details=list()){
  report$status<-status
  for(k in names(details))report[[k]]<-details[[k]]
  write_json(report,path,pretty=TRUE,auto_unbox=TRUE,null="null")
  cat(sprintf("AGRICOLA %s\n",status))
}
uid<-Sys.getenv("EBSCO_EHOST_UID"); pwd<-Sys.getenv("EBSCO_EHOST_PWD")
if(!nzchar(uid)||!nzchar(pwd)){
 finish("missing_eit_profile_credentials",list(
   missing=as.list(c(if(!nzchar(uid))"EBSCO_EHOST_UID",if(!nzchar(pwd))"EBSCO_EHOST_PWD"))))
 stop("EBSCO EIT profile credentials missing in destination repository; no AGRICOLA API request attempted")
}
fetch<-function(endpoint,params){
 req<-request(paste0("https://eit.ebscohost.com/Services/SearchService.asmx/",endpoint)) |>
   req_url_query(!!!params) |> req_timeout(90) |> req_error(is_error=function(r)FALSE)
 res<-tryCatch(req_perform(req),error=identity)
 if(inherits(res,"error"))return(list(error=conditionMessage(res)))
 status<-resp_status(res)
 body<-tryCatch(resp_body_string(res),error=function(e)"")
 doc<-tryCatch(read_xml(body),error=function(e)NULL)
 list(http=status,doc=doc,excerpt=substr(body,1L,500L))
}
report$api_requested<-TRUE
info<-fetch("Info",list(prof=uid,pwd=pwd,authType="profile"))
if(is.null(info$http)||info$http!=200L||is.null(info$doc)){
 finish("info_failed",list(http=info$http,error=info$error,excerpt=info$excerpt))
 stop("EBSCO Info endpoint failed")
}
dbs<-xml_find_all(info$doc,"//*[local-name()='db']")
ix<-which(xml_attr(dbs,"shortName")=="agr")
if(length(ix)!=1L){
 finish("agricola_not_entitled",list(database_count=length(dbs),matched=length(ix)))
 stop("AGRICOLA db=agr is not listed in the EBSCO EIT enabled profile")
}
node<-dbs[[ix]]
fields<-unique(xml_attr(xml_find_all(node,".//*[local-name()='dbTag']"),"name"))
report$agricola_entitlement_confirmed<-TRUE
report$database_display_name<-xml_attr(node,"longName")
report$search_fields_available<-as.list(fields[!is.na(fields)])
probe<-fetch("Search",list(prof=uid,pwd=pwd,authType="profile",db="agr",
 query="TI (climate change) AND ZR (2020)",format="detailed",startrec="1",numrec="1"))
if(is.null(probe$http)||probe$http!=200L||is.null(probe$doc)){
 finish("smoke_search_failed",list(http=probe$http,error=probe$error,excerpt=probe$excerpt))
 stop("AGRICOLA API smoke search failed")
}
hits<-xml_find_all(probe$doc,"//*[local-name()='Hits']")
n<-suppressWarnings(as.integer(xml_text(hits)))
n<-n[!is.na(n)]
if(!length(n)){
 finish("smoke_search_missing_count")
 stop("AGRICOLA smoke search did not expose Hits")
}
finish("smoke_search_success",list(http=200,smoke_query="TI (climate change) AND ZR (2020)",smoke_hits=n[[1L]]))
