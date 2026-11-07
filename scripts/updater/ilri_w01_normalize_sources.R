#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(jsonlite))
'%||%' <- function(a,b) if(is.null(a)) b else a
one <- function(x) {
  if (is.null(x)||!length(x)) return(NULL)
  if(is.list(x)) return(NULL)
  s <- trimws(as.character(x[[1L]]))
  if(!nzchar(s)) NULL else s
}
norm_doi <- function(x) {
  z <- one(x); if(is.null(z)) return(NULL)
  z<-tolower(z); z<-sub("^https?://(dx\\.)?doi\\.org/","",z)
  z<-sub("^doi:","",z); z<-trimws(z)
  if(nzchar(z)) z else NULL
}
inverted_abstract <- function(z) {
  if(is.null(z) || !length(z)) return(NULL)
  vals<-character(); idx<-integer()
  for(term in names(z)) for(pos in unlist(z[[term]])) {
    if(is.finite(as.numeric(pos))) {vals<-c(vals,term);idx<-c(idx,as.integer(pos))}
  }
  if(!length(idx)) return(NULL)
  paste(vals[order(idx)],collapse=" ")
}
author_names <- function(a) {
  if(is.null(a)) return(NULL)
  v<-vapply(a,function(x) {
    if(is.character(x)) return(as.character(x[[1L]]))
    one(x$display_name %||% x$fullName %||% x$full_name %||% x$name %||% (x$author %||% list())$display_name) %||% ""
  },character(1))
  v<-v[nzchar(v)]
  if(length(v)) unname(v) else NULL
}
mk <- function(src,r) {
  if(src=="scopus") {
    z<-r$raw_standard %||% list()
    id<-one(r$eid);if(is.null(id))stop("Missing Scopus EID")
    a<-one(r$authors %||% z[['dc:creator']])
    return(list(id=paste0("scopus:",id),title=r$title,abstract=NULL,
      authors=if(is.null(a))NULL else list(a),year=substr(one(r$cover_date) %||% "",1,4),
      date=r$cover_date,journal=r$publication,doi=norm_doi(r$doi),
      type=r$document_type,kw=NULL,extra=list(scopus_eid=id)))
  }
  if(src=="lens") {
    z<-r$raw %||% list();id<-one(r$source_id %||% z$lens_id)
    if(is.null(id))stop("Missing Lens ID")
    d<-NULL
    for(x in z$external_ids %||% list())if(identical(tolower(one(x$type) %||% ""),"doi")) {
      d<-norm_doi(x$value);break
    }
    journal<-if(is.list(z$source))one(z$source$title) else one(z$source)
    return(list(id=paste0("lens:",id),title=z$title,abstract=z$abstract,
      authors=author_names(z$authors),year=z$year_published,date=z$date_published,
      journal=journal,doi=d,type=z$publication_type,kw=z$keywords,extra=list(lens_id=id)))
  }
  if(src=="openalex") {
    z<-r$raw %||% list();id<-one(r$source_id %||% z$id)
    if(is.null(id))stop("Missing OpenAlex ID")
    kw<-vapply(z$keywords %||% list(),function(x)one(x$display_name) %||% "",character(1));kw<-kw[nzchar(kw)]
    return(list(id=paste0("openalex:",id),title=z$title %||% z$display_name,
      abstract=inverted_abstract(z$abstract_inverted_index),
      authors=author_names(z$authorships),year=z$publication_year,date=z$publication_date,
      journal=one((z$primary_location %||% list())$source$display_name),
      doi=norm_doi(z$doi),type=z$type,kw=if(length(kw))unname(kw) else NULL,
      extra=list(openalex_id=id)))
  }
  if(src=="agricola") {
    z<-r$raw_europepmc_core %||% list();id<-one(z$id %||% r$source_uid)
    if(is.null(id))stop("Missing AGRICOLA ID")
    journal<-one((z$journalInfo %||% list())$journal$title %||% z$journalTitle)
    a<-one(z$authorString)
    return(list(id=paste0("agricola:",id),title=z$title,abstract=z$abstractText,
      authors=if(is.null(a))NULL else list(a),year=z$pubYear,date=z$firstPublicationDate,
      journal=journal,doi=norm_doi(z$doi),type=(z$pubTypeList %||% list())$pubType,
      kw=NULL,extra=list(agricola_id=id)))
  }
  if(src=="wos") {
    z<-r$raw_starter %||% list();id<-one(z$uid %||% r$uid)
    if(is.null(id))stop("Missing WoS UID")
    aa<-lapply((z$names %||% list())$authors %||% list(),function(x)
      list(display_name=x$displayName %||% x$wosStandard))
    return(list(id=paste0("wos:",id),title=z$title,abstract=z$abstract,
      authors=author_names(aa),year=(z$source %||% list())$publishYear,date=NULL,
      journal=(z$source %||% list())$sourceTitle,
      doi=norm_doi((z$identifiers %||% list())$doi),type=z$types,
      kw=(z$keywords %||% list())$authorKeywords,extra=list(wos_uid=id)))
  }
  stop("Unsupported source")
}
expected<-c(scopus=10919L,lens=11241L,openalex=9105L,agricola=192L,wos=9338L,
 cab=4951L,proquest=528L,lilacs=87L)
root<-"w01_archive_inputs"
dir.create("current_sources",showWarnings=FALSE)
paths<-list();coverage<-list()
for(src in names(expected)) {
  p<-if(src %in% c("cab","proquest","lilacs")) {
    file.path(root,"manual","current_sources",src,"records.jsonl")
  } else file.path(root,src,"unique_records.jsonl")
  if(!file.exists(p))stop(sprintf("%s: missing %s",src,p))
  target<-file.path("current_sources",src,"records.jsonl")
  dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE)
  input<-file(p,"r",encoding="UTF-8")
  output<-file(target,"w",encoding="UTF-8")
  n<-0L;title_n<-0L;abs_n<-0L;doi_n<-0L;ids<-new.env(hash=TRUE,parent=emptyenv())
  repeat {
    ln<-readLines(input,n=1L,warn=FALSE)
    if(!length(ln))break
    if(!nzchar(trimws(ln)))next
    r<-fromJSON(ln,simplifyVector=FALSE)
    if(src %in% c("cab","proquest","lilacs")) {
      obj<-r
      id<-one(obj$sidecar_identity$sidecar_record_id)
      title<-one(obj$mapped_fields$title);abstract<-one(obj$mapped_fields$abstract)
      d<-norm_doi(obj$mapped_fields$doi)
      provider<-one(obj$source$provider)
    } else {
      z<-mk(src,r);id<-z$id;title<-one(z$title);abstract<-one(z$abstract);d<-z$doi
      obj<-list(source=list(provider=src,source_format="ilri_w00_archive"),
        sidecar_identity=c(list(sidecar_record_id=id,doi=d),z$extra),
        mapped_fields=list(title=title,abstract=abstract,authors=z$authors,year=z$year,
          publication_date=z$date,source=z$journal,doi=d,
          publication_type=z$type,author_keywords=z$kw),
        provenance=list(source_archive="ILRI W00 restricted Zenodo archive",
          source_raw_payload_stored_upstream=TRUE))
      provider<-src
    }
    if(is.null(id)||exists(id,envir=ids,inherits=FALSE))stop(sprintf("%s: duplicate/missing ID %s",src,id))
    if(!identical(provider,src))stop(sprintf("%s: mismatched provider %s",src,provider))
    assign(id,TRUE,envir=ids)
    writeLines(toJSON(obj,auto_unbox=TRUE,null="null",na="null",digits=NA),output)
    n<-n+1L
    if(!is.null(title))title_n<-title_n+1L
    if(!is.null(abstract))abs_n<-abs_n+1L
    if(!is.null(d))doi_n<-doi_n+1L
  }
  close(input);close(output)
  if(n!=expected[[src]])stop(sprintf("%s: expected %d, found %d",src,expected[[src]],n))
  paths[[src]]<-target;coverage[[src]]<-list(records=n,titles=title_n,abstracts=abs_n,dois=doi_n)
  cat(sprintf("PASS W01 normalized %s: %d records; titles=%d abstracts=%d dois=%d\n",
      src,n,title_n,abs_n,doi_n))
  flush.console()
}
dir.create("outputs",showWarnings=FALSE)
writeLines(toJSON(list(schema="living-evidence-map-workflow01-source-inputs-v1",
 status="validated_full_initial_ilri_corpus",sources=paths,source_coverage=coverage),
 pretty=TRUE,auto_unbox=TRUE,null="null"),"outputs/ilri_w01_sources.json")
cat(sprintf("PASS W01 input normalization: %d records across %d sources\n",
  sum(expected),length(expected)))
