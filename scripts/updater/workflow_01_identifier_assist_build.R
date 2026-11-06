#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
})

`%||%` <- function(x,y) if (is.null(x)) y else x

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

manifest_path <- arg("--input-manifest")
metadata_path <- arg("--metadata")
guard_path <- arg("--guard-file")
prior_n <- as.integer(arg("--prior-manifestations"))
output_dir <- arg("--output-dir")
if (any(vapply(list(manifest_path,metadata_path,guard_path,output_dir),is.null,logical(1))) || is.na(prior_n)) {
  stop("Required: --input-manifest --metadata --guard-file --prior-manifestations --output-dir",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

scalar <- function(x) {
  if (is.null(x)||!length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if (!nzchar(y)||is.na(y)) NULL else y
}
norm_doi <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- tolower(trimws(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/","",x,perl=TRUE)
  x <- sub("^doi:\\s*","",x,perl=TRUE)
  x <- sub("[?#].*$","",x,perl=TRUE)
  x <- sub("/full/html?$","",x,perl=TRUE,ignore.case=TRUE)
  x <- sub("\\.(html?|pdf|xml)$","",x,perl=TRUE,ignore.case=TRUE)
  x <- sub("[.,;:]+$","",x,perl=TRUE)
  if (!nzchar(x)) NULL else x
}
norm_pmid <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- sub("^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/","",x,ignore.case=TRUE)
  x <- sub("/$","",x)
  x <- gsub("[^0-9]","",x)
  if (!nzchar(x)) NULL else x
}
norm_pmcid <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- toupper(x)
  x <- sub("^https?://www\\.ncbi\\.nlm\\.nih\\.gov/pmc/articles/","",x,ignore.case=TRUE)
  x <- sub("/$","",x); x <- gsub("[^A-Z0-9]","",x)
  if (!startsWith(x,"PMC")) x <- paste0("PMC",x)
  if (!grepl("^PMC[0-9]+$",x)) NULL else x
}
norm_openalex <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- toupper(sub("^https?://openalex\\.org/","",x,ignore.case=TRUE))
  if (!grepl("^W[0-9]+$",x)) NULL else x
}
norm_mag <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- gsub("[^0-9]","",x)
  if (!nzchar(x)) NULL else x
}
norm_core <- function(x) {
  x <- scalar(x); if (is.null(x)) return(NULL)
  x <- sub("^https?://core\\.ac\\.uk/(works/)?","",x,ignore.case=TRUE)
  x <- sub("^core[: ]*","",x,ignore.case=TRUE)
  x <- trimws(x)
  if (!nzchar(x)) NULL else x
}
normalise <- function(ns,value) {
  ns <- tolower(trimws(as.character(ns %||% "")))
  aliases <- c(
    doi="doi", pmid="pmid", pubmed="pmid", pubmed_id="pmid", `pubmed-id`="pmid",
    pmcid="pmcid", pmc="pmcid", openalex="openalex", openalex_id="openalex",
    mag="mag", magid="mag", mag_id="mag", core="core", coreid="core", core_id="core"
  )
  if (!(ns %in% names(aliases))) return(NULL)
  ns <- unname(aliases[[ns]])
  v <- switch(ns,
    doi=norm_doi(value), pmid=norm_pmid(value), pmcid=norm_pmcid(value),
    openalex=norm_openalex(value), mag=norm_mag(value), core=norm_core(value), NULL
  )
  if (is.null(v)) NULL else list(identifier_type=ns,identifier_value=v)
}
source_record_id <- function(r) {
  if (is.list(r$lens)) {
    return(as.character((r$identity %||% list())$lens_id %||%
                        (r$identity %||% list())$record_id %||% ""))
  }
  as.character((r$sidecar_identity %||% list())$sidecar_record_id %||% "")
}
read_jsonl <- function(path,fun) {
  con <- file(path,"rt",encoding="UTF-8"); on.exit(close(con),add=TRUE)
  n <- 0L
  repeat {
    lines <- readLines(con,n=500L,warn=FALSE)
    if (!length(lines)) break
    for (line in lines[nzchar(trimws(lines))]) {
      n <- n+1L
      fun(fromJSON(line,simplifyVector=FALSE),n)
    }
  }
  n
}
title_similarity_lv <- function(a,b) {
  if (is.na(a)||is.na(b)||!nzchar(a)||!nzchar(b)) return(NA_real_)
  1 - adist(a,b)[1L] / max(nchar(a),nchar(b),1L)
}

im <- fromJSON(manifest_path,simplifyVector=FALSE)
if (!identical(im$schema,"living-evidence-map-workflow01-source-inputs-v1")) stop("Unsupported input manifest",call.=FALSE)
paths <- vapply(im$sources,function(z)as.character(z[[1L]]),character(1))
if (!length(paths)||any(!file.exists(paths))) stop("Identifier assist input source missing",call.=FALSE)

meta <- fread(metadata_path,na.strings=c("","NA"))
need_meta <- c("idx","source","source_record_id","title_norm","doi_norm","year")
if (length(setdiff(need_meta,names(meta)))) stop("normalised metadata lacks required fields",call.=FALSE)
if (!identical(as.integer(meta$idx),seq_len(nrow(meta)))) stop("metadata idx is not contiguous",call.=FALSE)
if (prior_n<0L || prior_n>nrow(meta)) stop("Invalid prior manifestation count",call.=FALSE)
meta[,manifestation_key:=paste(source,source_record_id,sep="::")]
meta_key <- setNames(meta$idx,meta$manifestation_key)
meta_membership <- new.env(hash=TRUE,parent=emptyenv(),size=max(29L,2L*nrow(meta)))
for (k in meta$manifestation_key) assign(k,TRUE,envir=meta_membership)

guards <- fread(guard_path,na.strings=c("","NA"))
if (!all(c("identifier_type","identifier_value") %in% names(guards))) stop("Guard file schema invalid",call.=FALSE)
guards <- unique(guards[,.(identifier_type=tolower(identifier_type),identifier_value)])
guard_keys <- paste(guards$identifier_type,guards$identifier_value,sep="::")

registry <- list()
add_id <- function(key,source,ns,value,provenance) {
  if (!(exists(key,envir=meta_membership,inherits=FALSE))) return(invisible(NULL))
  z <- normalise(ns,value)
  if (is.null(z)) return(invisible(NULL))
  registry[[length(registry)+1L]] <<- data.table(
    manifestation_key=key,source=source,
    identifier_type=z$identifier_type,identifier_value=z$identifier_value,
    identifier_provenance=provenance
  )
}
add_named_ids <- function(key,source,x,provenance) {
  if (is.null(x)||!is.list(x)||is.null(names(x))) return(invisible(NULL))
  for (nm in names(x)) add_id(key,source,nm,x[[nm]],paste0(provenance,".",nm))
}

# DOI is already normalised deterministically by W01; always retain it.
for (i in which(!is.na(meta$doi_norm)&nzchar(meta$doi_norm))) {
  add_id(meta$manifestation_key[[i]],meta$source[[i]],"doi",meta$doi_norm[[i]],"w01_metadata.doi_norm")
}

source_counts <- list()
for (src in names(paths)) {
  source_counts[[src]] <- read_jsonl(paths[[src]],function(r,i) {
    rid <- source_record_id(r)
    if (!nzchar(rid)) stop(sprintf("%s record %d lacks source record ID",src,i),call.=FALSE)
    key <- paste(src,rid,sep="::")
    if (!(exists(key,envir=meta_membership,inherits=FALSE))) stop(sprintf("%s record missing from W01 metadata: %s",src,rid),call.=FALSE)

    if (identical(src,"lens")) {
      ext <- ((r$lens %||% list())$raw_payload %||% list())$external_ids %||% list()
      for (z in ext) if (is.list(z)) add_id(key,src,z$type %||% "",z$value,"lens.raw_payload.external_ids")
    } else {
      sid <- r$sidecar_identity %||% list()
      for (nm in c("doi","pmid","pmcid","openalex_id","mag_id","core_id")) {
        add_id(key,src,nm,sid[[nm]],paste0("sidecar_identity.",nm))
      }
      add_named_ids(key,src,r$identifiers %||% list(),"identifiers")
      mf <- r$mapped_fields %||% list()
      for (nm in c("doi","pmid","pmcid","openalex","mag","core")) {
        add_id(key,src,nm,mf[[nm]],paste0("mapped_fields.",nm))
      }

      raw <- NULL
      if (identical(src,"scopus")) raw <- (r$scopus %||% list())$raw_payload
      if (identical(src,"agricola")) raw <- (r$agricola %||% list())$raw_payload
      if (identical(src,"wos")) raw <- (r$wos %||% list())$raw_payload
      if (identical(src,"openalex")) raw <- (r$openalex %||% list())$raw_payload
      if (is.list(raw)) {
        add_id(key,src,"doi",raw[["prism:doi"]] %||% raw$doi,"raw_payload.doi")
        add_id(key,src,"pmid",raw[["pubmed-id"]] %||% raw$pmid,"raw_payload.pmid")
        add_id(key,src,"pmcid",raw$pmcid,"raw_payload.pmcid")
        add_named_ids(key,src,raw$identifiers %||% list(),"raw_payload.identifiers")
        add_named_ids(key,src,raw$ids %||% list(),"raw_payload.ids")
      }
    }
  })
}

reg <- if (length(registry)) unique(rbindlist(registry,use.names=TRUE,fill=TRUE)) else
  data.table(manifestation_key=character(),source=character(),identifier_type=character(),
             identifier_value=character(),identifier_provenance=character())
if (!nrow(reg)) stop("Identifier registry is empty",call.=FALSE)
fwrite(reg,file.path(output_dir,"identifier_registry.csv"))

coverage <- reg[,.(manifestations=uniqueN(manifestation_key),identifier_rows=.N),
                by=.(source,identifier_type)][order(source,identifier_type)]
fwrite(coverage,file.path(output_dir,"identifier_coverage.csv"))

groups <- reg[,.(n_manifestations=uniqueN(manifestation_key),n_sources=uniqueN(source)),
              by=.(identifier_type,identifier_value)]
shared <- groups[n_manifestations>1L & n_sources>1L]
fwrite(shared,file.path(output_dir,"shared_identifier_groups.csv"))

reg_unique <- unique(reg[,.(manifestation_key,source,identifier_type,identifier_value)])
shared_reg <- reg_unique[shared,on=.(identifier_type,identifier_value),nomatch=0L]
if (nrow(shared_reg)) {
  left <- shared_reg[,.(identifier_type,identifier_value,
                       record_i=manifestation_key,source_i=source)]
  right <- shared_reg[,.(identifier_type,identifier_value,
                        record_j=manifestation_key,source_j=source)]
  raw_pairs <- merge(
    left,right,
    by=c("identifier_type","identifier_value"),
    allow.cartesian=TRUE,
    sort=FALSE
  )
  raw_pairs <- raw_pairs[source_i != source_j & record_i != record_j]
  raw_pairs[, `:=`(
    lo=pmin(record_i,record_j),
    hi=pmax(record_i,record_j)
  )]
  raw_pairs <- unique(raw_pairs[,.(record_i=lo,record_j=hi,identifier_type,identifier_value)])
} else {
  raw_pairs <- data.table(
    record_i=character(),record_j=character(),
    identifier_type=character(),identifier_value=character()
  )
}

pair_ev <- raw_pairs[, .(
  namespaces=paste(sort(unique(identifier_type)),collapse="|"),
  shared_identifiers=paste(sort(unique(paste(identifier_type,identifier_value,sep="="))),collapse="|"),
  guard_hit=any(paste(identifier_type,identifier_value,sep="::") %in% guard_keys)
),by=.(record_i,record_j)]
pair_ev[,families:=vapply(strsplit(namespaces,"\\|"),function(z) {
  z[z %in% c("openalex","mag")] <- "openalex_mag"
  paste(sort(unique(z)),collapse="|")
},character(1))]
pair_ev[,n_independent_families:=lengths(strsplit(families,"\\|"))]
if (nrow(raw_pairs) && anyDuplicated(paste(raw_pairs$record_i,raw_pairs$record_j,raw_pairs$identifier_type,raw_pairs$identifier_value,sep="::"))) {
  stop("Raw identifier pair evidence is unexpectedly duplicated",call.=FALSE)
}

lookup <- meta[,.(manifestation_key,idx,source,title_norm,year)]
setkey(lookup,manifestation_key)
pi <- lookup[pair_ev,on=.(manifestation_key=record_i)]
setnames(pi,c("idx","source","title_norm","year"),c("idx_i","source_i","title_i","year_i"))
pj <- lookup[pi,on=.(manifestation_key=record_j)]
setnames(pj,c("idx","source","title_norm","year"),c("idx_j","source_j","title_j","year_j"))
pair_ev <- pj
if (anyNA(pair_ev$idx_i)||anyNA(pair_ev$idx_j)) stop("Identifier pair failed metadata mapping",call.=FALSE)

pair_ev[,title_similarity:=mapply(title_similarity_lv,title_i,title_j)]
pair_ev[,year_diff:=ifelse(!is.na(year_i)&!is.na(year_j),abs(year_i-year_j),NA_real_)]
pair_ev[,year_compatible:=is.na(year_diff)|year_diff<=1L]
pair_ev[,involves_appended:=idx_i>prior_n|idx_j>prior_n]
pair_ev[,safe_identifier_preresolve:=FALSE]
pair_ev[
  !guard_hit & year_compatible & !is.na(title_similarity) &
    n_independent_families>=2L & title_similarity>=0.65,
  safe_identifier_preresolve:=TRUE
]
pair_ev[
  !guard_hit & year_compatible & !is.na(title_similarity) &
    n_independent_families==1L & families=="doi" & title_similarity>=0.90,
  safe_identifier_preresolve:=TRUE
]
pair_ev[
  !guard_hit & year_compatible & !is.na(title_similarity) &
    n_independent_families==1L & families %in% c("pmid","openalex_mag") & title_similarity>=0.65,
  safe_identifier_preresolve:=TRUE
]

fwrite(pair_ev,file.path(output_dir,"identifier_candidate_pairs.csv"))
safe <- pair_ev[safe_identifier_preresolve==TRUE & involves_appended==TRUE]
safe[,pair_key:=paste(pmin(idx_i,idx_j),pmax(idx_i,idx_j),sep="::")]
if (anyDuplicated(safe$pair_key)) stop("Safe identifier edge keys are duplicated",call.=FALSE)
fwrite(safe,file.path(output_dir,"safe_identifier_edges.csv"))

audit <- list(
  schema="living-evidence-map-workflow01-identifier-assist-v1",
  status="success",
  source_count=length(paths),
  source_manifestations=nrow(meta),
  prior_manifestations=prior_n,
  appended_manifestations=nrow(meta)-prior_n,
  identifier_rows=nrow(reg),
  manifestations_with_identifier=uniqueN(reg$manifestation_key),
  shared_identifier_groups=nrow(shared),
  cross_source_identifier_pairs=nrow(pair_ev),
  guard_registry_entries=nrow(guards),
  guarded_pairs=sum(pair_ev$guard_hit),
  safe_incremental_edges=nrow(safe),
  safe_multi_family_edges=sum(safe$n_independent_families>=2L),
  safe_single_family_edges=sum(safe$n_independent_families==1L),
  thresholds=list(
    multi_family_title_similarity=0.65,
    doi_only_title_similarity=0.90,
    pmid_or_openalex_mag_only_title_similarity=0.65,
    maximum_year_difference=1L
  ),
  automatic_production_merges_performed=0L
)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),
           file.path(output_dir,"identifier_assist_summary.json"))
cat(sprintf("PASS: identifier assist built %d safe incremental edges from %d cross-source identifier pairs\n",
            nrow(safe),nrow(pair_ev)))
