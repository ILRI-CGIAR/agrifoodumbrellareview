#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(data.table)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
cluster_map_path <- arg("--cluster-map")
strip_path <- arg("--strip-actions")
repairs_path <- arg("--data-quality-repairs",NULL)
repair_audit_path <- arg("--repair-audit",NULL)

input_manifest_path <- arg("--input-manifest",NULL)
input_pos <- which(args == "--input")
inputs <- if(length(input_pos)) vapply(input_pos,function(i){
  if(i==length(args)) stop("Missing value after --input",call.=FALSE)
  args[[i+1L]]
},character(1)) else character()

if(!is.null(input_manifest_path)){
  if(!file.exists(input_manifest_path)) stop(sprintf("Input manifest not found: %s",input_manifest_path),call.=FALSE)
  im <- fromJSON(input_manifest_path,simplifyVector=FALSE)
  if(!identical(im$schema,"living-evidence-map-workflow01-source-inputs-v1")) stop("Unsupported source-input manifest schema",call.=FALSE)
  if(is.null(im$sources) || !length(im$sources) || is.null(names(im$sources))) stop("Source-input manifest contains no sources",call.=FALSE)
  source_paths <- vapply(im$sources,function(z)as.character(z[[1L]]),character(1))
} else if(length(inputs)){
  if(any(!grepl("^[^=]+=",inputs))) stop("Each --input must be source=path",call.=FALSE)
  source_names <- sub("=.*$","",inputs)
  source_paths <- sub("^[^=]+=","",inputs)
  names(source_paths) <- source_names
  if(anyDuplicated(source_names)) stop("Each source may be supplied only once",call.=FALSE)
} else {
  source_paths <- c(
    lens=arg("--lens"),
    scopus=arg("--scopus"),
    openalex=arg("--openalex"),
    agricola=arg("--agricola"),
    wos=arg("--wos")
  )
  if(any(vapply(source_paths,is.null,logical(1)))) {
    stop("Provide --input-manifest, repeated --input source=path arguments, or all five legacy source flags",call.=FALSE)
  }
}

run_id <- arg("--run-id")
output_path <- arg("--output")
manifest_path <- arg("--manifest")
required <- list(cluster_map_path,run_id,output_path,manifest_path)
if(any(vapply(required,is.null,logical(1))) || !length(source_paths)) stop("Required: --cluster-map, at least one source input, --run-id --output --manifest",call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
scalar <- function(x){
  if(is.null(x)||!length(x)) return(NULL)
  y <- trimws(as.character(x[[1L]]))
  if(!nzchar(y)) NULL else y
}
clean_text <- function(x){
  x <- scalar(x)
  if(is.null(x)) return(NULL)
  x <- gsub("[[:space:]]+"," ",x)
  x <- trimws(x)
  if(!nzchar(x)) NULL else x
}
norm_doi <- function(x){
  x <- clean_text(x)
  if(is.null(x)) return(NULL)
  x <- tolower(x)
  x <- sub("^https?://(dx\\.)?doi\\.org/","",x,perl=TRUE)
  x <- sub("^doi:\\s*","",x,perl=TRUE)
  x <- sub("[?#].*$","",x,perl=TRUE)
  x <- sub("[.,;:]+$","",x,perl=TRUE)
  if(!nzchar(x)) NULL else x
}
source_kind <- function(r){
  if(is.list(r$lens)) return("lens")
  p <- scalar((r$source %||% list())$provider)
  if(is.null(p)) stop("Source provider is missing",call.=FALSE)
  if(identical(p,"agricola_via_europe_pmc")) return("agricola")
  if(identical(p,"wos_starter")) return("wos")
  if(!grepl("^[a-z0-9][a-z0-9_-]*$",p)) stop(sprintf("Invalid source provider slug: %s",p),call.=FALSE)
  p
}
source_record_id <- function(r){
  src <- source_kind(r)
  if(src=="lens") return(as.character((r$identity %||% list())$lens_id %||% (r$identity %||% list())$record_id %||% ""))
  as.character((r$sidecar_identity %||% list())$sidecar_record_id %||% "")
}
field <- function(r,name){
  src <- source_kind(r)
  if(src=="lens"){
    can <- r$canonical %||% list()
    raw <- (r$lens %||% list())$raw_payload %||% list()
    if(name=="title") return(clean_text(can$title %||% raw$title))
    if(name=="abstract") return(clean_text(can$abstract %||% raw$abstract))
    if(name=="doi"){
      d <- norm_doi(can$doi)
      if(!is.null(d)) return(d)
      ids <- raw$external_ids %||% list()
      for(z in ids) if(is.list(z) && identical(tolower(as.character(z$type %||% "")),"doi")){
        d <- norm_doi(z$value); if(!is.null(d)) return(d)
      }
      return(NULL)
    }
    if(name=="authors") return(can$authors %||% raw$authors)
    if(name=="year") return(can$year %||% raw$year_published %||% raw$date_published)
    if(name=="journal"){
      s <- can$source %||% raw$source
      if(is.list(s)) return(clean_text(s$title))
      return(clean_text(s))
    }
    if(name=="volume") return(clean_text(can$volume %||% raw$volume))
    if(name=="issue") return(clean_text(can$issue %||% raw$issue))
    if(name=="pages") return(clean_text(can$pages %||% raw$pages))
  } else {
    m <- r$mapped_fields %||% list()
    if(name=="title") return(clean_text(m$title))
    if(name=="abstract") return(clean_text(m$abstract))
    if(name=="doi") return(norm_doi(m$doi %||% (r$sidecar_identity %||% list())$doi))
    if(name=="authors") return(m$authors %||% m$first_author)
    if(name=="year") return(m$year %||% m$publication_date)
    if(name=="journal") return(clean_text(m$source %||% m$journal))
    if(name=="volume") return(clean_text(m$volume))
    if(name=="issue") return(clean_text(m$issue))
    if(name=="pages") return(clean_text(m$pages %||% m$article_number))
  }
  NULL
}
manifestation_metadata <- function(r){
  src <- source_kind(r)
  if(src=="lens"){
    can <- r$canonical %||% list()
    raw <- (r$lens %||% list())$raw_payload %||% list()
    return(list(
      source_identity=list(
        lens_id=scalar((r$identity %||% list())$lens_id %||% can$lens_id)
      ),
      author_keywords=can$keywords %||% raw$keywords %||% NULL,
      indexing_terms=NULL,
      publication_type=can$publication_type %||% raw$publication_type %||% NULL,
      publication_date=clean_text(raw$date_published),
      language=NULL,
      identifiers=list(
        issn=NULL,eissn=NULL,issn_l=NULL,pmid=NULL,
        scopus_eid=NULL,scopus_id=NULL,openalex_id=NULL,agricola_id=NULL,wos_uid=NULL
      ),
      structured_authors=can$authors %||% raw$authors %||% NULL,
      affiliations_or_institutions=NULL,
      source_specific=list()
    ))
  }

  m <- r$mapped_fields %||% list()
  sid <- r$sidecar_identity %||% list()

  if(src=="scopus"){
    return(list(
      source_identity=list(scopus_eid=sid$scopus_eid %||% NULL,scopus_id=sid$scopus_id %||% NULL),
      author_keywords=NULL,
      indexing_terms=NULL,
      publication_type=m$publication_type %||% NULL,
      publication_date=m$publication_date %||% NULL,
      language=NULL,
      identifiers=list(
        issn=NULL,eissn=NULL,issn_l=NULL,pmid=NULL,
        scopus_eid=sid$scopus_eid %||% NULL,scopus_id=sid$scopus_id %||% NULL,
        openalex_id=NULL,agricola_id=NULL,wos_uid=NULL
      ),
      structured_authors=m$authors %||% NULL,
      affiliations_or_institutions=m$affiliations %||% NULL,
      source_specific=list(first_author_only=TRUE)
    ))
  }

  if(src=="openalex"){
    return(list(
      source_identity=list(openalex_id=sid$openalex_id %||% NULL),
      author_keywords=NULL,
      indexing_terms=m$indexing_terms %||% NULL,
      publication_type=m$publication_type %||% NULL,
      publication_date=m$publication_date %||% NULL,
      language=NULL,
      identifiers=list(
        issn=NULL,eissn=NULL,
        issn_l=(m$primary_location %||% list())$issn_l %||% NULL,
        pmid=NULL,scopus_eid=NULL,scopus_id=NULL,
        openalex_id=sid$openalex_id %||% NULL,agricola_id=NULL,wos_uid=NULL
      ),
      structured_authors=m$authors %||% NULL,
      affiliations_or_institutions=m$institutions %||% NULL,
      source_specific=list(
        open_access=m$open_access %||% NULL,
        primary_location=m$primary_location %||% NULL
      )
    ))
  }

  if(src=="agricola"){
    return(list(
      source_identity=list(agricola_id=sid$agricola_id %||% NULL,europe_pmc_source=sid$europe_pmc_source %||% NULL),
      author_keywords=m$author_keywords %||% NULL,
      indexing_terms=NULL,
      publication_type=m$publication_type %||% NULL,
      publication_date=m$first_publication_date %||% NULL,
      language=m$language %||% NULL,
      identifiers=list(
        issn=NULL,eissn=NULL,issn_l=NULL,pmid=NULL,
        scopus_eid=NULL,scopus_id=NULL,openalex_id=NULL,
        agricola_id=sid$agricola_id %||% NULL,wos_uid=NULL
      ),
      structured_authors=m$authors %||% NULL,
      affiliations_or_institutions=m$affiliation %||% NULL,
      source_specific=list(author_string=m$author_string %||% NULL)
    ))
  }

  if(src=="wos"){
    return(list(
      source_identity=list(wos_uid=sid$wos_uid %||% NULL),
      author_keywords=m$author_keywords %||% NULL,
      indexing_terms=NULL,
      publication_type=m$publication_type %||% NULL,
      publication_date=NULL,
      language=NULL,
      identifiers=list(
        issn=m$issn %||% NULL,eissn=m$eissn %||% NULL,issn_l=NULL,pmid=m$pmid %||% NULL,
        scopus_eid=NULL,scopus_id=NULL,openalex_id=NULL,agricola_id=NULL,
        wos_uid=sid$wos_uid %||% NULL
      ),
      structured_authors=m$authors %||% NULL,
      affiliations_or_institutions=NULL,
      source_specific=list(
        source_types=m$source_types %||% NULL,
        times_cited_wos=m$times_cited_wos %||% NULL
      )
    ))
  }

  # Generic sidecar/RIS manifestation. This preserves additive metadata but
  # does not alter any deduplication logic or legacy source semantics.
  return(list(
    source_identity=list(source_record_id=sid$source_record_id %||% sid$sidecar_record_id %||% NULL),
    author_keywords=m$author_keywords %||% NULL,
    indexing_terms=m$indexing_terms %||% NULL,
    publication_type=m$publication_type %||% NULL,
    publication_date=m$publication_date %||% NULL,
    language=m$language %||% NULL,
    identifiers=list(
      issn=m$issn %||% NULL,
      eissn=m$eissn %||% NULL,
      issn_l=m$issn_l %||% NULL,
      pmid=m$pmid %||% NULL,
      scopus_eid=NULL,scopus_id=NULL,openalex_id=NULL,agricola_id=NULL,wos_uid=NULL,
      isbn=m$isbn %||% NULL
    ),
    structured_authors=m$structured_authors %||% m$authors %||% NULL,
    affiliations_or_institutions=m$affiliations %||% m$institutions %||% NULL,
    source_specific=r$source_specific %||% list()
  ))
}

author_strings <- function(x){
  if(is.null(x)||!length(x)) return(character())
  one <- function(z){
    if(is.character(z)) return(clean_text(z) %||% "")
    if(is.list(z)){
      display <- clean_text(z$display_name %||% z$name %||% z$full_name)
      if(!is.null(display)) return(display)
      surname <- clean_text(z$surname %||% z$last_name %||% z$family) %||% ""
      given <- clean_text(z$given_name %||% z$first_name %||% z$given) %||% ""
      return(trimws(paste(given,surname)))
    }
    ""
  }
  if(is.character(x)) vals <- as.character(x) else vals <- vapply(x,one,character(1))
  vals <- trimws(vals)
  vals[nzchar(vals)]
}
year_value <- function(x){
  s <- as.character(x %||% "")
  m <- regexpr("(18|19|20|21)[0-9]{2}",s,perl=TRUE)
  if(m[[1L]]<0L) return(NULL)
  as.integer(regmatches(s,m)[[1L]])
}
normalise_publication_date <- function(x){
  x <- clean_text(x)
  if(is.null(x)) return(NULL)
  if(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",x)) return(x)
  if(grepl("^[0-9]{4}-[0-9]{2}$",x)) return(x)
  if(grepl("^[0-9]{4}$",x)) return(x)
  m <- regexpr("(18|19|20|21)[0-9]{2}(-[0-9]{2})?(-[0-9]{2})?",x,perl=TRUE)
  if(m[[1L]]<0L) return(NULL)
  regmatches(x,m)[[1L]]
}

pick_publication_date <- function(mans, manifestation_keys){
  vals <- character()
  keys <- character()
  specificity <- integer()

  for(i in seq_along(mans)){
    x <- ((mans[[i]]$manifestation_metadata %||% list())$publication_date) %||% NULL
    if(is.null(x) || !length(x)) next
    for(v in as.character(unlist(x,use.names=FALSE))){
      d <- normalise_publication_date(v)
      if(is.null(d)) next
      vals <- c(vals,d)
      keys <- c(keys,manifestation_keys[[i]])
      specificity <- c(specificity,nchar(d))
    }
  }

  if(!length(vals)){
    return(list(value=NULL,provenance=list(selection="prefer_most_specific_iso_then_modal",source_keys=character())))
  }

  best_spec <- max(specificity)
  keep <- specificity==best_spec
  v <- vals[keep]
  k <- keys[keep]
  freq <- table(v)
  best_n <- max(freq)
  candidates <- sort(names(freq)[freq==best_n])
  chosen <- candidates[[1L]]
  contributing <- unique(k[v==chosen])

  list(
    value=chosen,
    provenance=list(
      selection="prefer_most_specific_iso_then_modal",
      source_keys=unname(contributing)
    )
  )
}

normalise_issn <- function(x){
  x <- clean_text(x)
  if(is.null(x)) return(NULL)
  y <- toupper(gsub("[^0-9X]","",x))
  if(nchar(y)==8L) return(paste0(substr(y,1L,4L),"-",substr(y,5L,8L)))
  x
}

normalise_pmid <- function(x){
  x <- clean_text(x)
  if(is.null(x)) return(NULL)
  y <- gsub("^PMID[: ]*","",toupper(x))
  y <- gsub("[^0-9]","",y)
  if(!nzchar(y)) NULL else y
}

collect_identifier_values <- function(mans, manifestation_keys, identifier_name, normaliser, selection_label){
  values <- character()
  norm_seen <- new.env(hash=TRUE,parent=emptyenv())
  contributors <- character()

  for(i in seq_along(mans)){
    ids <- (mans[[i]]$manifestation_metadata %||% list())$identifiers %||% list()
    x <- ids[[identifier_name]] %||% NULL
    if(is.null(x) || !length(x)) next
    raw_vals <- as.character(unlist(x,use.names=FALSE))
    if(!length(raw_vals)) next

    contributed <- FALSE
    for(v in raw_vals){
      z <- normaliser(v)
      if(is.null(z) || !nzchar(z)) next
      key <- tolower(z)
      if(!exists(key,envir=norm_seen,inherits=FALSE)){
        assign(key,TRUE,envir=norm_seen)
        values <- c(values,z)
        contributed <- TRUE
      }
    }
    if(contributed) contributors <- c(contributors,manifestation_keys[[i]])
  }

  list(
    value=unname(values),
    provenance=list(
      selection=selection_label,
      source_keys=unname(unique(contributors))
    )
  )
}

collect_manifestation_metadata_values <- function(mans, manifestation_keys, field_name, selection_label){
  values <- character()
  norm_seen <- new.env(hash=TRUE,parent=emptyenv())
  contributors <- character()

  for(i in seq_along(mans)){
    x <- ((mans[[i]]$manifestation_metadata %||% list())[[field_name]]) %||% NULL
    if(is.null(x) || !length(x)) next
    raw_vals <- as.character(unlist(x,use.names=FALSE))
    raw_vals <- gsub("[[:space:]]+"," ",trimws(raw_vals))
    raw_vals <- raw_vals[!is.na(raw_vals) & nzchar(raw_vals)]
    if(!length(raw_vals)) next

    contributed <- FALSE
    for(v in raw_vals){
      key <- tolower(v)
      if(!exists(key,envir=norm_seen,inherits=FALSE)){
        assign(key,TRUE,envir=norm_seen)
        values <- c(values,v)
        contributed <- TRUE
      }
    }
    if(contributed) contributors <- c(contributors,manifestation_keys[[i]])
  }

  list(
    value=unname(values),
    provenance=list(
      selection=selection_label,
      source_keys=unname(unique(contributors))
    )
  )
}

collect_author_keywords <- function(mans, manifestation_keys){
  values <- character()
  norm_seen <- new.env(hash=TRUE,parent=emptyenv())
  contributors <- character()

  for(i in seq_along(mans)){
    kws <- ((mans[[i]]$manifestation_metadata %||% list())$author_keywords) %||% NULL
    if(is.null(kws) || !length(kws)) next
    raw_vals <- as.character(unlist(kws,use.names=FALSE))
    raw_vals <- gsub("[[:space:]]+"," ",trimws(raw_vals))
    raw_vals <- raw_vals[!is.na(raw_vals) & nzchar(raw_vals)]
    if(!length(raw_vals)) next

    contributed <- FALSE
    for(v in raw_vals){
      key <- tolower(v)
      if(!exists(key,envir=norm_seen,inherits=FALSE)){
        assign(key,TRUE,envir=norm_seen)
        values <- c(values,v)
        contributed <- TRUE
      }
    }
    if(contributed) contributors <- c(contributors,manifestation_keys[[i]])
  }

  list(
    value=unname(values),
    provenance=list(
      selection="union_distinct_case_insensitive",
      source_keys=unname(unique(contributors))
    )
  )
}

modal_pick <- function(values, keys, normalise=function(x)x){
  ok <- !vapply(values,is.null,logical(1))
  if(!any(ok)) return(list(value=NULL,source_key=NULL))
  vals <- values[ok]; ks <- keys[ok]
  disp <- vapply(vals,function(v){
    if(is.null(v) || length(v)==0L) return("")
    z <- if(is.list(v)||length(v)>1L) paste(as.character(unlist(v,use.names=FALSE)),collapse="; ") else as.character(v[[1L]])
    if(length(z)==0L || is.na(z[[1L]])) "" else as.character(z[[1L]])
  },character(1))
  norm <- vapply(disp,function(v){
    z <- normalise(v)
    if(is.null(z)) "" else as.character(z)
  },character(1))
  keep <- nzchar(norm)
  if(!any(keep)) return(list(value=NULL,source_key=NULL))
  vals <- vals[keep]; ks <- ks[keep]; disp <- disp[keep]; norm <- norm[keep]
  freq <- table(norm)
  bestn <- max(freq)
  candidates <- names(freq)[freq==bestn]
  ix <- which(norm %in% candidates)
  lens <- nchar(disp[ix],type="chars")
  best <- ix[order(-lens,ks[ix])][1L]
  list(value=vals[[best]],source_key=ks[[best]])
}

cluster_map <- fread(cluster_map_path,na.strings=c("","NA"))
need <- c("source","source_record_id","cluster_id","cluster_size")
if(length(setdiff(need,names(cluster_map)))) stop("Cluster map missing required columns",call.=FALSE)
cluster_map[,key:=paste(source,source_record_id,sep="::")]
if(anyDuplicated(cluster_map$key)) stop("Cluster map has duplicate source manifestation keys",call.=FALSE)

strip_keys <- character()
strip_audit <- list()
if(!is.null(strip_path) && file.exists(strip_path)){
  lines <- readLines(strip_path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if(length(lines)){
    ss <- lapply(lines,fromJSON,simplifyVector=FALSE)
    strip_keys <- unique(vapply(ss,function(z)paste(z$source,z$source_record_id,sep="::"),character(1)))
    strip_audit <- setNames(ss,strip_keys)
  }
}


read_jsonl <- function(path){
  if(is.null(path) || !file.exists(path)) return(list())
  lines <- readLines(path,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  lapply(lines,fromJSON,simplifyVector=FALSE)
}
repairs <- read_jsonl(repairs_path)
allowed_repair_actions <- c("strip_abstract","replace_abstract","set_doi","set_title","set_canonical_preference")
repair_by_key <- list()
repair_application <- vector("list",length(repairs))
if(length(repairs)){
  for(i in seq_along(repairs)){
    r <- repairs[[i]]
    action <- as.character(r$action %||% "")
    if(!(action %in% allowed_repair_actions)) stop(sprintf("Unsupported data-quality repair action: %s",action),call.=FALSE)
    src <- as.character(r$source %||% "")
    rid <- as.character(r$source_record_id %||% "")
    if(!nzchar(src)||!nzchar(rid)) stop(sprintf("Repair %d lacks immutable source identity",i),call.=FALSE)
    key <- paste(src,rid,sep="::")
    repair_by_key[[key]] <- c(repair_by_key[[key]],list(r))
    repair_application[[i]] <- list(
      review_case_id=as.character(r$review_case_id %||% ""),
      source=src,
      source_record_id=rid,
      action=action,
      value=if(is.null(r$value)) NULL else r$value,
      status="pending"
    )
  }
}
if(is.null(repair_audit_path)) repair_audit_path <- file.path(dirname(manifest_path),"data_quality_repair_application.jsonl")

records <- new.env(hash=TRUE,parent=emptyenv())
read_source <- function(path,expected){
  con <- file(path,"rt",encoding="UTF-8"); on.exit(close(con),add=TRUE)
  n <- 0L
  repeat{
    lines <- readLines(con,n=500L,warn=FALSE)
    if(!length(lines)) break
    for(line in lines){
      if(!nzchar(trimws(line))) next
      r <- fromJSON(line,simplifyVector=FALSE)
      src <- source_kind(r)
      if(!identical(src,expected)) stop(sprintf("Expected %s, found %s",expected,src),call.=FALSE)
      rid <- source_record_id(r)
      if(!nzchar(rid)) stop(sprintf("%s record missing source ID",expected),call.=FALSE)
      key <- paste(src,rid,sep="::")
      if(exists(key,records,inherits=FALSE)) stop(sprintf("Duplicate source record key: %s",key),call.=FALSE)
      assign(key,r,records); n <- n+1L
    }
  }
  n
}
source_counts <- vapply(names(source_paths),function(src)read_source(source_paths[[src]],src),integer(1))
all_keys <- ls(records,all.names=TRUE)
missing <- setdiff(cluster_map$key,all_keys)
extra <- setdiff(all_keys,cluster_map$key)
if(length(missing)) stop(sprintf("%d cluster-map manifestations are absent from source records",length(missing)),call.=FALSE)
if(length(extra)) stop(sprintf("%d source records are absent from cluster map",length(extra)),call.=FALSE)

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
out <- file(output_path,"wt",encoding="UTF-8")
on.exit(close(out),add=TRUE)
clusters <- split(cluster_map,cluster_map$cluster_id)
manifestations_total <- 0L
duplicate_clusters <- 0L
stripped_manifestations <- 0L
missing_abstract_manifestations <- 0L

for(cid in sort(names(clusters))){
  cm <- clusters[[cid]]
  setorder(cm,source,source_record_id)
  mans <- vector("list",nrow(cm))
  preferred_flags <- logical(nrow(cm))
  keys <- cm$key
  for(i in seq_len(nrow(cm))){
    r <- get(keys[[i]],records,inherits=FALSE)
    title <- field(r,"title")
    a <- field(r,"abstract")
    doi <- field(r,"doi")
    preferred <- FALSE
    applied_repairs <- list()

    # Automatic strip actions and explicit human repairs are both immutable,
    # source-manifestation-specific transformations.
    stripped <- keys[[i]] %in% strip_keys
    if(stripped) a <- NULL

    rs <- repair_by_key[[keys[[i]]]]
    if(length(rs)){
      for(rr in rs){
        action <- as.character(rr$action)
        before <- switch(action,
          strip_abstract=a,
          replace_abstract=a,
          set_doi=doi,
          set_title=title,
          set_canonical_preference=preferred
        )
        if(action=="strip_abstract"){
          a <- NULL
          stripped <- TRUE
        } else if(action=="replace_abstract"){
          a <- clean_text(rr$value)
        } else if(action=="set_doi"){
          doi <- norm_doi(rr$value)
        } else if(action=="set_title"){
          title <- clean_text(rr$value)
        } else if(action=="set_canonical_preference"){
          preferred <- TRUE
        }
        after <- switch(action,
          strip_abstract=a,
          replace_abstract=a,
          set_doi=doi,
          set_title=title,
          set_canonical_preference=preferred
        )
        applied_repairs[[length(applied_repairs)+1L]] <- list(
          review_case_id=as.character(rr$review_case_id %||% ""),
          action=action,
          supplied_value=if(is.null(rr$value)) NULL else rr$value,
          before=before,
          after=after,
          reason=rr$reason %||% NULL
        )
        # Mark the exact ledger item as applied.
        hit <- which(vapply(repair_application,function(z)
          identical(z$review_case_id,as.character(rr$review_case_id %||% "")) &&
          identical(z$source,as.character(cm$source[[i]])) &&
          identical(z$source_record_id,as.character(cm$source_record_id[[i]])) &&
          identical(z$action,action),logical(1)))
        if(length(hit)!=1L) stop(sprintf("Could not uniquely map repair application for %s %s",keys[[i]],action),call.=FALSE)
        repair_application[[hit]]$status <- "applied"
        repair_application[[hit]]$cluster_id <- cid
      }
    }

    if(stripped) stripped_manifestations <- stripped_manifestations+1L
    if(is.null(a)) missing_abstract_manifestations <- missing_abstract_manifestations+1L
    preferred_flags[[i]] <- preferred
    mans[[i]] <- list(
      source=as.character(cm$source[[i]]),
      source_record_id=as.character(cm$source_record_id[[i]]),
      title=title,
      abstract=a,
      doi=doi,
      authors=author_strings(field(r,"authors")),
      year=year_value(field(r,"year")),
      journal=field(r,"journal"),
      volume=field(r,"volume"),
      issue=field(r,"issue"),
      pages=field(r,"pages"),
      manifestation_metadata=manifestation_metadata(r),
      abstract_stripped=stripped,
      abstract_strip_provenance=if(keys[[i]] %in% strip_keys) strip_audit[[keys[[i]]]] else NULL
    )
  }
  manifestations_total <- manifestations_total+length(mans)
  if(length(mans)>1L) duplicate_clusters <- duplicate_clusters+1L
  mk <- vapply(mans,function(m)paste(m$source,m$source_record_id,sep=":"),character(1))
  preferred_ix <- which(preferred_flags)

  # Human canonical-preference decisions are pairwise and can legitimately
  # identify more than one preferred manifestation within a deduplicated work.
  # Treat them as a preferred subset, not as mutually exclusive single-record
  # choices. For each field, select deterministically within that subset when
  # it contains usable values; otherwise fall back to the full cluster.
  pick_field <- function(name,normalise=function(x)x,transform=function(x)x){
    if(length(preferred_ix)){
      vals <- lapply(mans[preferred_ix],`[[`,name)
      keys <- mk[preferred_ix]
      usable <- vapply(vals,function(v){
        if(is.null(v) || length(v)==0L) return(FALSE)
        z <- paste(as.character(unlist(v,use.names=FALSE)),collapse=" ")
        nzchar(trimws(z))
      },logical(1))
      if(any(usable)){
        z <- modal_pick(vals[usable],keys[usable],normalise)
        z$value <- transform(z$value)
        z$selection <- if(sum(usable)==1L) "human_canonical_preference" else "human_preferred_subset_modal"
        return(z)
      }
    }
    z <- modal_pick(lapply(mans,`[[`,name),mk,normalise)
    z$selection <- "deterministic_modal"
    z
  }
  title_pick <- pick_field("title",function(x)tolower(gsub("[^[:alnum:]]+","",x)))
  abstract_pick <- pick_field("abstract",function(x)tolower(gsub("[[:space:]]+"," ",x)))
  doi_pick <- pick_field("doi",norm_doi)
  authors_pick <- pick_field("authors",function(x)tolower(gsub("[^[:alnum:]; ]+","",x)))
  year_pick <- pick_field("year",as.character)
  journal_pick <- pick_field("journal",function(x)tolower(gsub("[^[:alnum:]]+","",x)))
  volume_pick <- pick_field("volume")
  issue_pick <- pick_field("issue")
  pages_pick <- pick_field("pages")
  author_keywords_pick <- collect_author_keywords(mans,mk)
  publication_type_pick <- collect_manifestation_metadata_values(
    mans,mk,"publication_type","union_distinct_source_values_case_insensitive"
  )
  publication_date_pick <- pick_publication_date(mans,mk)
  language_pick <- collect_manifestation_metadata_values(
    mans,mk,"language","union_distinct_explicit_source_values_case_insensitive"
  )
  issn_pick <- collect_identifier_values(mans,mk,"issn",normalise_issn,"union_distinct_normalised_identifiers")
  eissn_pick <- collect_identifier_values(mans,mk,"eissn",normalise_issn,"union_distinct_normalised_identifiers")
  issn_l_pick <- collect_identifier_values(mans,mk,"issn_l",normalise_issn,"union_distinct_normalised_identifiers")
  pmid_pick <- collect_identifier_values(mans,mk,"pmid",normalise_pmid,"union_distinct_normalised_identifiers")
  rec <- list(
    schema_version="living-evidence-map-canonical-v1",
    identity=list(record_id=cid,record_id_type="deduplication_cluster_id"),
    canonical=list(
      title=title_pick$value,
      abstract=abstract_pick$value,
      doi=doi_pick$value,
      authors=if(is.null(authors_pick$value)) character() else authors_pick$value,
      year=year_pick$value,
      journal=journal_pick$value,
      volume=volume_pick$value,
      issue=issue_pick$value,
      pages=pages_pick$value,
      author_keywords=author_keywords_pick$value,
      publication_type=publication_type_pick$value,
      publication_date=publication_date_pick$value,
      language=language_pick$value,
      issn=issn_pick$value,
      eissn=eissn_pick$value,
      issn_l=issn_l_pick$value,
      pmid=pmid_pick$value,
      field_provenance=list(
        title=title_pick$source_key,abstract=abstract_pick$source_key,doi=doi_pick$source_key,
        authors=authors_pick$source_key,year=year_pick$source_key,journal=journal_pick$source_key,
        volume=volume_pick$source_key,issue=issue_pick$source_key,pages=pages_pick$source_key,
        author_keywords=author_keywords_pick$provenance,
        publication_type=publication_type_pick$provenance,
        publication_date=publication_date_pick$provenance,
        language=language_pick$provenance,
        issn=issn_pick$provenance,
        eissn=eissn_pick$provenance,
        issn_l=issn_l_pick$provenance,
        pmid=pmid_pick$provenance
      )    ),
    manifestations=mans,
    provenance=list(
      source_manifestation_count=length(mans)
    ),
    abstract_enrichment=list(
      status="pending_workflow02",
      manifestations_missing_abstract=sum(vapply(mans,function(m)is.null(m$abstract),logical(1)))
    ),
    deduplication=list(
      status=if(length(mans)>1L)"reconciled" else "singleton",
      cluster_id=cid,
      member_count=length(mans),
      final=TRUE
    ),
    screening=list(status="pending"),
    species=list(),
    geography=list(),
    topics=list()
  )
  writeLines(toJSON(rec,auto_unbox=TRUE,null="null",na="null"),out,useBytes=TRUE)
}
close(out); on.exit(NULL,add=FALSE)

unapplied <- which(vapply(repair_application,function(z)!identical(z$status,"applied"),logical(1)))
dir.create(dirname(repair_audit_path),recursive=TRUE,showWarnings=FALSE)
ra <- file(repair_audit_path,"wt",encoding="UTF-8")
if(length(repair_application)) for(z in repair_application) writeLines(toJSON(z,auto_unbox=TRUE,null="null",na="null"),ra,useBytes=TRUE)
close(ra)
if(length(unapplied)) stop(sprintf("%d approved data-quality repairs were not applied",length(unapplied)),call.=FALSE)

manifest <- list(
  schema="living-evidence-map-canonical-manifest-v1",
  canonical_schema_version="living-evidence-map-canonical-v1",
  workflow="01",
  github_run_id=as.character(run_id),
  records=length(clusters),
  source_manifestations=manifestations_total,
  duplicate_clusters=duplicate_clusters,
  singleton_clusters=length(clusters)-duplicate_clusters,
  abstract_strip_actions=length(strip_keys),
  approved_data_quality_repairs=length(repairs),
  applied_data_quality_repairs=length(repair_application)-length(unapplied),
  repair_application_audit=basename(repair_audit_path),
  manifestations_missing_abstract=missing_abstract_manifestations,
  source_counts=as.list(source_counts),
  cluster_map_sha256=digest(file=cluster_map_path,algo="sha256",serialize=FALSE),
  canonical_jsonl_sha256=digest(file=output_path,algo="sha256",serialize=FALSE),
  canonical_jsonl_bytes=unname(file.info(output_path)$size),
  generated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),manifest_path,useBytes=TRUE)
cat(sprintf("PASS: canonical JSONL: %d work records from %d manifestations; %d approved repairs applied; %d strip actions\n",
            manifest$records,manifest$source_manifestations,manifest$applied_data_quality_repairs,manifest$abstract_strip_actions))
