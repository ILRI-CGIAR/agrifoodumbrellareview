#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(jsonlite)
  library(stringi)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

decisions_path <- arg("--decisions")
metadata_path <- arg("--metadata")
output_path <- arg("--output")
audit_path <- arg("--audit")
if (any(vapply(list(decisions_path,metadata_path,output_path,audit_path),is.null,logical(1)))) {
  stop("Required: --decisions --metadata --output --audit",call.=FALSE)
}

x <- fread(decisions_path,na.strings=c("","NA"))
meta <- fread(metadata_path,na.strings=c("","NA"))
need_x <- c("record_i","record_j","rescored_classification","rescored_rule","first_author_match")
need_m <- c("idx","title","title_norm","abstract_hash","author_norm","year","doi_norm")
if (length(setdiff(need_x,names(x)))) stop("Decision input missing required fields",call.=FALSE)
if (length(setdiff(need_m,names(meta)))) stop("Metadata input missing required fields",call.=FALSE)
if (!identical(as.integer(meta$idx),seq_len(nrow(meta)))) stop("Metadata idx is not contiguous",call.=FALSE)

norm_title <- function(v) {
  if (is.na(v) || !nzchar(trimws(v))) return(NA_character_)
  z <- stri_trans_tolower(stri_trans_nfkc(v))
  z <- stri_replace_all_regex(z,"[\\p{P}\\p{S}\\p{Z}\\s]+","")
  if (!nzchar(z)) NA_character_ else z
}
trim_wrapped <- function(v) {
  if (is.na(v)) return(NA_character_)
  z <- trimws(v)
  z <- stri_replace_all_regex(z,'^[\\"“”\'’]+|[\\"“”\'’]+$',"")
  z <- stri_replace_first_regex(z,"(?i)\\s*\\(v[0-9]+(?:\\.[0-9]+)*\\)\\s*$","")
  trimws(stri_replace_all_regex(z,'^[\\"“”\'’]+|[\\"“”\'’]+$',""))
}
strip_wrapper <- function(v) {
  if (is.na(v) || !nzchar(trimws(v))) return(list(type=NA_character_,stripped=NA_character_))
  z <- trimws(v)

  if (stri_detect_regex(z,"(?i)^additional\\s+file\\s*[0-9]+[a-z]?\\s*:")) {
    rem <- stri_replace_first_regex(z,"(?i)^additional\\s+file\\s*[0-9]+[a-z]?\\s*:\\s*","")
    if (stri_detect_regex(rem,"(?i)(?:^|\\.\\s+|\\s+)of\\s+.+$")) {
      rem <- stri_replace_first_regex(rem,"(?is)^.*?(?:^|\\.\\s+|\\s+)of\\s+","")
    }
    return(list(type="additional_file",stripped=trim_wrapped(rem)))
  }

  pats <- list(
    peer_review="(?i)^peer\\s*review\\s*#?\\s*[0-9]+\\s*(?:of|for)\\s*",
    supplementary_material="(?i)^supplementary\\s+(?:material|materials)\\s+from\\s*",
    supporting_information="(?i)^supporting\\s+information\\s+(?:for|of)\\s*:?\\s*",
    decision_letter="(?i)^decision\\s+letter\\s*(?:for|on|of|to)?\\s*:?\\s*",
    author_response="(?i)^author\\s+(?:response|reply)\\s*(?:for|to|on|of)?\\s*:?\\s*",
    dataset="(?i)^(?:data|dataset|data\\s*set)\\s+(?:from|for|of)\\s*:?\\s*"
  )
  for (nm in names(pats)) {
    if (stri_detect_regex(z,pats[[nm]])) {
      rem <- stri_replace_first_regex(z,pats[[nm]],"")
      return(list(type=nm,stripped=trim_wrapped(rem)))
    }
  }
  list(type=NA_character_,stripped=NA_character_)
}
generic_attachment_type <- function(v) {
  if (is.na(v)) return(NA_character_)
  pats <- c(
    supplementary_file="(?i)^\\s*supplement(?:ary)?\\s+file\\s*[0-9]+[a-z]?\\.(?:xlsx?|docx?|csv|pdf|txt|zip)\\s*$",
    data_sheet="(?i)^\\s*data\\s*sheet\\s*[0-9]+[a-z]?\\.(?:xlsx?|docx?|csv|pdf|txt|zip)\\s*$",
    table="(?i)^\\s*table\\s*[0-9]+[a-z]?\\.(?:xlsx?|docx?|csv|pdf|txt)\\s*$",
    image="(?i)^\\s*image\\s*[0-9]+[a-z]?\\.(?:jpe?g|png|tiff?|gif)\\s*$"
  )
  for (nm in names(pats)) if (stri_detect_regex(v,pats[[nm]])) return(nm)
  NA_character_
}
supplement_doi_relation <- function(a,b) {
  if (is.na(a)||is.na(b)||!nzchar(a)||!nzchar(b)) return(FALSE)
  a <- tolower(trimws(a)); b <- tolower(trimws(b))
  ab <- paste0(b,".s"); ba <- paste0(a,".s")
  (startsWith(a,ab) && stri_detect_regex(substr(a,nchar(ab)+1L,nchar(a)),"^[0-9]+$")) ||
    (startsWith(b,ba) && stri_detect_regex(substr(b,nchar(ba)+1L,nchar(b)),"^[0-9]+$"))
}

m <- meta[,.(idx,title,title_norm,abstract_hash,author_norm,year,doi_norm)]
mi <- copy(m); setnames(mi,names(mi)[-1L],paste0(names(mi)[-1L],"_i"))
mj <- copy(m); setnames(mj,names(mj)[-1L],paste0(names(mj)[-1L],"_j"))
z <- mi[x,on=.(idx=record_i)]
setnames(z,"idx","record_i")
z <- mj[z,on=.(idx=record_j)]
setnames(z,"idx","record_j")
if (anyNA(z$title_i) && anyNA(z$title_j)) {
  # Missing titles are allowed, but pair indices must map.
  if (any(!z$record_i %in% meta$idx) || any(!z$record_j %in% meta$idx)) stop("Decision pair failed metadata mapping",call.=FALSE)
}

z[, `:=`(
  title_assist=FALSE,
  title_assist_rule=NA_character_,
  title_assist_wrapper_type=NA_character_,
  title_assist_attachment_type=NA_character_,
  title_assist_stripped_title=NA_character_
)]

review_idx <- which(z$rescored_classification=="review")
for (ii in review_idx) {
  ti <- z$title_i[[ii]]; tj <- z$title_j[[ii]]
  wi <- strip_wrapper(ti); wj <- strip_wrapper(tj)
  gi <- generic_attachment_type(ti); gj <- generic_attachment_type(tj)

  ni <- if (!is.na(wi$stripped)) norm_title(wi$stripped) else NA_character_
  nj <- if (!is.na(wj$stripped)) norm_title(wj$stripped) else NA_character_
  oi <- norm_title(ti); oj <- norm_title(tj)

  wrapper_hit <- FALSE
  wrapper_type <- NA_character_
  stripped <- NA_character_
  if (!is.na(wi$type) && !is.na(ni) && nchar(ni)>=20L && !is.na(oj) && identical(ni,oj)) {
    wrapper_hit <- TRUE; wrapper_type <- wi$type; stripped <- wi$stripped
  } else if (!is.na(wj$type) && !is.na(nj) && nchar(nj)>=20L && !is.na(oi) && identical(nj,oi)) {
    wrapper_hit <- TRUE; wrapper_type <- wj$type; stripped <- wj$stripped
  }

  generic_i <- !is.na(gi); generic_j <- !is.na(gj)
  exactly_one_generic <- xor(generic_i,generic_j)
  exact_abs <- !is.na(z$abstract_hash_i[[ii]]) && !is.na(z$abstract_hash_j[[ii]]) &&
    identical(z$abstract_hash_i[[ii]],z$abstract_hash_j[[ii]])
  author_match <- isTRUE(z$first_author_match[[ii]])
  yi <- z$year_i[[ii]]; yj <- z$year_j[[ii]]
  year_ok <- is.na(yi) || is.na(yj) || abs(yi-yj)<=1L
  doi_parent <- supplement_doi_relation(z$doi_norm_i[[ii]],z$doi_norm_j[[ii]])
  attachment_hit <- exactly_one_generic && exact_abs && author_match && year_ok && doi_parent

  if (wrapper_hit) {
    z$title_assist[[ii]] <- TRUE
    z$title_assist_rule[[ii]] <- "wrapper_stripped_exact_title"
    z$title_assist_wrapper_type[[ii]] <- wrapper_type
    z$title_assist_stripped_title[[ii]] <- stripped
  } else if (attachment_hit) {
    z$title_assist[[ii]] <- TRUE
    z$title_assist_rule[[ii]] <- "generic_attachment_exact_abstract_author_year_parent_doi"
    z$title_assist_attachment_type[[ii]] <- if (generic_i) gi else gj
  }
}

hit <- which(z$title_assist)
if (length(hit)) {
  z[hit, `:=`(
    classification="duplicate",
    rule=paste0("title_assist:",title_assist_rule),
    rescored_classification="duplicate",
    rescored_rule=paste0("title_assist:",title_assist_rule),
    review_route="resolved_title_assist",
    decision_changed=TRUE
  )]
}

orig_names <- names(x)
extra <- c("title_assist","title_assist_rule","title_assist_wrapper_type",
           "title_assist_attachment_type","title_assist_stripped_title")
out <- z[,c(orig_names,extra),with=FALSE]
fwrite(out,output_path)

audit <- list(
  schema="living-evidence-map-workflow01-title-assist-v1",
  status="success",
  input_pairs=nrow(x),
  input_manual_review=sum(x$rescored_classification=="review"),
  automatic_title_duplicates=length(hit),
  wrapper_stripped_exact_title=sum(z$title_assist_rule=="wrapper_stripped_exact_title",na.rm=TRUE),
  generic_attachment_exact_abstract_author_year_parent_doi=sum(
    z$title_assist_rule=="generic_attachment_exact_abstract_author_year_parent_doi",na.rm=TRUE),
  output_manual_review=sum(out$rescored_classification=="review"),
  non_review_pairs_modified=sum(z$title_assist & !(seq_len(nrow(z)) %in% review_idx))
)
if (audit$non_review_pairs_modified != 0L) stop("Title assist modified non-review pairs",call.=FALSE)
writeLines(toJSON(audit,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),audit_path)
cat(sprintf("PASS: title assist resolved %d of %d manual-review pairs; %d remain\n",
            audit$automatic_title_duplicates,audit$input_manual_review,audit$output_manual_review))
