#!/usr/bin/env Rscript
# Exhaustive score stage: copies unchanged matching rules from the transferred
# workflow_01_deduplication_incremental_candidates.R to score all candidate pairs.
# Each pair is assigned deterministically to exactly one shard by row index.
suppressPackageStartupMessages({
  library(jsonlite); library(data.table); library(stringdist); library(digest)
})
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,default=NULL) { i<-match(k,args); if(is.na(i))return(default); if(i==length(args))stop(paste("Missing",k)); args[[i+1L]] }
input_dir<-arg("--input-dir","inputs/w01")
output_dir<-arg("--output-dir","outputs/w01_scored")
shard<-as.integer(arg("--shard"));shards<-as.integer(arg("--shards","24"))
if(is.na(shard)||is.na(shards)||shard<1L||shards<1L||shard>shards)stop("Invalid shard settings")
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
meta<-fread(file.path(input_dir,"normalised_metadata.csv"),na.strings=c("","NA"))
candidate_dt<-fread(file.path(input_dir,"all_candidate_pairs.csv"),na.strings=c("","NA"))
if(nrow(meta)!=46361L||nrow(candidate_dt)!=348692L)stop("Input count mismatch; do not score incomplete corpus")
if(!all(c("record_i","record_j","blocks") %in% names(candidate_dt)))stop("Candidate pair fields missing")
if(any(candidate_dt$record_i<1L|candidate_dt$record_j>nrow(meta)|candidate_dt$record_i==candidate_dt$record_j))stop("Invalid candidate indices")
ix<-seq.int(from=shard,to=nrow(candidate_dt),by=shards)
score_sample<-candidate_dt[ix,]
cat(sprintf("Starting exhaustive shard %d/%d: %d pairs\n",shard,shards,nrow(score_sample)))
tokens <- function(x) if (is.na(x)||!nzchar(x)) character() else strsplit(x," ",fixed=TRUE)[[1L]]
shingles5 <- function(t) if (length(t)<5L) character() else unique(vapply(seq_len(length(t)-4L),function(i)paste(t[i:(i+4L)],collapse=" "),character(1)))
lcs_len <- function(a,b) {
  if (!length(a)||!length(b)) return(0L)
  if (length(a)>length(b)) {tmp<-a;a<-b;b<-tmp}
  prev <- integer(length(b)+1L); cur <- integer(length(b)+1L)
  for (i in seq_along(a)) {
    cur[] <- 0L
    for (j in seq_along(b)) cur[[j+1L]] <- if (identical(a[[i]],b[[j]])) prev[[j]]+1L else max(prev[[j+1L]],cur[[j]])
    tmp<-prev;prev<-cur;cur<-tmp
  }
  prev[[length(b)+1L]]
}
abstract_metrics <- function(a,b) {
  ta<-tokens(a);tb<-tokens(b);shorter<-min(length(ta),length(tb))
  if (!shorter) return(list(lcs=0L,ordered=NA_real_,shingle=NA_real_,strong=FALSE))
  sa<-shingles5(ta);sb<-shingles5(tb)
  sh<-if (!length(sa)||!length(sb)) NA_real_ else length(intersect(sa,sb))/min(length(sa),length(sb))
  l<-lcs_len(ta,tb); oc<-l/shorter
  strong <- (l>=80L && oc>=0.90 && !is.na(sh)&&sh>=0.60) || (l>=60L&&l<=79L&&oc>=0.95&&!is.na(sh)&&sh>=0.75)
  list(lcs=l,ordered=oc,shingle=sh,strong=strong)
}

scored <- vector("list",nrow(score_sample))
for (k in seq_len(nrow(score_sample))) {
  if (k==1L || k%%100L==0L || k==nrow(score_sample)) {
    cat(sprintf("Shard %d/%d: scored %d / %d pairs\n", shard, shards, k, nrow(score_sample))); flush.console()

  }
  i<-score_sample$record_i[[k]];j<-score_sample$record_j[[k]]
  a<-meta[i];b<-meta[j]
  same_doi <- !is.na(a$doi_norm)&&!is.na(b$doi_norm)&&identical(a$doi_norm,b$doi_norm)
  same_family <- !is.na(a$doi_family)&&!is.na(b$doi_family)&&identical(a$doi_family,b$doi_family)
  diff_doi <- !is.na(a$doi_norm)&&!is.na(b$doi_norm)&&!identical(a$doi_norm,b$doi_norm)
  exact_title <- !is.na(a$title_norm)&&!is.na(b$title_norm)&&identical(a$title_norm,b$title_norm)
  tsim <- if (!is.na(a$title_norm)&&!is.na(b$title_norm)) as.numeric(stringsim(a$title_norm,b$title_norm,method="jw",p=0.1)) else NA_real_
  containment <- FALSE
  if (!is.na(a$title_norm)&&!is.na(b$title_norm)) {
    short<-if(nchar(a$title_norm)<=nchar(b$title_norm))a$title_norm else b$title_norm
    long<-if(nchar(a$title_norm)<=nchar(b$title_norm))b$title_norm else a$title_norm
    containment<-nchar(short)>=30L&&grepl(short,long,fixed=TRUE)
  }
  exact_abs <- !is.na(a$abstract_hash)&&!is.na(b$abstract_hash)&&identical(a$abstract_hash,b$abstract_hash)
  exact_author <- !is.na(a$author_norm)&&!is.na(b$author_norm)&&identical(a$author_norm,b$author_norm)
  exact_year <- !is.na(a$year)&&!is.na(b$year)&&identical(a$year,b$year)
  yd <- if (!is.na(a$year)&&!is.na(b$year)) abs(a$year-b$year) else NA_integer_
  blocks<-score_sample$blocks[[k]]
  brA<-grepl("(^|;)bramer_A(;|$)",blocks)
  brB<-grepl("(^|;)bramer_B(;|$)",blocks)
  need_abs <- !exact_abs && !is.na(tsim) && (tsim>=0.90||containment||exact_title)
  am <- if (need_abs && !is.na(a$abstract_norm)&&!is.na(b$abstract_norm)) abstract_metrics(a$abstract_norm,b$abstract_norm) else list(lcs=NA_integer_,ordered=NA_real_,shingle=NA_real_,strong=FALSE)

  auto<-NULL
  if(brA)auto<-"bramer_A"
  else if(brB)auto<-"bramer_B"
  else if(same_doi&&exact_title)auto<-"exact_doi_exact_title"
  else if(exact_title&&exact_abs)auto<-"exact_title_exact_abstract"
  else if(same_doi&&containment)auto<-"exact_doi_title_containment"
  else if(same_doi&&!is.na(tsim)&&tsim>=0.985)auto<-"exact_doi_title_similarity_0.985"
  else if(same_doi&&exact_abs)auto<-"exact_doi_exact_abstract"
  else if(exact_title&&am$strong)auto<-"exact_title_strong_abstract"
  else if(containment&&am$strong)auto<-"title_containment_strong_abstract"
  else if(!is.na(tsim)&&tsim>=0.97&&am$strong)auto<-"title_similarity_0.97_strong_abstract"
  else if(!is.na(tsim)&&tsim>=0.95&&exact_author&&exact_year)auto<-"title_similarity_0.95_exact_author_year"

  cls<-"unresolved";rule<-"candidate_only"
  if(!is.null(auto)) {
    if(diff_doi&&!same_family&&!grepl("^exact_doi_",auto)){cls<-"review";rule<-"material_conflict_different_doi"}
    else{cls<-"duplicate";rule<-auto}
  } else if(exact_abs){cls<-"review";rule<-"exact_abstract_insufficient_metadata"}
  else if(same_family&&diff_doi&&!is.na(tsim)&&tsim>=0.97){cls<-"review";rule<-"doi_family_title_similarity_0.97"}
  else if(exact_title&&!is.na(yd)&&yd<=1L){cls<-"review";rule<-"exact_title_year_within_1"}
  else if(am$strong&&diff_doi&&!same_family){cls<-"review";rule<-"strong_content_material_doi_conflict"}

  scored[[k]]<-data.table(
    record_i=i,record_j=j,source_i=a$source,source_j=b$source,
    title_i=a$title,title_j=b$title,doi_i=a$doi_norm,doi_j=b$doi_norm,
    blocks=blocks,title_similarity=tsim,title_containment=containment,
    exact_title=exact_title,exact_abstract=exact_abs,
    ordered_coverage=am$ordered,shingle_containment=am$shingle,lcs_tokens=am$lcs,
    classification=cls,rule=rule
  )
}
scored_dt<-rbindlist(scored,fill=TRUE)
fwrite(scored_dt,file.path(output_dir,sprintf("scored_shard_%02d.csv",shard)))


if(nrow(scored_dt)!=length(ix))stop("Scoring incomplete")
writeLines(toJSON(list(schema="ilri-w01-exhaustive-shard-v1",shard=shard,shards=shards,
 expected_pairs=length(ix),scored_pairs=nrow(scored_dt),
 classifications=as.list(table(scored_dt$classification)),status="success"),
 auto_unbox=TRUE,pretty=TRUE),file.path(output_dir,sprintf("summary_shard_%02d.json",shard)))
cat(sprintf("PASS W01 exhaustive shard %d/%d: %d candidate pairs scored\n",shard,shards,nrow(scored_dt)))
