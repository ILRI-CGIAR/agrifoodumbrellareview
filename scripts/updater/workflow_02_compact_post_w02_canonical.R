#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}

input_path <- arg("--input")
output_path <- arg("--output")
report_path <- arg("--report")
expected_input_sha <- tolower(arg("--expected-input-sha256",""))

if(is.null(input_path) || is.null(output_path) || is.null(report_path)){
  stop("Required: --input --output --report",call.=FALSE)
}
if(!file.exists(input_path)) stop(sprintf("Input not found: %s",input_path),call.=FALSE)

`%||%` <- function(x,y) if(is.null(x)) y else x
clean_scalar <- function(x){
  if(is.null(x) || !length(x)) return("")
  s <- trimws(as.character(x[[1L]]))
  if(is.na(s)) "" else s
}
stable_json <- function(x) toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA)

input_sha <- digest(file=input_path,algo="sha256",serialize=FALSE)
if(nzchar(expected_input_sha) && !identical(tolower(input_sha),expected_input_sha)){
  stop(sprintf("Input SHA256 mismatch: expected %s, got %s",expected_input_sha,input_sha),call.=FALSE)
}

dir.create(dirname(output_path),recursive=TRUE,showWarnings=FALSE)
dir.create(dirname(report_path),recursive=TRUE,showWarnings=FALSE)

pin <- file(input_path,"rt",encoding="UTF-8")
pout <- file(output_path,"wt",encoding="UTF-8")
on.exit({try(close(pin),silent=TRUE);try(close(pout),silent=TRUE)},add=TRUE)

records <- 0L
manifestations_total <- 0L
record_ids <- character()
source_counts <- integer()
refs_total <- 0L

repeat {
  line <- readLines(pin,n=1L,warn=FALSE)
  if(!length(line)) break
  if(!nzchar(trimws(line))) next

  records <- records + 1L
  r <- fromJSON(line,simplifyVector=FALSE)
  rid <- clean_scalar((r$identity %||% list())$record_id)
  if(!nzchar(rid)) stop(sprintf("Input record %d lacks identity.record_id",records),call.=FALSE)
  if(rid %in% record_ids) stop(sprintf("Duplicate record_id: %s",rid),call.=FALSE)
  record_ids <- c(record_ids,rid)

  mans <- r$manifestations %||% list()
  if(!is.list(mans)) stop(sprintf("%s manifestations is not a list",rid),call.=FALSE)

  refs <- character()
  if(length(mans)){
    refs <- vapply(seq_along(mans),function(i){
      m <- mans[[i]]
      source <- tolower(clean_scalar(m$source))
      source_id <- clean_scalar(m$source_record_id)
      if(!nzchar(source) || !nzchar(source_id)){
        stop(sprintf("%s manifestation %d lacks source/source_record_id",rid,i),call.=FALSE)
      }
      current_n <- if(source %in% names(source_counts)) source_counts[[source]] else 0L
      source_counts[[source]] <<- current_n + 1L
      paste0(source,":",source_id)
    },character(1))
    if(anyDuplicated(refs)){
      dup <- unique(refs[duplicated(refs)])
      stop(sprintf("%s has duplicate manifestation reference(s): %s",rid,paste(dup,collapse=", ")),call.=FALSE)
    }
  }

  manifestations_total <- manifestations_total + length(mans)
  refs_total <- refs_total + length(refs)

  expected <- r
  expected$manifestations <- NULL
  expected$manifestation_refs <- sort(refs)

  # Only the manifestation representation may change.
  original_other <- r
  original_other$manifestations <- NULL
  original_other$manifestation_refs <- NULL
  compacted_other <- expected
  compacted_other$manifestation_refs <- NULL
  if(!identical(stable_json(original_other),stable_json(compacted_other))){
    stop(sprintf("Non-manifestation fields changed while compacting %s",rid),call.=FALSE)
  }

  writeLines(stable_json(expected),pout,useBytes=TRUE)
}
close(pin); close(pout); on.exit(NULL,add=FALSE)

if(records < 1L){
  stop("Lean compaction produced zero canonical records",call.=FALSE)
}
if(refs_total != manifestations_total){
  stop("Manifestation/reference count mismatch",call.=FALSE)
}

# Independent replay validation of the written lean JSONL against the heavy input.
a <- file(input_path,"rt",encoding="UTF-8")
b <- file(output_path,"rt",encoding="UTF-8")
on.exit({try(close(a),silent=TRUE);try(close(b),silent=TRUE)},add=TRUE)
validated <- 0L
repeat {
  la <- readLines(a,n=1L,warn=FALSE)
  lb <- readLines(b,n=1L,warn=FALSE)
  if(!length(la) && !length(lb)) break
  if(!length(la) || !length(lb)) stop("Input/output line count differs",call.=FALSE)
  if(!nzchar(trimws(la)) && !nzchar(trimws(lb))) next
  if(!nzchar(trimws(la)) || !nzchar(trimws(lb))) stop("Input/output blank-line structure differs",call.=FALSE)

  heavy <- fromJSON(la,simplifyVector=FALSE)
  lean <- fromJSON(lb,simplifyVector=FALSE)
  rid_h <- clean_scalar((heavy$identity %||% list())$record_id)
  rid_l <- clean_scalar((lean$identity %||% list())$record_id)
  if(!identical(rid_h,rid_l)) stop(sprintf("record_id changed: %s -> %s",rid_h,rid_l),call.=FALSE)

  mans <- heavy$manifestations %||% list()
  expected_refs <- if(length(mans)) sort(vapply(mans,function(m){
    paste0(tolower(clean_scalar(m$source)),":",clean_scalar(m$source_record_id))
  },character(1))) else character()
  actual_refs <- lean$manifestation_refs %||% list()
  actual_refs <- if(length(actual_refs)) sort(unlist(actual_refs,use.names=FALSE)) else character()
  if(!identical(expected_refs,actual_refs)){
    stop(sprintf("Manifestation references do not exactly replay for %s",rid_h),call.=FALSE)
  }

  heavy$manifestations <- NULL
  heavy$manifestation_refs <- NULL
  lean$manifestation_refs <- NULL
  lean$manifestations <- NULL
  if(!identical(stable_json(heavy),stable_json(lean))){
    stop(sprintf("Canonical/non-manifestation content changed for %s",rid_h),call.=FALSE)
  }
  validated <- validated + 1L
}
close(a); close(b); on.exit(NULL,add=FALSE)
if(validated != records) stop("Independent validation count mismatch",call.=FALSE)

output_sha <- digest(file=output_path,algo="sha256",serialize=FALSE)
input_bytes <- as.numeric(file.info(input_path)$size)
output_bytes <- as.numeric(file.info(output_path)$size)
source_counts <- sort(source_counts,decreasing=TRUE)

report <- list(
  schema="living-evidence-map-post-w02-lean-canonical-compaction-v1",
  status="PASS",
  input_canonical_sha256=input_sha,
  output_lean_canonical_sha256=output_sha,
  canonical_records=records,
  record_ids_unchanged=TRUE,
  non_manifestation_fields_unchanged=TRUE,
  manifestations=manifestations_total,
  manifestation_refs=refs_total,
  manifestation_refs_exact=TRUE,
  source_manifestation_counts=as.list(source_counts),
  input_bytes=input_bytes,
  output_bytes=output_bytes,
  bytes_removed=input_bytes-output_bytes,
  size_reduction_fraction=if(input_bytes>0) 1-(output_bytes/input_bytes) else NA_real_
)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),report_path,useBytes=TRUE)
cat(sprintf("PASS: compacted %d canonical records; %d manifestations -> %d exact references; SHA256=%s\n",
            records,manifestations_total,refs_total,output_sha))
