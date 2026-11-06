#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL){
  i <- match(flag,args)
  if(is.na(i)) return(default)
  if(i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
summary_path <- arg("--summary")
canonical_manifest_path <- arg("--canonical-manifest")
validation_path <- arg("--validation-summary","")
zenodo_receipt_path <- arg("--zenodo-receipt","")
run_id <- arg("--run-id")
out_md <- arg("--out-md")
out_json <- arg("--out-json")
repo <- arg("--repository","thesalmonandthetomato/LivingEvidenceMap")
if(any(vapply(list(summary_path,canonical_manifest_path,run_id,out_md,out_json),is.null,logical(1)))) {
  stop("Required: --summary --canonical-manifest --run-id --out-md --out-json",call.=FALSE)
}
s <- fromJSON(summary_path,simplifyVector=FALSE)
c <- fromJSON(canonical_manifest_path,simplifyVector=FALSE)
v <- if(nzchar(validation_path) && file.exists(validation_path)) fromJSON(validation_path,simplifyVector=FALSE) else NULL
z <- if(nzchar(zenodo_receipt_path) && file.exists(zenodo_receipt_path)) fromJSON(zenodo_receipt_path,simplifyVector=FALSE) else NULL

run_url <- sprintf("https://github.com/%s/actions/runs/%s",repo,run_id)
report <- list(
  schema="living-evidence-map-workflow01-run-report-v1",
  workflow="01",
  github_run_id=as.character(run_id),
  github_run_url=run_url,
  generated_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
  status=as.character(s$status %||% "unknown"),
  source_manifestations=as.integer(c$source_manifestations),
  canonical_records=as.integer(c$records),
  duplicate_clusters=as.integer(c$duplicate_clusters),
  singleton_clusters=as.integer(c$singleton_clusters),
  adjudicated_cases=as.integer(s$adjudicated_cases %||% NA),
  llm_final_decisions=as.integer(s$llm_final_decisions %||% NA),
  human_final_decisions=as.integer(s$human_final_decisions %||% NA),
  abstract_strip_actions=as.integer(c$abstract_strip_actions),
  manifestations_missing_abstract=as.integer(c$manifestations_missing_abstract),
  canonical_jsonl_sha256=as.character(c$canonical_jsonl_sha256),
  canonical_jsonl_bytes=as.numeric(c$canonical_jsonl_bytes),
  source_counts=c$source_counts,
  validation=v,
  zenodo=if(is.null(z)) NULL else list(
    record_id=z$zenodo_record_id,
    doi=z$doi,
    record_url=z$record_url,
    manifest_sha256=z$manifest_sha256,
    visibility=z$visibility
  )
)
dir.create(dirname(out_json),recursive=TRUE,showWarnings=FALSE)
dir.create(dirname(out_md),recursive=TRUE,showWarnings=FALSE)
writeLines(toJSON(report,auto_unbox=TRUE,pretty=TRUE,null="null",na="null"),out_json,useBytes=TRUE)

source_lines <- paste(vapply(names(c$source_counts),function(nm)
  sprintf("| %s | %s |",nm,as.integer(c$source_counts[[nm]])),character(1)),collapse="\n")
zenodo_text <- if(is.null(z)) "Not yet archived." else sprintf("[%s](%s), DOI %s",z$zenodo_record_id,z$record_url,z$doi)
validation_text <- if(is.null(v)) "No validation summary supplied to this report run." else {
  if(!is.null(v$wrong_automatic_promotions)) {
    sprintf("%s cases; %s wrong automatic decisions; %s routed to human review.",
            v$cases,v$wrong_automatic_promotions,v$routed_to_human_review)
  } else {
    sprintf("%s",toJSON(v,auto_unbox=TRUE))
  }
}
md <- c(
  sprintf("# Workflow 01 run %s",run_id),
  "",
  "## Status",
  "",
  sprintf("- GitHub Actions: [%s](%s)",run_id,run_url),
  sprintf("- Finalisation status: **%s**",s$status %||% "unknown"),
  sprintf("- Canonical records: **%s**",c$records),
  sprintf("- Source manifestations: **%s**",c$source_manifestations),
  sprintf("- Duplicate clusters: **%s**",c$duplicate_clusters),
  sprintf("- Singleton clusters: **%s**",c$singleton_clusters),
  sprintf("- Abstracts stripped for metadata mismatch: **%s**",c$abstract_strip_actions),
  "",
  "## Source manifestations",
  "",
  "| Source | Manifestations |",
  "|---|---:|",
  source_lines,
  "",
  "## Adjudication",
  "",
  sprintf("- Adjudicated cases: %s",s$adjudicated_cases %||% "NA"),
  sprintf("- Final LLM decisions: %s",s$llm_final_decisions %||% "NA"),
  sprintf("- Final human decisions: %s",s$human_final_decisions %||% "NA"),
  sprintf("- Unresolved pair decisions: %s",s$unresolved_pair_decisions %||% "NA"),
  "",
  "## Validation",
  "",
  validation_text,
  "",
  "## Canonical JSONL",
  "",
  sprintf("- SHA-256: `%s`",c$canonical_jsonl_sha256),
  sprintf("- Bytes: %s",format(as.numeric(c$canonical_jsonl_bytes),scientific=FALSE)),
  sprintf("- Manifestations missing abstracts after Workflow 01: %s",c$manifestations_missing_abstract),
  "",
  "## Durable archive",
  "",
  zenodo_text,
  "",
  "## Downstream handoff",
  "",
  "The authoritative handoff is the canonical JSONL stored in the restricted Workflow 01 Zenodo record. Workflow 02 restores this file, verifies its checksum, and scans manifestations for missing titles and abstracts.",
  ""
)
writeLines(md,out_md,useBytes=TRUE)
cat(sprintf("PASS: wrote Workflow 01 run report for %s\n",run_id))

`%||%` <- function(x,y) if(is.null(x)) y else x
