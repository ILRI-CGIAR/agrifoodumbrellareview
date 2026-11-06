#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
`%||%` <- function(x,y) {
  if (is.null(x)||!length(x)||is.na(x)||!nzchar(as.character(x))) y else as.character(x)
}

input_path <- arg("--input")
output_dir <- arg("--output-dir")
source_run_id <- arg("--source-run-id")
recipient <- arg("--recipient","nealhaddaway@gmail.com")
repository <- arg("--repository",Sys.getenv("GITHUB_REPOSITORY","thesalmonandthetomato/LivingEvidenceMap"))
workflow_run_id <- arg("--workflow-run-id",Sys.getenv("GITHUB_RUN_ID",""))
if (any(vapply(list(input_path,output_dir,source_run_id),is.null,logical(1)))) {
  stop("Required: --input --output-dir --source-run-id",call.=FALSE)
}
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)

lines <- readLines(input_path,warn=FALSE,encoding="UTF-8")
lines <- lines[nzchar(trimws(lines))]
cases <- lapply(lines,fromJSON,simplifyVector=FALSE)
ids <- vapply(cases,function(x)as.character(x$review_case_id),character(1))
if (anyDuplicated(ids)) stop("Human-review queue contains duplicate review_case_id",call.=FALSE)

queue_path <- file.path(output_dir,"human_review_cases.jsonl")
if (length(lines)) writeLines(lines,queue_path,useBytes=TRUE) else file.create(queue_path)

case_summary <- function(r) {
  sprintf(
    "%s | %s (%s) <> %s (%s)",
    r$review_case_id,
    r$record_i$title %||% "<missing title>",r$record_i$year %||% "?",
    r$record_j$title %||% "<missing title>",r$record_j$year %||% "?"
  )
}

manifest <- list(
  schema="living-evidence-map-workflow01-human-review-manifest-v1",
  source_workflow01_run_id=source_run_id,
  adjudication_workflow_run_id=workflow_run_id,
  repository=repository,
  status=if(length(cases)) "awaiting_human_review" else "no_human_review_required",
  pending_count=length(cases),
  review_case_ids=ids,
  queue_sha256=digest(file=queue_path,algo="sha256",serialize=FALSE)
)
writeLines(toJSON(manifest,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"review_manifest.json"))

decision_path <- sprintf("data/adjudication/workflow01/run-%s/human_decisions.jsonl",source_run_id)
complete_path <- sprintf("data/adjudication/workflow01/run-%s/COMPLETE.json",source_run_id)

handoff <- c(
  "# LivingEvidenceMap Workflow 01 human duplicate review",
  "",
  "This package contains the residual duplicate cases that were not resolved automatically by deterministic rules and LLM adjudication.",
  "",
  "## Instructions for ChatGPT",
  "",
  sprintf("Repository: %s",repository),
  sprintf("Adjudication workflow run: %s",workflow_run_id),
  sprintf("Source Workflow 01 baseline run: %s",source_run_id),
  sprintf("Pending cases: %d",length(cases)),
  "",
  "Use the GitHub connector to retrieve the Actions artefact from the adjudication workflow run containing this file and human_review_cases.jsonl if it is not already attached.",
  "",
  "Review cases one at a time in stable review_case_id order. For each case, show both records with:",
  "",
  "- title",
  "- abstract",
  "- keywords",
  "- journal/source",
  "- year",
  "- authors",
  "- volume",
  "- issue",
  "- pages",
  "- DOI and source record identifier",
  "- deterministic duplicate evidence",
  "- LLM decision, confidence and rationale",
  "",
  "The human decision must be exactly duplicate, not_duplicate, or uncertain. DOI is supporting evidence only and must never be decisive by itself. Topical similarity is not evidence that two records are the same publication. Account for legitimate manifestation changes such as preprint to journal publication, early-online to final version, database duplicates, and conference abstract to full publication where the evidence supports identity.",
  "",
  "Do not silently decide on behalf of the reviewer. Explain the bibliographic evidence briefly, then obtain or confirm the human decision.",
  "",
  "For every resolved case maintain an immutable decision record with:",
  "",
  "review_case_id | decision | rationale | reviewer | resolved_at_utc",
  "",
  sprintf("Write human decisions back to GitHub under: %s",decision_path),
  "",
  "Do not trigger finalisation until every case has a decision other than uncertain.",
  sprintf("When all cases are resolved, also write: %s",complete_path),
  "",
  "The COMPLETE marker must state the number of resolved cases and SHA-256 of the decision file. This marker is the resume signal for Workflow 01 finalisation."
)
writeLines(handoff,file.path(output_dir,"CHATGPT_HANDOFF.md"))

if (length(cases)) {
  summaries <- vapply(cases,case_summary,character(1))
  writeLines(c("# Pending case index","",sprintf("- %s",summaries)),
             file.path(output_dir,"case_index.md"))
} else {
  writeLines(c("# Pending case index","","No human review is required."),
             file.path(output_dir,"case_index.md"))
}

run_url <- if(nzchar(workflow_run_id)) sprintf("https://github.com/%s/actions/runs/%s",repository,workflow_run_id) else ""
body <- paste(
  "LivingEvidenceMap Workflow 01 duplicate review requires human adjudication.",
  "",
  sprintf("Pending cases after LLM adjudication: %d",length(cases)),
  sprintf("Source Workflow 01 baseline run: %s",source_run_id),
  sprintf("Adjudication workflow result: %s",run_url),
  "",
  "Open the workflow result and download the human-review artefact. The artefact contains CHATGPT_HANDOFF.md with the complete prompt and human_review_cases.jsonl with the full bibliographic evidence.",
  "",
  "Paste the workflow link and the contents of CHATGPT_HANDOFF.md into ChatGPT. Review cases one by one. Final decisions are written back to GitHub and Workflow 01 resumes only when the COMPLETE marker has been created.",
  sep="\n"
)
payload <- list(
  recipient=recipient,
  subject=sprintf("LivingEvidenceMap: %d duplicate case(s) require review",length(cases)),
  pending_count=length(cases),
  workflow_run_url=run_url,
  source_workflow01_run_id=source_run_id,
  body_text=body
)
writeLines(toJSON(payload,auto_unbox=TRUE,pretty=TRUE,null="null"),
           file.path(output_dir,"email_payload.json"))

cat(sprintf("PASS: built human-review package with %d pending cases\n",length(cases)))
