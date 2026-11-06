#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call. = FALSE)
  args[[i + 1L]]
}

workflow <- arg("--workflow")
manifest_path <- arg("--manifest")
artifact_url <- arg("--artifact-url")
workflow_url <- arg("--workflow-url")
output_path <- arg("--output")
app_url <- arg("--app-url", Sys.getenv(
  "ADJUDICATION_APP_URL",
  "https://01a0f774-a1a3-dab9-3d3f-f86402fc6d81.share.connect.posit.cloud"
))
if (any(vapply(list(workflow, manifest_path, artifact_url, workflow_url, output_path), is.null, logical(1)))) {
  stop("Required: --workflow --manifest --artifact-url --workflow-url --output", call. = FALSE)
}
if (!workflow %in% c("01", "02")) stop("--workflow must be 01 or 02", call. = FALSE)
if (!file.exists(manifest_path)) stop("Manifest/status file not found", call. = FALSE)

m <- fromJSON(manifest_path, simplifyVector = FALSE)
recipient <- Sys.getenv("REVIEW_EMAIL", "")
if (!nzchar(recipient)) recipient <- Sys.getenv("SMTP_USERNAME", "")
if (!nzchar(recipient)) stop("REVIEW_EMAIL or SMTP_USERNAME must be set", call. = FALSE)

if (workflow == "01") {
  pending <- as.integer(m$pending_count)
  if (is.na(pending)) stop("Workflow 01 manifest missing pending_count", call. = FALSE)
  subject <- sprintf("LivingEvidenceMap: Workflow 01 review ready — %d duplicate case(s)", pending)
  body <- paste(
    "LivingEvidenceMap Workflow 01 requires human duplicate adjudication.",
    "",
    sprintf("Pending duplicate cases: %d", pending),
    sprintf("Locked queue SHA-256: %s", as.character(m$queue_sha256)),
    "",
    "Open the Shiny adjudication app:",
    app_url,
    "",
    "Audit/recovery package:",
    artifact_url,
    "",
    "Workflow run:",
    workflow_url,
    "",
    "Workflow 01 final publication is paused until all required human decisions are completed and the locked review state passes the integrity gate.",
    sep = "\n"
  )
} else {
  pending <- as.integer(m$blocking_cases)
  conflicts <- as.integer(m$blocking_conflicts)
  if (is.na(pending)) stop("Workflow 02 status missing blocking_cases", call. = FALSE)
  subject <- sprintf("LivingEvidenceMap: Workflow 02 review ready — %d record(s)", pending)
  body <- paste(
    "LivingEvidenceMap Workflow 02 requires human adjudication of quarantined enrichment conflicts.",
    "",
    sprintf("Pending records: %d", pending),
    sprintf("Quarantined conflicts: %d", conflicts),
    if (!is.null(m$manifest_sha256)) sprintf("Review manifest SHA-256: %s", as.character(m$manifest_sha256)) else NULL,
    "",
    "Open the Shiny adjudication app:",
    app_url,
    "",
    "Audit/recovery package:",
    artifact_url,
    "",
    "Workflow run:",
    workflow_url,
    "",
    "Workflow 02 publication and downstream handoff are blocked until these conflicts are adjudicated.",
    sep = "\n"
  )
}

payload <- list(
  recipient = recipient,
  subject = subject,
  pending_count = pending,
  body_text = body
)
write_json(payload, output_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
cat(sprintf("PASS: Workflow %s review email payload built for %d pending record(s)\n", workflow, pending))
