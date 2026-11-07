#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(jsonlite)
  library(digest)
})

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag, default=NULL) {
  i <- match(flag, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop(sprintf("Missing value after %s", flag), call.=FALSE)
  args[[i + 1L]]
}
`%||%` <- function(x, y) if (is.null(x)) y else x

mode <- arg("--mode")
if (is.null(mode) || !mode %in% c("stamp", "select")) {
  stop("--mode must be stamp or select", call.=FALSE)
}

fingerprint_files <- c(
  ".github/workflows/workflow_02_batched.yml",
  "scripts/updater/workflow_02_plan_batches.R",
  "scripts/updater/workflow_02_metadata_enrichment.R",
  "scripts/updater/workflow_02_build_patch.R",
  "scripts/updater/workflow_02_apply_patch.R",
  "scripts/updater/workflow_02_resume_checkpoints.R"
)
missing_fp <- fingerprint_files[!file.exists(fingerprint_files)]
if (length(missing_fp)) {
  stop(sprintf("Missing fingerprint files: %s", paste(missing_fp, collapse=", ")), call.=FALSE)
}
file_hashes <- vapply(
  fingerprint_files,
  function(p) digest(file=p, algo="sha256", serialize=FALSE),
  character(1)
)
code_fingerprint <- digest(
  paste(paste(names(file_hashes), file_hashes, sep="="), collapse="\n"),
  algo="sha256",
  serialize=FALSE
)

count_jsonl <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  x <- readLines(path, warn=FALSE, encoding="UTF-8")
  as.integer(sum(nzchar(trimws(x))))
}

if (mode == "stamp") {
  batch <- arg("--batch")
  input_path <- arg("--input")
  checkpoint_dir <- arg("--checkpoint-dir")
  run_id <- arg("--run-id", Sys.getenv("GITHUB_RUN_ID"))
  if (any(vapply(list(batch, input_path, checkpoint_dir), is.null, logical(1)))) {
    stop("stamp requires --batch --input --checkpoint-dir", call.=FALSE)
  }
  required <- c(
    "current_patch.jsonl", "current_patch_report.json",
    "enrichment_audit.jsonl", "enrichment_report.json",
    "replay_report.json", "retry_queue.jsonl"
  )
  missing <- required[!file.exists(file.path(checkpoint_dir, required))]
  if (length(missing)) {
    stop(sprintf("Checkpoint incomplete: %s", paste(missing, collapse=", ")), call.=FALSE)
  }
  er <- fromJSON(file.path(checkpoint_dir, "enrichment_report.json"), simplifyVector=FALSE)
  pr <- fromJSON(file.path(checkpoint_dir, "current_patch_report.json"), simplifyVector=FALSE)
  rr <- fromJSON(file.path(checkpoint_dir, "replay_report.json"), simplifyVector=FALSE)
  n <- count_jsonl(input_path)
  if (!identical(as.integer(er$processed_eligible_records %||% -1L), n)) {
    stop("Checkpoint processed_eligible_records does not match batch input", call.=FALSE)
  }
  if (!identical(pr$status %||% "", "PASS") || !identical(rr$status %||% "", "PASS")) {
    stop("Checkpoint reports are not PASS", call.=FALSE)
  }
  manifest <- list(
    schema="living-evidence-map-workflow02-batch-checkpoint-v1",
    batch=batch,
    records=n,
    batch_input_sha256=digest(file=input_path, algo="sha256", serialize=FALSE),
    code_fingerprint_sha256=code_fingerprint,
    source_run_id=as.character(run_id),
    created_at_utc=format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ")
  )
  writeLines(
    toJSON(manifest, auto_unbox=TRUE, pretty=TRUE, null="null"),
    file.path(checkpoint_dir, "checkpoint_manifest.json"),
    useBytes=TRUE
  )
  cat(sprintf("PASS: stamped Workflow 02 checkpoint batch %s (%d records)\n", batch, n))
  quit(save="no", status=0)
}

plan_dir <- arg("--plan-dir")
resume_root <- arg("--resume-root")
output_matrix <- arg("--output-matrix")
output_resume_dir <- arg("--output-resume-dir")
output_report <- arg("--output-report")
if (any(vapply(list(plan_dir, output_matrix, output_resume_dir, output_report), is.null, logical(1)))) {
  stop("select requires --plan-dir --output-matrix --output-resume-dir --output-report", call.=FALSE)
}

matrix_path <- file.path(plan_dir, "matrix.json")
plan_path <- file.path(plan_dir, "plan.json")
if (!file.exists(matrix_path) || !file.exists(plan_path)) stop("Batch plan is incomplete", call.=FALSE)
matrix <- fromJSON(matrix_path, simplifyVector=FALSE)
planned <- matrix$include %||% list()
dir.create(output_resume_dir, recursive=TRUE, showWarnings=FALSE)

valid <- character()
rejected <- list()
if (!is.null(resume_root) && nzchar(resume_root) && dir.exists(resume_root)) {
  manifests <- list.files(
    resume_root,
    pattern="checkpoint_manifest\\.json$",
    recursive=TRUE,
    full.names=TRUE
  )
  for (m_path in manifests) {
    m <- tryCatch(fromJSON(m_path, simplifyVector=FALSE), error=function(e) NULL)
    reason <- NULL
    if (is.null(m)) {
      reason <- "unreadable_manifest"
    } else if (!identical(m$schema %||% "", "living-evidence-map-workflow02-batch-checkpoint-v1")) {
      reason <- "unsupported_manifest_schema"
    } else if (!identical(m$code_fingerprint_sha256 %||% "", code_fingerprint)) {
      reason <- "code_fingerprint_mismatch"
    } else {
      batch <- as.character(m$batch %||% "")
      expected <- Filter(function(x) identical(as.character(x$batch), batch), planned)
      if (length(expected) != 1L) {
        reason <- "batch_not_in_current_plan"
      } else {
        input_path <- file.path(plan_dir, paste0("batch-", batch, ".jsonl"))
        expected_sha <- digest(file=input_path, algo="sha256", serialize=FALSE)
        expected_n <- count_jsonl(input_path)
        if (!identical(m$batch_input_sha256 %||% "", expected_sha)) {
          reason <- "batch_input_sha256_mismatch"
        } else if (!identical(as.integer(m$records %||% -1L), expected_n)) {
          reason <- "batch_record_count_mismatch"
        } else {
          checkpoint_dir <- dirname(m_path)
          required <- c(
            "current_patch.jsonl", "current_patch_report.json",
            "enrichment_audit.jsonl", "enrichment_report.json",
            "replay_report.json", "retry_queue.jsonl",
            "checkpoint_manifest.json"
          )
          missing <- required[!file.exists(file.path(checkpoint_dir, required))]
          if (length(missing)) {
            reason <- paste0("missing_files:", paste(missing, collapse=","))
          } else {
            er <- fromJSON(file.path(checkpoint_dir, "enrichment_report.json"), simplifyVector=FALSE)
            pr <- fromJSON(file.path(checkpoint_dir, "current_patch_report.json"), simplifyVector=FALSE)
            rr <- fromJSON(file.path(checkpoint_dir, "replay_report.json"), simplifyVector=FALSE)
            if (!identical(as.integer(er$processed_eligible_records %||% -1L), expected_n)) {
              reason <- "processed_record_count_mismatch"
            } else if (!identical(pr$status %||% "", "PASS") || !identical(rr$status %||% "", "PASS")) {
              reason <- "checkpoint_report_not_pass"
            } else {
              target <- file.path(output_resume_dir, paste0("workflow02-batch-", batch, "-resumed"))
              if (dir.exists(target)) unlink(target, recursive=TRUE, force=TRUE)
              dir.create(target, recursive=TRUE, showWarnings=FALSE)
              ok <- file.copy(
                file.path(checkpoint_dir, required),
                file.path(target, required),
                overwrite=TRUE
              )
              if (!all(ok)) stop(sprintf("Failed to copy resumed checkpoint %s", batch), call.=FALSE)
              valid <- c(valid, batch)
            }
          }
        }
      }
    }
    if (!is.null(reason)) {
      key <- if (!is.null(m) && nzchar(as.character(m$batch %||% ""))) as.character(m$batch) else basename(dirname(m_path))
      rejected[[length(rejected)+1L]] <- list(batch=key, reason=reason)
    }
  }
}

valid <- sort(unique(valid))
remaining <- Filter(function(x) !as.character(x$batch) %in% valid, planned)
matrix_out <- if (length(remaining)) {
  list(include=remaining)
} else {
  list(include=list(list(batch="none", records=0L)))
}
writeLines(
  toJSON(matrix_out, auto_unbox=TRUE, null="null"),
  output_matrix,
  useBytes=TRUE
)
report <- list(
  schema="living-evidence-map-workflow02-resume-selection-v1",
  code_fingerprint_sha256=code_fingerprint,
  planned_batches=length(planned),
  resumed_batches=valid,
  resumed_batch_count=length(valid),
  remaining_batch_count=length(remaining),
  rejected=rejected,
  created_at_utc=format(Sys.time(), tz="UTC", format="%Y-%m-%dT%H:%M:%SZ")
)
writeLines(
  toJSON(report, auto_unbox=TRUE, pretty=TRUE, null="null"),
  output_report,
  useBytes=TRUE
)
cat(sprintf(
  "PASS: Workflow 02 resume selection reused %d of %d planned batches; %d remain\n",
  length(valid), length(planned), length(remaining)
))
