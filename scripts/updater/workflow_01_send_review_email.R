#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

args <- commandArgs(trailingOnly=TRUE)
arg <- function(flag,default=NULL) {
  i <- match(flag,args)
  if (is.na(i)) return(default)
  if (i==length(args)) stop(sprintf("Missing value after %s",flag),call.=FALSE)
  args[[i+1L]]
}
payload_path <- arg("--payload")
if (is.null(payload_path) || !file.exists(payload_path)) stop("Required: --payload",call.=FALSE)
x <- fromJSON(payload_path,simplifyVector=FALSE)
if (as.integer(x$pending_count) <= 0L) {
  cat("PASS: no pending review cases; email not sent\n")
  quit(status=0)
}

required_env <- c("SMTP_HOST","SMTP_PORT","SMTP_USERNAME","SMTP_PASSWORD")
missing <- required_env[!nzchar(Sys.getenv(required_env))]
if (length(missing)) stop(sprintf("Missing SMTP environment variables: %s",paste(missing,collapse=", ")),call.=FALSE)

tmp <- tempfile(fileext=".txt")
msg <- c(
  sprintf("From: %s",Sys.getenv("SMTP_USERNAME")),
  sprintf("To: %s",x$recipient),
  sprintf("Subject: %s",x$subject),
  "MIME-Version: 1.0",
  "Content-Type: text/plain; charset=UTF-8",
  "",
  x$body_text
)
writeLines(msg,tmp,useBytes=TRUE)

port <- as.integer(Sys.getenv("SMTP_PORT"))
scheme <- if (port==465L) "smtps" else "smtp"
url <- sprintf("%s://%s:%d",scheme,Sys.getenv("SMTP_HOST"),port)
cmd <- c(
  "--silent","--show-error","--fail",
  "--url",url,
  "--user",paste0(Sys.getenv("SMTP_USERNAME"),":",Sys.getenv("SMTP_PASSWORD")),
  "--mail-from",Sys.getenv("SMTP_USERNAME"),
  "--mail-rcpt",as.character(x$recipient),
  "--upload-file",tmp
)
if (port != 465L) cmd <- c(cmd,"--ssl-reqd")
status <- system2("curl",cmd)
if (status != 0L) stop(sprintf("SMTP notification failed with curl status %d",status),call.=FALSE)
cat(sprintf("PASS: review notification sent to %s for %d pending case(s)\n",
            x$recipient,as.integer(x$pending_count)))
