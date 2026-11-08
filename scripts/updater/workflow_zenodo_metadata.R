# Shared, topic-independent restricted Zenodo metadata. Caller must supply
# EVIDENCE_ZENODO_CONFIG, never inherit an unrelated project's authorship.
workflow_zenodo_metadata <- function(){
  path <- trimws(Sys.getenv("EVIDENCE_ZENODO_CONFIG",unset=""))
  if(!nzchar(path)||!file.exists(path))
    stop("Set EVIDENCE_ZENODO_CONFIG to an existing project-specific Zenodo metadata file",call.=FALSE)
  cfg <- jsonlite::fromJSON(path,simplifyVector=FALSE)
  title <- cfg$title
  if(is.null(title)||length(title)!=1L||!nzchar(trimws(title))||
     grepl("REPLACE_WITH_",title,fixed=TRUE))
    stop("Zenodo project title must be supplied",call.=FALSE)
  creators <- cfg$creators
  if(is.null(creators)||!length(creators)||
     any(!vapply(creators,function(z){
       n<-z$name
       !is.null(n)&&length(n)==1L&&nzchar(trimws(n))&&
         !grepl("REPLACE_WITH_",n,fixed=TRUE)
     },logical(1))))
    stop("Zenodo creators must identify the actual project authors/depositors",call.=FALSE)
  access <- if(is.null(cfg$access_right))"restricted" else as.character(cfg$access_right)
  if(length(access)!=1L||!identical(access,"restricted"))
    stop("This workflow requires restricted Zenodo publication",call.=FALSE)
  list(title=as.character(title),creators=creators,access_right=access)
}
