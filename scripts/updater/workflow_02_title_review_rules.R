# W02 strict title equivalence for metadata enrichment.
# Only promotes complete title identity or an intact complete title followed by
# a substantial appended title (commonly a translation). DOI and record identity
# checks in the calling enrichment pipeline remain mandatory.
w02_normalize_title <- function(x) {
  if(is.null(x) || !length(x)) return("")
  s <- as.character(x[[1L]])
  if(is.na(s) || !nzchar(trimws(s))) return("")
  for(i in seq_len(3L)) {
    s <- gsub("&amp;","&",s,fixed=TRUE)
    s <- gsub("&lt;","<",s,fixed=TRUE)
    s <- gsub("&gt;",">",s,fixed=TRUE)
    s <- gsub("&quot;","\"",s,fixed=TRUE)
    s <- gsub("&apos;","'",s,fixed=TRUE)
    s <- gsub("&#39;","'",s,fixed=TRUE)
    s <- gsub("&#x([0-9a-fA-F]+);", " ",s,perl=TRUE)
  }
  s <- gsub("<[^>]*>","",s,perl=TRUE)
  s <- iconv(s,from="",to="ASCII//TRANSLIT",sub="")
  if(is.na(s)) return("")
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+"," ",s,perl=TRUE)
  trimws(gsub("[[:space:]]+"," ",s))
}
w02_title_equivalence <- function(canonical,provider) {
  a <- w02_normalize_title(canonical)
  b <- w02_normalize_title(provider)
  if(!nzchar(a) || !nzchar(b)) return("manual_review")
  if(identical(a,b) && nchar(a)>=20L) return("exact_after_markup_normalisation")
  if(nchar(a)>=40L && startsWith(b,paste0(a," "))) {
    suffix <- trimws(substring(b,nchar(a)+1L))
    if(grepl("^(correction|erratum|corrigendum|retraction|retracted|author s reply)\\b",suffix,perl=TRUE))
      return("correction_notice_needs_review")
    if(nchar(suffix)>=35L) return("complete_title_with_appended_translation")
  }
  "no_strict_equivalence"
}
