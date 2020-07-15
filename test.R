
library(tm)
tst <- function() {
  browser()
  txt <-
    tolower(
      "SEPARATE OUTFITS BECAUSE NEGROES THEMSELVES \\(OR BOTH NEGROES AND WHITES\\) LIKE IT BETTER THAT WAY \\(NEGROES FEEL UNCOMFORTABLE WITH WHITES, NEGROES FEEL MORE AT HOME WITH OWN RACE\\)"
    )
  
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  res <- stringr::str_replace_all(txt, stopwords_regex, '')
  res <- stringr::str_replace_all(res, "\\s+", " ")
  nchar(res)
  res
}

tst()

opts <- ""
for (i in names(dt_out)) {
  opt <- names(attributes(dt_out[, .(get(i))][[1]])$labels)
  opts <- c(opts, opt)
}
