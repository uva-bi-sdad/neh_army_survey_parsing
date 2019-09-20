parse_question <- function(question_str) {
  #browser()
  splt <- strsplit(question_str, "[RACJUY]\\.\\sCOLS?\\.\\s[0-9]+-?[0-9]?[0-9]?", type = "before")
  if (!is.na(splt[[1]][2])) {
    q <- splt[[1]][1]
    c <- splt[[1]][2:length(splt[[1]])]
    first <- stringr::str_replace(c, "[RACJUY]\\.\\sCOLS?\\.\\s[0-9]+-?[0-9]?[0-9]?\\.\\s", "")
    second <- stringr::str_replace_all(first, "[1-9]?[0-9]*?\\s", " ")
    paste(q, second) 
  }
}

parse_questions <- function(questions_ls) {
  out_ls <- list()
  for (t in unlist(questions_ls)) out_ls <- c(out_ls, parse_question(t))
  return(data.table::as.data.table(unlist(out_ls)))
}