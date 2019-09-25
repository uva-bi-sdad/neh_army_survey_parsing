parse_question <- function(question_str) {
  #browser()
  splt <- strsplit(question_str, "[RACJUY]\\.\\s\\s?[Cc][Oo][Ll][Ss]?\\.\\s[0-9]+-?[0-9]?[0-9]?", type = "before")
  if (!is.na(splt[[1]][2])) {
    question <- splt[[1]][1]
    columns <- splt[[1]][2:length(splt[[1]])]
    first <- stringr::str_replace(columns, "[RACJUY]\\.\\s\\s?[Cc][Oo][Ll][Ss]?\\.\\s[0-9]+-?[0-9]?[0-9]?\\.\\s", "")
    #second <- stringr::str_replace_all(first, "[1-9]?[0-9]*?\\s", " ")
    second <- stringr::str_replace_all(first, "(?<!Q\\.)\\w[0-9][0-9]?[0-9]?", " ")
    paste(question, second) 
  }
}

parse_questions <- function(questions_ls) {
  out_ls <- list()
  for (t in unlist(questions_ls)) out_ls <- c(out_ls, parse_question(t))
  return(data.table::as.data.table(unlist(out_ls)))
}
