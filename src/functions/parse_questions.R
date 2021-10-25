parse_question <- function(question_str) {
#browser()
  splt <- strsplit(question_str, "[RACJUY]\\.\\s\\s?[Cc][Oo][Ll][Ss]?\\.?\\s[0-9]+-?[0-9]?[0-9]?", type = "before")
  if (!is.na(splt[[1]][2])) {
    question <- splt[[1]][1]
    columns <- splt[[1]][2:length(splt[[1]])]
    first <- stringr::str_replace(columns, "[RACJUY]\\.\\s\\s?[Cc][Oo][Ll][Ss]?\\.?\\s[0-9]+-?[0-9]?[0-9]?\\.\\s", "")
    
    #browser()
    
    #second <- stringr::str_replace_all(first, "[1-9]?[0-9]*?\\s", " ")
    second <- stringr::str_match_all(first,"(?<!Q\\.)([0-9]+)\\s([0-9]{1,2}\\.)")
    third <- first
    if (!dim(second[[1]])[1] == 0) {
      for (i in 1:dim(second[[1]])[1]) {
        x <- second[[1]][i, 1]
        y <- second[[1]][i, 3]
        third <- stringr::str_replace_all(third, x, y)
      }
    }
    #browser()
    #second <- stringr::str_replace_all(first, "(?<!Q\\.)\\w[0-9][0-9]?[0-9]?", " ")
    
    paste(question, third) 
  }
}

parse_questions <- function(questions_ls) {
  out_ls <- list()
  for (t in unlist(questions_ls)) out_ls <- c(out_ls, parse_question(t))
  return(data.table::as.data.table(unlist(out_ls)))
}

# t1 <- "S035.Q68A. WHAT CLOTHES IN THE FOLLOWING LIST HAVE YOU BOUGHT IN THE LAST TWO MONTHS? "
# t2 <- "S035.Q68B. NOW, WHAT DID YOU BUY IN THE LAST TWO MONTHS IN THIS LIST? "
# parse_question(t1)
# # 
# # 
# txt3 <- "Q.48L7. YESTERDAY WAS THURSDAY, FIRST CHOICE: C. COLS. 61-62. 15 01. YES, BING CROSBY (KRAFT MUSIC HALL) 4 02. YES, HENRY ALDRICH 6 03. YES, BOB BURNS 28 04. YES, NEWS AND NEWS COMMENTATORS 6 05. YES, ABBOTT AND COSTELLO 9 06. YES, DANCE MUSIC 4 07. YES, MUSIC NOT SPECIFIED 8 08. YES, MISCELLANEOUS 44 09. YES, NO PROGRAM NAMED 11 10. YES, NO RADIO PROGRAM CODED HERE BUT ONE CODED ELSEWHERE 136 11. NO 15 00. NO ANSWER 4010 99. INAPPLICABLE (YESTERDAY WAS NOT THURSDAY), NO CODE OR NO DATA"
# # 
# parse_question(txt3)
