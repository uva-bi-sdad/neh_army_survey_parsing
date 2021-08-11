replace_continued <- function(questions_dt) {
  # find questions start with X
  rownums <- which(grepl("^X", questions_dt[, get(names(questions_dt)[1])]))
  for (r in rownums) {
    # get full X question and question number
    qx <- str_match(questions_dt[r], "X\\. QUESTION ([0-9][0-9]?[0-9A-Z]?[0-9A-Z])? CONTINUED\\.?")
    # X question full text
    qxt <- qx[1]
    # X question number
    qxn <- qx[2]
    # get original question
    qo <- questions_dt[get(names(questions_dt)[1]) %like% paste0("[Q|V]", qxn, ".")][1]
    qo <- str_match(qo, "(^.*)0?1\\.")[2]
    # create new question text
    qn <- str_replace(questions_dt[r], qxt, qo)
    questions_dt[r] <- qn
  }
  questions_dt
}

