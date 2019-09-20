create_one_row_respondent <- function(answerlines_ls) {
  if (substr(answerlines_ls[1], 1, 1) != substr(answerlines_ls[2], 1, 1)) {
    ansdt <- as.data.table(t(as.data.table(strsplit(answerlines_ls, "\n"))))
    ansdt.firsts = ansdt[substr(V1, 1, 1) == 1]
    ansdt.seconds = ansdt[substr(V1, 1, 1) == 2]
    fnl <- cbind(ansdt.firsts, ansdt.seconds)   
    colnames(fnl) <- c("C1", "C2")
    fnl[, rec := paste0(C1, C2)]
    singlerecs <- fnl[, .(rec)]
    return(singlerecs)
  } else {
    return(answerlines_ls)
  }
}
