extract_questions <- function(lines) {
  
  question_indicies <- which(stringr::str_detect(lines, "^\\s*[QVN]\\.\\.?[0-9][0-9]?[0-9]?[A-Z]?\\."))
  admin_qs_indicies <- which(stringr::str_detect(lines, "(^\\s*CARD\\.\\s+$|^\\s*DECK\\.\\s+$|^\\s*BALLOT\\.\\s+|^\\s*FORM\\.\\s+$)"))
  combined_indicies <- sort(c(question_indicies, admin_qs_indicies))
  combined_less_one <- combined_indicies - 1
  combined_less_one <- combined_less_one[2:length(combined_less_one)]
  end <- which(stringr::str_detect(lines, "[Cc][Oo][Ll].*[Nn][Oo][Tt]\\s[Uu][Ss][Ee][Dd]")) - 1
  if (length(end) == 0) end <- length(lines)
  combined_less_one[length(combined_less_one) + 1] <- max(end)
  index_ranges <- data.table::data.table(start = as.integer(combined_indicies), end = as.integer(combined_less_one))
  
  ret_ls <- list()
  for (i in 1:nrow(index_ranges)) {
    start <- index_ranges[i, 1][[1]]
    end <- index_ranges[i, 2][[1]]
    q_str <- paste(lines[start:end], collapse = "") %>% 
      stringr::str_replace_all("\\s+", " ") %>% 
      stringr::str_replace("^\\s+", "")
    ret_ls[i] <- list(q_str)
  }
  
  return(ret_ls)
}
