import_answer_file <- function(answer_folder) {
  answer_file_path <- list.files(answer_folder, pattern = "*.CLEAN", full.names = TRUE)[1]
  answerlines <- readr::read_lines(answer_file_path)
  return(answerlines)
}