write_output_files <- function(dir = "data/working/") {
  data.table::fwrite(questions, file = paste0("data/working/", question_file), col.names = FALSE)
  data.table::fwrite(prsd, file = paste0("data/working/", answer_file))
}