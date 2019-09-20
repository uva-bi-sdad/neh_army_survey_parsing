check_instructions <- function(code = "X") {
  unlist(stringr::str_match_all(codebook_lines, paste0("\\s", code, "\\.\\s.*")))
}