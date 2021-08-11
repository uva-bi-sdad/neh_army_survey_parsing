library(readr)
library(stringr)





remove_end_counts <- function(txt_file_path = "data/working/AMS74rawData/AMS74metaAnswerLabels.txt") {
  # srch_str = string ending with a space followed by 1 to 5 numbers followed by a double quote and comma, 
  # but not following the words "AND", "OF", "THAN" or "CARD"
  srch_str <- "(?<!(AND|OF|OR|THRU|OVER|CODE|THAN|TO|IN|CARD|JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER|,))\\s[0-9]{1,5}\",$"
  srch_lines <- read_lines(txt_file_path)
  tofix_lines <- str_which(srch_lines, srch_str)
  print(srch_lines[tofix_lines])
  fixd_lines <- str_remove(srch_lines, srch_str)
  print("----------------------------------")
  print(fixd_lines[tofix_lines])
  fixd_lines
}

file_paths <- list.files("data/working", pattern = "*raw*", full.names = T)
for (f in file_paths) {
  file_path <- list.files(f, "*AnswerLabels*", full.names = T)[1]
  print(file_path)
  fxd <- remove_end_counts(file_path)
  write_lines(fxd, paste0(file_path, ".fxd.txt"))
}

fixed_lines <- remove_end_counts("data/working/AMS234BErawData/AMS234BEmetaAnswerLabels.txt")
