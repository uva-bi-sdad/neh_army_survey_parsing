# LOAD LIBRARIES
source("src/libraries.R")

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

# GET FILE PATHS TO ALL ANSWER AND CODEBOOK FILES
qs_n_as <- find_all_answer_and_codebook_files()

# COUNT PARTICIPANTS PER SURVEY PRE AND POST PROCESSING
num_resp_dt <- data.table(survey = character(), resp_pre = numeric(), resp_post = numeric(), resp_post0 = numeric())

for (s in qs_n_as$answer_file) {
  # pre-processing
  sur_num <- stringr::str_extract(s, "AMS\\.?P?S?[0-9][0-9A-Z]?[0-9A-Z]?[0-9A-Z]?[A-Z]?")
  file <- readLines(s)
  num_lines <- length(file)
  div <- 1
  for (i in 2:10) {
    if (str_sub(file[i], 1, 1) == 1) {
      div <- i - 1
      break()
    }
  }
  
  # post-processing
  strt <- str_extract(sur_num, "AMS")
  end <- str_extract(sur_num, "[0-9][0-9A-Z]?[0-9A-Z]?[0-9A-Z]?[A-Z]?")
  ans_file_name <- list.files("data/working/", paste0(strt, "P?S?0?0?", end, "_answers.csv"))[1]
  ans_file_path <- paste0("data/working/", ans_file_name)
  
  ans_file_lines <- readLines(ans_file_path)
  ans_resp_lines <- length(ans_file_lines) - 1
  
  # previous post-processing
  # post-processing
  strt0 <- str_extract(sur_num, "AMS")
  end0 <- str_extract(sur_num, "[0-9][0-9A-Z]?[0-9A-Z]?[0-9A-Z]?[A-Z]?")
  ans_file_name0 <- list.files("data/temp/3rd Extraction/", paste0(strt0, "P?S?0?0?", end0, "_answers.csv"))[1]
  ans_file_path0 <- paste0("data/temp/3rd Extraction/", ans_file_name0)
  
  ans_file_lines0 <- readLines(ans_file_path0)
  ans_resp_lines0 <- length(ans_file_lines0) - 1
  
  # outfile
  num_resp_dt <- rbindlist(list(num_resp_dt, data.table(sur_num, num_lines/div, ans_resp_lines, ans_resp_lines0)), use.names = F)
}

num_resp_dt[resp_pre != resp_post0, survey]



# CHECK QUESTIONS STARTING WITH X
csvs <- list.files("data/working/", pattern = "*questions.csv", full.names = T)
for (c in csvs) {
  questions_dt <- data.table::as.data.table(readr::read_lines(c))
  o <- which(grepl("^\"?X", questions_dt[, get(names(questions_dt)[1])]))
  if (length(o) != 0) {
    print(c)
    print(o)
  } 
}