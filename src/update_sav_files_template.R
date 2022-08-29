library(data.table)
library(haven)

# load new questions and answers
answers <- fread("data/working/update_2022_08_29/answers.csv", header = T)
questions <- fread("data/working/update_2022_08_29/questions.csv", header = T)

# set directories for input and output files
base_dir <- "data/working/update_2022_08_29"
input_dir <- file.path(base_dir, "input") 
output_dir <- file.path(base_dir, "output")

# get list of the original .sav files for which you will be creating updated versions
sav_files <- list.files(input_dir, pattern = "*.sav", full.names = T)

# get unique survey identifiers
surveys_unq <- unique(questions[parent_id!= "" & parent_id!= "X"]$parent_id)

# start log
readr::write_lines(paste0(date()),
                   file = file.path(base_dir, "log.txt"),
                   append = TRUE)

# for each unique survey, update the questions and potential answers
for (s in surveys_unq) {
  # get original .sav file path
  sav_file_path <- sav_files[sav_files %like% s][1]
  # read the original .sav file
  tbl_haven_labelled <- haven::read_sav(sav_file_path)
  # get the new survey questions
  survey_qs <- questions[parent_id == s, .(parent_id, identifier, question)]
  
  # for each question, update the question text ('label' in SPSS) and the potential answers texts (labels in SPSS)
  for (i in 1:nrow(survey_qs)) {
    # get question id
    q <- survey_qs[i]$identifier
    
    # check if question is in both question and answer files
    q_and_a <- nrow(answers[question_id == q]) > 0
    # check if question is in SPSS file
    q_in_spss <- !is.null(tbl_haven_labelled[[q]])
    
    # log if question not in original spss file
    if (q_in_spss == FALSE) {
      readr::write_lines(paste0(q, " in new questions but missing from input SPSS file"),
                         file = file.path(base_dir, "log.txt"),
                         append = TRUE)
    }
    
    if (q_and_a == TRUE & q_in_spss == TRUE) {
      # assign question text ('label' in SPSS)
      attr(tbl_haven_labelled[[q]], "label") <- survey_qs[i]$question
      
      # assign potential answers texts (labels in SPSS)
      #- build replacement named vector of labels
      labels_nmd_vec <- as.double(answers[question_id==q]$position)
      names(labels_nmd_vec) <- answers[question_id==q]$label
      #- replace named vectors of labels
      attr(tbl_haven_labelled[[q]], "labels") <- labels_nmd_vec
    }
  }
  
  # write new updated .sav file
  haven::write_sav(tbl_haven_labelled, file.path(output_dir, paste0(s, ".sav")))
}


