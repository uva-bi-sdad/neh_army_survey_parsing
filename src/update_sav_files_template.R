library(data.table)
library(haven)

# load new questions and answers
answers <- fread("data/working/update_2022_06_29/qa_2022_06-09/answers.csv", header = T)
questions <- fread("data/working/update_2022_06_29/qa_2022_06-09/questions.csv", header = T)

# set directories for input and output files
input_dir <- "data/working/update_2021_11_22/"
output_dir <- "data/working/update_2022_06_29/"

# get list of the original .sav files for which you will be creating updated versions
sav_files <- list.files(input_dir, pattern = "*.sav", full.names = T)

# get unique survey identifiers
surveys_unq <- unique(questions[parent_id!= "" & parent_id!= "X"]$parent_id)

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
    if (q_and_a == TRUE) {
      # assign question text ('label' in SPSS)
      attr(tbl_haven_labelled[[q]], "label") <- survey_qs[i]$question
      # assign potential answers texts (labels in SPSS)
      names(attr(tbl_haven_labelled[[q]], "labels")) <- answers[question_id==q][order(position)]$label
    }
  }
  
  # write new updated .sav file
  haven::write_sav(tbl_haven_labelled, paste0(output_dir, s, ".sav"))
}

