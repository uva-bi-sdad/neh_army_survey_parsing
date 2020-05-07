# LOAD LIBRARIES
source("src/libraries.R")

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)


answerscsv <- read.csv("data/working/AMS0035_answers.csv", stringsAsFactors = F)
questionsscsv <- read.csv("data/working/AMS0035_questions.csv", stringsAsFactors = F, header = F)

# create data.table of raw questions
codebook_raw <- 
  data.table(
    id = 1:nrow(questionsscsv),
    raw = questionsscsv[,1]
    ) %>% 
# get question nickname
.[, q_nickname := str_replace(names(answerscsv), "\\.$", "")] %>% 
# parse question
.[, question := str_trim(str_match(raw, "^.+?\\s(.+?)\\s?[0-9X]{1,3}\\.")[,2])] %>% 
# fill empty questions with q_nickname
.[question == "", question := q_nickname] %>% 
# fix parentheses for future regex
.[, question := str_replace_all(question, "\\(", "\\\\\\(")] %>%
.[, question := str_replace_all(question, "\\)", "\\\\\\)")] %>% 
# fix asterics for future regex
.[, question := str_replace_all(question, "\\*", "\\\\\\*")] %>% 
# get answer options
.[, codes_raw := str_trim(str_remove(str_remove(raw, "^.+?\\s"), question))] %>% 
# parse codes
.[, codes := str_match_all(codes_raw, "[0-9X]{1,3}\\.")]

# parse answer options
answer_options <- parse_answer_options()



# STRUCTURE FOR .SAV
for (i in 1:ncol(example_data)) {
  # example_response_text %>% filter(q_nickname == colnames(example_data[i])) %>% .$labels
  response_text <- factor(example_response_text %>% filter(q_nickname == colnames(example_data[i])) %>% .$labels )
  # REATTACH OPTION NUMBER AND OPTION TEXT
  levels(example_data[,i]) <- response_text
  # BODY OF THE TEXT
  attr(example_data[,i], "labels") <- response_text
  names(attr(example_data[,i], "labels")) <- response_text
  # COLUMN NAME - SHOULD BE QEUSTION TEXT
  attr(example_data[,i], "label") <- examplecodebook$question[i]
  names(attr(example_data[,i], "label")) <- examplecodebook$question[i]
}

# REMOVE QUESTIONS THAT ARE TOO LONG
example_data_min2cols <- example_data[,colnames(example_data) %in% c("Q.20C.", "Q.27.") == FALSE]

str(example_data_min2cols)

# WRITE SPSS FILE
write_sav(example_data_min2cols, "test8.sav")
