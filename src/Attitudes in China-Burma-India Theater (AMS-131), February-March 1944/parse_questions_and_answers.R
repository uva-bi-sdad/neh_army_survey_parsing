# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)
# SET STUDY FOLDER
study_folder <- "data/original/Attitudes in China-Burma-India Theater (AMS-131), February-March 1944"
# CREATE SESSION VARIABLES
set_session_variables(study_folder)
# IMPORT CODEBOOK
codebook_lines <- import_codebook(codebook_folder)
# IMPORT ANSWER FILE
answerlines <- import_answer_file(answer_folder)
# EXTRACT QUESTIONS
questions <- extract_questions(codebook_lines)
# CHECK FOR SPECIAL INSTRUCTIONS
check_instructions()
# SPECIAL RULES FOR JUST THIS SURVEY
## questions 3 and 4 are coded together
combine_indicies <- which(str_detect(questions, "Q.[67]\\."))
combined_questions <- paste(questions[combine_indicies], collapse = "")
questions[combine_indicies[1]] <- combined_questions
questions[[combine_indicies[2]]] <- NULL
# PARSE TO QUESTIONS AND OPTIONS
questions <- parse_questions(questions)
# EXTRACT QUESTION COLUMN WIDTHS
colwidths <- extract_answer_column_widths(codebook_lines)
# TEST QUESTION EXTRACTION OUTPUT
if (nrow(questions) != length(colwidths)) stop("NUMBER OF QUESTIONS DOES NOT MATCH NUMBER OF COLUMNS!")
# CREATE ONE ROW PER RESPONDENT
singlerows <- create_one_row_respondent(answerlines)
write.table(singlerows, file = "data/working/temp.fwf", row.names = F, col.names = F, quote = F)
# PARSE EACH ANSWER ROW WITH COLWIDTHS
prsd <- parse_answer_rows(colwidths, answer_rows_file = "data/working/temp.fwf")
# WRITE OUTPUT FILES
write_output_files(dir = "data/working/")
# CLEANUP
unlink("data/working/temp.fwf")