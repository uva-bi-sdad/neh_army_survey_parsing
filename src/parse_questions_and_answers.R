library(magrittr)
library(data.table)

parse_questions_and_answers <- function(study_folder_path) {
  print(study_folder_path)
  # LOAD FUNCTIONS
  functions <- list.files("src/functions/", full.names = TRUE)
  for (f in functions) source(f)
  # SET STUDY FOLDER
  study_folder <- study_folder_path
  # CREATE SESSION VARIABLES
  set_session_variables(study_folder)
  # IMPORT CODEBOOK
  print("import codebook")
  codebook_lines <- import_codebook(codebook_folder)
  # IMPORT ANSWER FILE
  print("import answers")
  answerlines <- import_answer_file(answer_folder)
  # CHECK INSTRUCTIONS
  instructions <- check_instructions(codebook_lines)
  print(instructions)
  # FIX NOT USED COLUMNS
  codebook_lines <- fix_col_not_used(instructions, codebook_lines)
  # EXTRACT QUESTIONS
  print("extract questions")
  questions <- extract_questions(codebook_lines)
  # CHECK FOR JOINED QUESTIONS
  print("fix combined questions")
  questions <- fix_coded_together(instructions, questions)
  # PARSE TO QUESTIONS AND OPTIONS
  print("parse questions")
  questions <- parse_questions(questions)
  # EXTRACT QUESTION COLUMN WIDTHS
  print("extract column widths")
  colwidths <- extract_answer_column_widths(codebook_lines)
  # TEST QUESTION EXTRACTION OUTPUT
  if (nrow(questions) != length(colwidths)) stop("NUMBER OF QUESTIONS DOES NOT MATCH NUMBER OF COLUMNS!")
  # CREATE ONE ROW PER RESPONDENT
  singlerows <- create_one_row_respondent(answerlines)
  write.table(singlerows, file = "data/working/temp.fwf", row.names = F, col.names = F, quote = F)
  # PARSE EACH ANSWER ROW WITH COLWIDTHS
  print("parse answers with col widths")
  prsd <- parse_answer_rows(colwidths, answer_rows_file = "data/working/temp.fwf")
  print(prsd[1,])
  #browser()
  # WRITE OUTPUT FILES
  write_output_files(dir = "data/working/", questions, prsd)
  # CLEANUP
  unlink("data/working/temp.fwf")
  #browser()
}


dirs <- list.dirs("data/original", recursive = F)
for (d in dirs[57:83]) parse_questions_and_answers(d)

parse_questions_and_answers("data/original/Attitudes of (or towards) Negroes (PS-32), March 1943")
parse_questions_and_answers("data/original/Attitudes in Air Transport Command (AMS-141), July 1944")
parse_questions_and_answers("data/original/Attitudes in the Caribbean (AMS-115), January-February 1944")
parse_questions_and_answers("data/original/Attitudes of Negro Quartermaster Troops (AMS-174), October 1944")
parse_questions_and_answers("data/original/Attitudes of Army Nurses (AMS-192), January 1945")
parse_questions_and_answers("data/original/Attitudes toward Branch of Service (AMS-44), April 1943")
parse_questions_and_answers("data/original/Attitudes toward the War and Further Duty (AMS-212), May 1945")
parse_questions_and_answers("data/original/Returnees_ Reactions to Enemy and Further Duty (AMS-211), June 1945")
parse_questions_and_answers("data/original/Methodological Study of the Measurement of Intensity (AMS-215), June 1945")
