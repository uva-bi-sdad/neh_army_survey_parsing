library(data.table)
library(stringr)
library(readr)

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

# SET STUDY FOLDER
study_folder <- "data/original/Survey of Hospital Patients (AMS-193), July 1945"

# CREATE SESSION VARIABLES
set_session_variables(study_folder)

# IMPORT CODEBOOK
codebook_lines <- import_codebook(codebook_folder)

# EXTRACT QUESTIONS
questions <- extract_questions(codebook_lines)

# SPECIAL RULES FOR JUST THIS SURVEY

# REMOVE QUESTIONS THAT HAVE NO ASSOCIATED ANSWER COLUMN(S)
questions <- remove_questions_without_columns(questions)

# TRIM TO JUST QUESTION TEXT
questions_text <- trim_to_question(questions)

# EXTRACT QUESTION COLUMN WIDTHS
colwidths <- extract_answer_column_widths(questions)

# REPEAT QUESTIONS
l <- lapply(stringr::str_match_all(questions, "[RACJUY]\\.\\sCOLS?\\."), length)
dub_rows <- data.table::data.table(w = as.data.table(t(as.data.table(l)))[V1 > 1, which = TRUE],
                                   d = as.data.table(t(as.data.table(l)))[V1 > 1][[1]] - 1)
dub_rows$q <- questions_text[dub_rows$w]

qdt <- as.data.table(questions_text)
if (nrow(dub_rows) > 0) {
  for (i in 1:nrow(dub_rows)) {
    rn <- qdt[questions_text == dub_rows[i]$q, which = TRUE]
    qdt <- add_duplicate_rows(qdt, rn, dub_rows[i]$d)
  }
}
questions_text <- qdt

fwrite(questions_text, file = paste0("data/working/", question_file), col.names = FALSE)

# IMPORT ANSWER FILE
answer_file_path <- list.files(answer_folder, pattern = "*.CLEAN", full.names = TRUE)[1]
answerlines <- readr::read_lines(answer_file_path)

# CREATE ONE ROW PER RESPONDENT
singlerows <- create_one_row_respondent(answerlines)
write.table(singlerows, file = "data/working/temp.fwf", row.names = F, col.names = F, quote = F)

# PARSE EACH ANSWER ROW WITH COLWIDTHS
prsd <- read.fwf("data/working/temp.fwf", colwidths)
colnames(prsd) <- gsub("V", "COL", colnames(prsd))
fwrite(prsd, paste0("data/working/", answer_file))

# CLEANUP
unlink("data/working/temp.fwf")
