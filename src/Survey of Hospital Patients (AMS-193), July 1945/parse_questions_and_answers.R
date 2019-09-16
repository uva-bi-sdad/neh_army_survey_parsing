library(data.table)
library(stringr)
library(readr)

# read data files
# data files
codebook_file <- "data/original/Survey of Hospital Patients (AMS-193), July 1945/Technical Documentation/AMS-193_ Survey of Hospital Patients, codebook.AMS0193.CDBK"
answer_file <- "data/original/Survey of Hospital Patients (AMS-193), July 1945/Electronic Records/AMS-193_ Survey of Hospital Patients, data.AMS193.CLEAN"

codebook_raw <- paste(readr::read_lines(codebook_file), collapse = "\r\n")
answerlines <- readr::read_lines(answer_file)

# set output filenames
study_code <- str_match(codebook_file, "\\.(.+)\\.CDBK")[, 2]
question_file <- paste0(study_code, "_questions.csv")
answer_file <- paste0(study_code, "_answers.csv")

# extract the questions
qs <- stringr::str_match_all(codebook_raw, "\\s\\s\\s([QV]\\.[0-9]+(.|\\r|\\n)*?)(\\*|:|R\\.|[A-Z]\\.\\s\\s\\s)")[[1]][,2]
qs_cln <- stringr::str_replace_all(qs, "\\s*(\\r|\\n)\\s*", " ")
qs_cln_dt <- as.data.table(qs_cln)
colnames(qs_cln_dt) <- c("V1")

# remove questions that have subquestions (becasue the main question has no answers)
m <- stringr::str_match(qs_cln, "([0-9][0-9]?)[A-Z]")[,2]
m <- unique(m[!is.na(m)])
for (i in m) {
  val <- paste0("*", i, "\\.")
  qs_cln_dt <- qs_cln_dt[!V1 %like% val]
}

# special rules just for this questionnaire

## questions not coded
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.33B\\."]
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.42B\\."]
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.43B\\."]
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.44B\\."]
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.56\\."]


# add duplicate rows
## get row number
dup_row_num <- qs_cln_dt[V1 %like% "Q\\.28B", which = TRUE]
qs_cln_dt_dups <- add_duplicate_rows(qs_cln_dt, dup_row_num, 10)

dup_row_num <- qs_cln_dt_dups[V1 %like% "Q\\.32\\.", which = TRUE]
qs_cln_dt_dups_2 <- add_duplicate_rows(qs_cln_dt_dups, dup_row_num, 11)

dup_row_num <- qs_cln_dt_dups_2[V1 %like% "Q\\.48\\.", which = TRUE]
qs_cln_dt_dups_3 <- add_duplicate_rows(qs_cln_dt_dups_2, dup_row_num, 11)


# add administrative questions to complete the set of questions
last_card1_q <- qs_cln_dt_dups_3[V1 %like% "Q.45\\.", which = TRUE]
first_card_qs <- qs_cln_dt_dups_3[1: last_card1_q]
second_card_qs <- qs_cln_dt_dups_3[(last_card1_q + 1):nrow(qs_cln_dt_dups_3)]
admin_entries <- data.table(V1 = c("CARD", "DECK", "BALLOT"))
all_questions <- rbindlist(list(admin_entries, first_card_qs, admin_entries, second_card_qs))
colnames(all_questions) <- c("question")
all_questions[, colnum := paste0("V", .I)]
fwrite(all_questions, file = paste0("data/working/", question_file))


# extract question columns and calculate question column widths
allcols <- stringr::str_extract_all(codebook_raw, "COLS?\\.\\s[0-9]+-?[0-9]?[0-9]?")
allcolsdt <- data.table::as.data.table(allcols)
allcolsdt[, V2 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][1]), V1]
allcolsdt[, V3 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][2]), V1]
allcolsdt[is.na(V3), V4 := 1]
allcolsdt[!is.na(V3), V4 := V3 - V2 + 1]
colwidths <- allcolsdt[, V4]

# create single answer record for each respondent
#strsplit(answerlines, "\n")
ansdt <- as.data.table(t(as.data.table(strsplit(answerlines, "\n"))))
ansdt.firsts = ansdt[substr(V1, 1, 1) == 1]
ansdt.seconds = ansdt[substr(V1, 1, 1) == 2]
fnl <- cbind(ansdt.firsts, ansdt.seconds)   
colnames(fnl) <- c("C1", "C2")
fnl[, rec := paste0(C1, C2)]
singlerecs <- fnl[, .(rec)]
write.table(singlerecs, file = "data/working/temp.fwf", row.names = F, col.names = F, quote = F)

# parse the single record answers with the column widths
prsd <- read.fwf("data/working/temp.fwf", colwidths)
fwrite(prsd, paste0("data/working/", answer_file))

# cleanup
unlink("data/working/temp.fwf")
