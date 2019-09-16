library(data.table)
library(stringr)
library(readr)

# read data files
# data files
codebook_file <- "data/original/Psychoneurotic Study (AMS-126), April-May 1944/Technical Documentation/AMS-126_ Psychoneurotic Study 04-05_1944, codebook.AMS0126.CDBK"
answer_file <- "data/original/Psychoneurotic Study (AMS-126), April-May 1944/Electronic Records/AMS-126_ Psychoneurotic Study 04-05_1944, data.AMS126.CLEAN"
codebook_raw <- readr::read_file(codebook_file)
answerlines <- readr::read_lines(answer_file)

# set output filenames
study_code <- str_match(codebook_file, "\\.(.+)\\.CDBK")[, 2]
question_file <- paste0(study_code, "_questions.csv")
answer_file <- paste0(study_code, "_answers.csv")

# extract the questions
qs <- stringr::str_match_all(codebook_raw, "\\s\\s\\s([QV]\\.[0-9]+(.|\\r|\\n)*?)(\\*|:|R\\.)")[[1]][,2]
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
## questions a and 4 coded together
combined_question <- qs_cln_dt[V1 %like% "Q.3\\." | V1 %like% "Q.4\\.", paste(V1, collapse = ": ")]
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.4\\."]
qs_cln_dt[V1 %like% "Q.3\\.", V1 := combined_question]

## question 93 not coded
qs_cln_dt <- qs_cln_dt[!V1 %like% "Q.93\\."]


# add administrative questions to complete the set of questions
last_card1_q <- qs_cln_dt[V1 %like% "Q.67\\.", which = TRUE]
first_card_qs <- qs_cln_dt[1: last_card1_q]
second_card_qs <- qs_cln_dt[(last_card1_q + 1):nrow(qs_cln_dt)]
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
