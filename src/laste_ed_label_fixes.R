tbl_haven_labelled <- haven::read_sav("data/working/update_2021_11_19/AMS32N_CLEAN_NOM_nocounts_lbl_txt_fix.sav")

which(names(tbl_haven_labelled) == "S32N.V2")

attr(tbl_haven_labelled[["S32N.V2"]], "label")


ed_edits_xl <- data.table::setDT(rio::import("data/working/update_2021_11_22/questions_no_counts.xlsx", sheet = 1))

library(data.table)
survey_question_label <- ed_edits_xl[, "a", c("survey", "q_name", "questions")][, V1 := NULL][q_name %like% "^S"]

surveys_unq <- unique(survey_question_label$survey)
surveys_unq <- paste0("data/working/update_2021_11_19/", surveys_unq)

for (s in surveys_unq) {
  tbl_haven_labelled <- haven::read_sav(s)
  survey_qs <- survey_question_label[survey == basename(s), .(survey, q_name, questions)]
  for (i in 1:nrow(survey_qs)) {
    q <- survey_qs[i]$q_name
    attr(tbl_haven_labelled[[q]], "label") <- survey_qs[i]$questions
  }
  haven::write_sav(tbl_haven_labelled, paste0("data/working/update_2021_11_22/", basename(s)))
}




ed_edits_xl <- data.table::setDT(rio::import("data/working/update_2021_11_22/questions_no_counts.xlsx", sheet = 2))
survey_question_label <- ed_edits_xl[, "a", c("survey", "q_name", "questions")][, V1 := NULL][q_name %like% "^S"]
surveys_unq <- unique(survey_question_label$survey)
surveys_unq <- paste0("data/working/update_2021_11_22/", surveys_unq)
for (s in surveys_unq) {
  tbl_haven_labelled <- haven::read_sav(s)
  survey_qs <- survey_question_label[survey == basename(s), .(survey, q_name, questions)]
  for (i in 1:nrow(survey_qs)) {
    q <- survey_qs[i]$q_name
    attr(tbl_haven_labelled[[q]], "label") <- survey_qs[i]$questions
  }
  haven::write_sav(tbl_haven_labelled, paste0("data/working/update_2021_11_22/", basename(s)))
}



tbl_haven_labelled <- haven::read_sav("data/working/update_2021_11_22/AMS235_CLEAN_NOM_nocounts_lbl_txt_fix.sav")
attr(tbl_haven_labelled[["S235.Q47B.5"]], "label") <- "IF YOU THINK THE U.S. WILL BE IN ANOTHER BIG WAR, WHO DO YOU THINK THE U.S. WILL BE FIGHTING AGAINST?"
haven::write_sav(tbl_haven_labelled, "data/working/update_2021_11_22/AMS235_CLEAN_NOM_nocounts_lbl_txt_fix.sav")

tbl_haven_labelled <- haven::read_sav("data/working/update_2021_11_22/AMS44I_CLEAN_NOM_nocounts_lbl_txt_fix.sav")
attr(tbl_haven_labelled[["S44I.Q49A.4"]], "label") <- "WHY DO YOU THINK MOST HIGHLY OF THIS BRANCH? E. ALTRUISTIC MOTIVATION:"
haven::write_sav(tbl_haven_labelled, "data/working/update_2021_11_22/AMS44I_CLEAN_NOM_nocounts_lbl_txt_fix.sav")



upd_11_19 <- list.files("data/working/update_2021_11_19/", pattern = "*fix.sav")
upd_11_22 <- list.files("data/working/update_2021_11_22/", pattern = "*fix.sav")

missing <- paste0("data/working/update_2021_11_19/", upd_11_19[!upd_11_19 %in% upd_11_22])

file.copy(missing, "data/working/update_2021_11_22/")



