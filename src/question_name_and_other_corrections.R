tbl_haven_labelled <- haven::read_spss("data/working/update_2021_11_19/AMS40O_CLEAN_NOM_nocounts_lbl_txt_fix.sav")

tbl_q_lbl_fixes <- data.table::fread("data/working/update_2021_11_19/AMS40O_question_labels.csv")

lbls <- data.table::data.table(row = integer(), question = character(), lbl_orig = character(), lbl_fix = character())
for (i in 1:length(tbl_haven_labelled)) {
  lbl <- attr(tbl_haven_labelled[[i]], "label")
  lbls <- data.table::rbindlist(list(lbls, data.table::data.table(row = i, question = names(tbl_haven_labelled[i]), lbl_orig = lbl, lbl_fix = lbl)))
}

write.csv(lbls, "data/working/update_2021_11_19/AMS40O_question_labels.csv", row.names = FALSE)


names(tbl_haven_labelled) <- tbl_q_lbl_fixes$question_fix
for (i in 1:length(tbl_haven_labelled)) {
  attr(tbl_haven_labelled[[i]], "label") <- tbl_q_lbl_fixes[i, lbl_fix]
}
haven::write_sav(tbl_haven_labelled, "data/working/update_2021_11_19/AMS40O_CLEAN_NOM_nocounts_lbl_txt_fix_2.sav")

unlink("data/working/update_2021_11_19/AMS40O_CLEAN_NOM_nocounts_lbl_txt_fix.sav")
file.rename("data/working/update_2021_11_19/AMS40O_CLEAN_NOM_nocounts_lbl_txt_fix_2.sav", "data/working/update_2021_11_19/AMS40O_CLEAN_NOM_nocounts_lbl_txt_fix.sav")
