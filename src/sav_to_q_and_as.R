

file_paths <- list.files("data/working/update_2021_11_22/", pattern = "*nocounts_lbl_txt_fix.sav", full.names = TRUE)

if (exists("qs_and_as")) rm(qs_and_as)

for (f in 1:length(file_paths)) {
  print(basename(file_paths[f]))
  tbl_haven_labelled <- haven::read_sav(file_paths[f])
  for (i in 1:length(tbl_haven_labelled)) {
    survey <- basename(file_paths[f])
    q_name <- names(tbl_haven_labelled[i])
    questions <- attr(tbl_haven_labelled[[i]], "label")
    answers_text <- names(attr(tbl_haven_labelled[[i]], "labels"))
    answers_nums <- attr(tbl_haven_labelled[[i]], "labels")
    dt <-
      data.table::data.table(survey, q_name, questions, answers_text, answers_nums)
    if (!exists("qs_and_as"))
      qs_and_as <- dt
    else
      qs_and_as <-
      data.table::rbindlist(list(qs_and_as, dt), fill = TRUE)
  }
}
  
rio::export(qs_and_as, "data/working/update_2021_11_22/all_qs_and_as.xlsx", overwrite=T)
