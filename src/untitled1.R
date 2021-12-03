AMS227_aaron <- haven::read_spss("data/working/update_2021_11_19/AMS227_CLEAN_NOM_nocounts_lbl_txt_fix.sav")
AMS227_mike <- haven::read_spss("../../Downloads/AMS227_CLEAN_NOM_nocounts.sav")

tbl_haven_labelled <- AMS227_aaron
for (i in 1:length(tbl_haven_labelled)) {
  survey <- basename(file_paths[f])
  q_name <- names(tbl_haven_labelled[i])
  questions <- attr(tbl_haven_labelled[[i]], "label")
  answers_text <- names(attr(tbl_haven_labelled[[i]], "labels"))
  answers_nums <- attr(tbl_haven_labelled[[i]], "labels")
  dt <-
    data.table::data.table(survey, q_name, questions, answers_text, answers_nums)
  if (!exists("AMS227_aaron_dt"))
    AMS227_aaron_dt <- dt
  else
    AMS227_aaron_dt <-
    data.table::rbindlist(list(AMS227_aaron_dt, dt), fill = TRUE)
}

tbl_haven_labelled <- AMS227_mike
for (i in 1:length(tbl_haven_labelled)) {
  survey <- basename(file_paths[f])
  q_name <- names(tbl_haven_labelled[i])
  questions <- attr(tbl_haven_labelled[[i]], "label")
  answers_text <- names(attr(tbl_haven_labelled[[i]], "labels"))
  answers_nums <- attr(tbl_haven_labelled[[i]], "labels")
  dt <-
    data.table::data.table(survey, q_name, questions, answers_text, answers_nums)
  if (!exists("AMS227_mike_dt"))
    AMS227_mike_dt <- dt
  else
    AMS227_mike_dt <-
    data.table::rbindlist(list(AMS227_mike_dt, dt), fill = TRUE)
}

for (i in 1:640) {
  if(identical(AMS227_aaron_dt[i],AMS227_mike_dt[i]) == FALSE) {
    print(i)
    print(AMS227_aaron_dt[i])
    print(AMS227_mike_dt[i])
  }
}

AMS227_aaron_dt[1312]
AMS

names(AMS227_aaron) <- names(AMS227_mike)

names(AMS227_aaron)[150:156]
names(AMS227_mike)[150:156]

AMS227_aaron[150:156][[1]]
AMS227_mike[150:156][[1]]

identical(AMS227_aaron_dt, AMS227_mike_dt)

AMS227_aaron_dt[620:640,]



