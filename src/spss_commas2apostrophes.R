library(haven)
library(stringr)
file_paths <- list.files("data/working/update_2021_10_26/", pattern = "*CLEAN_NOM_nocounts.sav", full.names = T)

string_fixes <- list()
string_fixes$problem <- c("B-29,S", "NEGRO,S", "IT,S", "C.O.,S ORDERS", "M.P.,S", "SOLDIER,S JOB", "4F,S")
string_fixes$fix <- c("B-29'S", "NEGROS'", "IT'S", "C.O.'S ORDERS", "M.P.'S", "SOLDIER'S JOB", "4F'S")

for (file_path in file_paths) {
  tbl_haven_labelled <- haven::read_spss(file_path)
  for (i in 1:ncol(tbl_haven_labelled)) {
    haven_labelled <- tbl_haven_labelled[, i][[1]]
    #print(haven_labelled)
    if (class(haven_labelled) == "haven_labelled") {
      names(attr(haven_labelled, "labels")) <- 
        str_replace_all(names(attr(haven_labelled, "labels")), 
                        string_fixes$problem[j], 
                        string_fixes$fix[j])
      tbl_haven_labelled[, i][[1]] <- haven_labelled
    }
  }
  haven::write_sav(tbl_haven_labelled, file_path)
}


