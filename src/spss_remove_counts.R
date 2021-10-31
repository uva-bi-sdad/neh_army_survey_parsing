file_paths <- list.files("data/working/update_2021_10_26/", pattern = "*CLEAN_NOM_nocounts.sav", full.names = T)

srch_str <- "(?<!(AND|OF|OR|THRU|OVER|CODE|THAN|TO|IN|CARD|JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER|,))\\s[0-9]{1,5}$"

for (file_path in file_paths) {
  tbl_haven_labelled <- haven::read_spss(file_path)
  for (i in 1:ncol(tbl_haven_labelled)) {
    haven_labelled <- tbl_haven_labelled[, i][[1]]
    if (class(haven_labelled) == "haven_labelled") {
      attr(haven_labelled, "label") <- stringr::str_remove(attr(haven_labelled, "label"), srch_str)
      names(attr(haven_labelled, "labels")) <- stringr::str_remove(names(attr(haven_labelled, "labels")), srch_str)
      tbl_haven_labelled[, i][[1]] <- haven_labelled  
    }
  }
  haven::write_sav(tbl_haven_labelled, file_path)
  # haven::write_sav(tbl_haven_labelled, paste0(tools::file_path_sans_ext(file_path), "_nocounts.sav"))  
}






# file_path <- "data/working/update_2021_10_26/AMS212_CLEAN_NOM_nocounts.sav"
# haven::write_sav(tbl_haven_labelled, file_path)