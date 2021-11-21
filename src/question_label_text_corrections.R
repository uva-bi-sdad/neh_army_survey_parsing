fixes <- data.table::fread("data/working/update_2021_10_31/text_corrections.csv")

file_paths <- list.files("data/working/update_2021_10_31", pattern = "*.nocounts.sav", full.names = TRUE)



for (i in 1:length(file_paths)) {
  tbl_haven_labelled <- haven::read_spss(file_paths[i])
  
  for (f in 1:nrow(fixes)) {
    for (j in 1:length(tbl_haven_labelled)) {
      bool <-
        stringr::str_detect(attr(tbl_haven_labelled[[j]], "label"), fixes[f]$original)
      if (bool == TRUE) {
        print(basename(file_paths[i]))
        print(fixes[f]$original)
        print(names(tbl_haven_labelled[j]))
        print(attr(tbl_haven_labelled[[j]], "label"))
        attr(tbl_haven_labelled[[j]], "label") <-
          stringr::str_replace(attr(tbl_haven_labelled[[j]], "label"),
                               fixes[f]$original,
                               fixes[f]$correction)
        print(attr(tbl_haven_labelled[[j]], "label"))
      }
    }
    
    for (j in 1:length(tbl_haven_labelled)) {
      bool <-
        any(stringr::str_detect(names(attr(tbl_haven_labelled[[j]], "labels")), fixes[f]$original))
      if (bool == TRUE) {
        print(basename(file_paths[i]))
        print(fixes[f]$original)
        print(names(tbl_haven_labelled[j]))
        print(names(attr(tbl_haven_labelled[[j]], "labels")))
        names(attr(tbl_haven_labelled[[j]], "labels")) <- stringr::str_replace(names(attr(tbl_haven_labelled[[j]], "labels")), fixes[f]$original, fixes[f]$correction)
        print(names(attr(tbl_haven_labelled[[j]], "labels")))
      }
    }
  }
  
  haven::write_sav(tbl_haven_labelled, paste0("data/working/update_2021_11_19/", tools::file_path_sans_ext(basename(file_paths[i])), "_lbl_txt_fix.sav"))
}


