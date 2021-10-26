library("data.table")
library(tools)
library(stringr)

apos_files <- list.files("data/working/update_2021_10_26/", pattern = "*nocounts.sav", full.names = TRUE)
rm(suspect_files)
for (f in apos_files) {
  tbl_haven_labelled <- haven::read_spss(f)
  for (i in 1:length(tbl_haven_labelled)) {
    if (str_detect(attr(tbl_haven_labelled[i][[1]], "label"), ".* E\\. .*") == TRUE) {
      if(!exists("suspect_files")) {
        suspect_files <- f
      } else {
        suspect_files <- c(suspect_files, f)
      }
    }
  }
}

new_old_sav_files <- data.table(suspect_file = unique(suspect_files))
new_old_sav_files[, new_basename := file_path_sans_ext(basename(suspect_file))]
new_old_sav_files[, study := str_sub(new_basename, 1, str_locate(new_basename, "_CLEAN")[,1] -1)]

nb <- new_old_sav_files$new_basename
str_locate(nb, "_CLEAN")[,1]

old_sav_files <- grep(list.files(path="data/working", full.names = TRUE), pattern='.*CLEAN.*', invert=TRUE, value=TRUE)
old_sav_files <- grep(f1, pattern = ".sav$", value = TRUE)

old_sav_files_paths <- data.table(old_sav_file = old_sav_files)
old_sav_files_paths$study00s <- tools::file_path_sans_ext(basename(old_sav_files))
old_sav_files_paths$study <- str_replace(old_sav_files_paths$study00s, str_match(old_sav_files_paths$study00s, "AMS0+"), "AMS")
old_sav_files_paths[is.na(study), study := study00s]
old_sav_files_paths[study00s %like% "PS5", study := str_replace(study00s, "AMS", "")]

new_old <- merge(new_old_sav_files, old_sav_files_paths, by = "study", all.x = TRUE)
new_old <- new_old[, .(new = suspect_file, old = old_sav_file)]

#sav_file_match <- fread("data/working/update_2021_10_26/SAV_file_matching.csv")


new_section_labels <- function(sav_file_path) {
  tbl_haven_labelled <- haven::read_spss(sav_file_path)
  
  new_file_path <- new_old$new[i]
  
  if(exists("section_changes")) rm(section_changes)
  
  for (i in 1:length(tbl_haven_labelled)) {
    #print(i)
    #browser()
    if (str_detect(attr(tbl_haven_labelled[i][[1]], "label"), ".* E\\. .*") == TRUE &
        any(str_detect(names(attr(tbl_haven_labelled[i][[1]], "labels")), ".* E\\. .*")) == TRUE) {
      
      q_label_old <- attr(tbl_haven_labelled[i+1][[1]], "label")
      q_label_subsec_old <- str_match(q_label_old, ".* (E. .*)")[2]
      str_ls <- str_match(names(attr(tbl_haven_labelled[i][[1]], "labels")), ".* (E. .*)")[,2]
      q_label_subsec_new <- str_ls[!is.na(str_ls)]
      q_label_new <- str_replace(q_label_old, q_label_subsec_old, q_label_subsec_new)
#browser()
      q_id_old <- attr(tbl_haven_labelled[i+1][1], "names")
      q_id_old_base <- str_replace_all(str_replace(q_id_old, "\\.\\.[0-9][0-9]?$", ""), "\\.", "")
      q_label_new <- str_replace(q_label_new, q_id_old_base, "")
      q_label_new <- str_replace(q_label_new, "^\\. ?", "")
      
      # if doesn't have ..some_number, then .1
      # if has ..some_number, then .some_number+1
      has_subno <- str_detect(q_id_old, "\\.\\.[0-9][0-9]?$")
      if (!has_subno) {
        q_id_new <- paste0(q_id_old, ".1")
      } else {
        q_id_new_subno <- as.integer(str_extract(q_id_old, "[0-9][0-9]?$")) + 1
        q_id_new <- paste0(str_replace(q_id_old, "\\.\\.([0-9][0-9]?)", "\\."), q_id_new_subno)
      }
      
      # remove leading zeros in question ID
      q_id_new <- str_replace(q_id_new, "^([A-Z])+0+", "\\1")
      
      
      
      if(exists("section_changes")) {
        section_changes <- rbind(section_changes, data.frame(OLDFILE = sav_file_path, OLDQID = q_id_old, NEWFILE=new_file_path, NEWQID = q_id_new, NEWQLABEL = q_label_new))
      } else {
        section_changes <- data.frame(OLDFILE = sav_file_path, OLDQID = q_id_old, NEWFILE=new_file_path, NEWQID = q_id_new, NEWQLABEL = q_label_new)
      }
    }
  }
  if (exists("section_changes")) return(section_changes)
  #else return(data.frame(OLDFILE = character(), OLDQID = character(), NEWQID = character(), NEWQLABEL = character(), NEWFILE = character()))
}


if (exists("df_out"))
  rm("df_out")
for (i in 1:nrow(new_old)) {
  if (!is.na(new_old$old[i])) {
    df <- new_section_labels(new_old$old[i])
    #print(i)
    if (!is.null(df)) {
      if (!exists("df_out")) {
        df_out <- df
      } else {
        df_out <- rbind(df_out, df)
      }
    }
  }
}

saveRDS(df_out, "data/working/update_2021_10_26/question_section_text_fixes.RDS")

question_section_text_fixes <- readRDS("data/working/update_2021_10_26/question_section_text_fixes.RDS")

fixes <- question_section_text_fixes[question_section_text_fixes$OLDFILE != "data/working/AMS040O.sav",]


for (i in 1:nrow(fixes)) {
  newfile <- paste0("data/working/update_2021_10_26/", basename(fixes[i,]$NEWFILE))
  newqid <- fixes[i,]$NEWQID
  newqlabel <- fixes[i,]$NEWQLABEL
  
  print(paste("loading", basename(fixes[i,]$NEWFILE)))
  tbl_haven_labelled <- haven::read_spss(newfile)
  
  print(paste("finding question", newqid))
  print(paste("changing label to", newqlabel))
  attr(tbl_haven_labelled[names(tbl_haven_labelled)==newqid][[1]], "label") <- newqlabel
  
  print("saving file")
  haven::write_sav(tbl_haven_labelled, newfile)
  print("success")
}





new_old[new == "data/working/AMS212_CLEAN_NOM_nocounts.sav", which = TRUE]

tbl_haven_labelled_test <- haven::read_spss(new_old$old[70])
tbl_haven_labelled_test$S0212.Q31C..9

tbl_haven_labelled_test_n <- haven::read_spss(new_old$new[70])
tbl_haven_labelled_test_n$S212.Q31C.10

new_section_labels(new_old$old[88])
