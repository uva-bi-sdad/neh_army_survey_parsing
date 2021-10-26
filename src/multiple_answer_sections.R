library("data.table")
library(tools)
library(stringr)

apos_files <- list.files("data/working", pattern = "*nocounts.sav", full.names = TRUE)
rm(suspect_files)
for (f in apos_files) {
  tbl_haven_labelled <- haven::read_spss(f)
  for (i in 1:length(tbl_haven_labelled)) {
    if (str_detect(attr(tbl_haven_labelled[i][[1]], "label"), ".* E. .*") == TRUE) {
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



new_section_labels <- function(sav_file_path) {
  tbl_haven_labelled <- haven::read_spss(sav_file_path)
  
  if(exists("section_changes")) rm(section_changes)
  
  for (i in 1:length(tbl_haven_labelled)) {
    #print(i)
    #browser()
    if (str_detect(attr(tbl_haven_labelled[i][[1]], "label"), ".* E. .*") == TRUE &
        any(str_detect(names(attr(tbl_haven_labelled[i][[1]], "labels")), ".* E. .*")) == TRUE) {
      
      q_label_old <- attr(tbl_haven_labelled[i+1][[1]], "label")
      q_label_subsec_old <- str_match(q_label_old, ".* (E. .*)")[2]
      str_ls <- str_match(names(attr(tbl_haven_labelled[i][[1]], "labels")), ".* (E. .*)")[,2]
      q_label_subsec_new <- str_ls[!is.na(str_ls)]
      q_label_new <- str_replace(q_label_old, q_label_subsec_old, q_label_subsec_new)
      
      q_id_old <- attr(tbl_haven_labelled[i+1][1], "names")
      
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
        section_changes <- rbind(section_changes, data.frame(OLDFILE = basename(sav_file_path), OLDQID = q_id_old, NEWQID = q_id_new, NEWQLABEL = q_label_new))
      } else {
        section_changes <- data.frame(OLDFILE = basename(sav_file_path), OLDQID = q_id_old, NEWQID = q_id_new, NEWQLABEL = q_label_new)
      }
      section_changes$NEWFILE <- basename(new_old$new[i])
    }
  }
  if (exists("section_changes")) return(section_changes)
  else return(data.frame(OLDFILE = character(), OLDQID = character(), NEWQID = character(), NEWQLABEL = character(), NEWFILE = character()))
}



df_out <- 

for (i in 1:30) {
  dt <- new_section_labels(new_old$old[i])
  if (nrow(dt) > 0) {
    #dt$NEWFILE <- basename(new_old$new[i])
    print(dt[, c("OLDFILE", "OLDQID", "NEWFILE", "NEWQID", "NEWQLABEL")])   
  }
}



tbl_haven_labelled <- haven::read_spss(new_old$new[19])
tbl_haven_labelled$S100B.Q4.2



new_old[new %like% "AMS107O_CLEAN_NOM_nocounts.sav", which = TRUE]
