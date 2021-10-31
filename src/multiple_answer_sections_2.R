library("data.table")
library(tools)
library(stringr)


get_new_qid <- function(q_id_old) {
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
  q_id_new
}

find_sav_E_value_labels <- function(sav_file_path) {
  tbl_haven_labelled <- haven::read_spss(sav_file_path)
  for (i in 1:length(tbl_haven_labelled)) {
    if (any(str_detect(names(attr(tbl_haven_labelled[i][[1]], "labels")), ".* E\\. .*")) == TRUE) {
      QROW <- i
      QID <- attr(tbl_haven_labelled[i][1], "names")
      QGROUP <- str_match(QID, "^(.*)\\.?\\.?")[,2]
      if(str_detect(QID, "\\.\\.")) {
        QGROUP <- str_match(QID, "^(.*)\\.\\.")[,2]
      } else {
        QGROUP <- QID 
      }
      QLABEL <- attr(tbl_haven_labelled[i][[1]], "label")
      QVLROW <- which(str_detect(names(attr(tbl_haven_labelled[i][[1]], "labels")), " E\\. "))
      QVLABEL <- names(attr(tbl_haven_labelled[i][[1]], "labels"))[QVLROW] %>%
        str_match(" E\\. .*?[:\\.]")
      QVLABEL <- QVLABEL[1,1]
      if (is.na(QVLABEL)) next
      
      QVE <- data.table(QGROUP, QROW, QID, QLABEL, QVLABEL)
      
      if (exists("QVEs")) QVEs <- rbindlist(list(QVEs, QVE))
      else QVEs <- QVE
    }
  }
  if (exists("QVEs")) QVEs
}

qves <- find_sav_E_value_labels("data/working/AMS0212.sav")
qves <- find_sav_E_value_labels("data/working/AMS0044T.sav")
qves <- find_sav_E_value_labels("data/working/AMS205E.sav")


find_sav_qs_with_wrong_E_label <- function(sav_file_path, qves) {
  if(exists("e_groups")) rm(e_groups)
  tbl_haven_labelled <- haven::read_spss(sav_file_path)
  for (g in unique(qves$QGROUP)) {
    #browser()
    qves_grp <- qves[QGROUP==g,]
    qves_grp_max_row <- max(which(str_detect(attr(tbl_haven_labelled, "names"), g)))
    if (is.infinite(qves_grp_max_row)) {
      next
    }
    for (i in 1:nrow(qves_grp)) {
      e_lab <- qves_grp[i,]$QVLABEL
      start_row <- qves_grp[i,]$QROW + 1
      end_row <- qves_grp[i + 1,]$QROW
      if (is.na(end_row)) end_row <- qves_grp_max_row
      
      if (end_row < start_row) {
        next
      }
      
      e_group <- data.table(g, e_lab, start_row, end_row)
      if(exists("e_groups")) e_groups <- rbindlist(list(e_groups, e_group))
      else e_groups = e_group
    }
  }
  if (exists("e_groups")) e_groups
}

egrps <- find_sav_qs_with_wrong_E_label("data/working/AMS0212.sav", qves)
egrps <- find_sav_qs_with_wrong_E_label("data/working/AMS0044T.sav", qves)
egrps <- find_sav_qs_with_wrong_E_label("data/working/AMS205E.sav", qves)


check_fix_egrps <- function(sav_file_path, egrps) {
  tbl_haven_labelled <- haven::read_spss(sav_file_path)
  for (i in 1:nrow(egrps)) {
    for (r in egrps[i]$start_row:egrps[i]$end_row) {
      QLABEL <- attr(tbl_haven_labelled[r][[1]], "label")
      QID <- attr(tbl_haven_labelled[r][1], "names")
      NEWQID <- get_new_qid(QID)
      #browser()
      if (str_detect(QLABEL, " E\\. ")) {
        QLABEL_E <- str_match(QLABEL, " E\\. .*")
        QLABEL_E <- QLABEL_E[1,1]
        if (!str_detect(QLABEL, egrps[i]$e_lab)) {
          #attr(tbl_haven_labelled[r][[1]], "label") <- 
          new_q_label <- str_replace(QLABEL, QLABEL_E, egrps[i]$e_lab)
          new_q_id <- NEWQID
          new_out_row <- data.table(new_q_id, new_q_label)
          if(!exists("out_rows")) out_rows <- new_out_row
          else out_rows <- rbindlist(list(out_rows, new_out_row))
        }
      }
    }
  }
  #tbl_haven_labelled
  if (exists("out_rows")) out_rows
}

fixed_labels <- check_fix_egrps("data/working/AMS0212.sav", egrps)
fixed_labels <- check_fix_egrps("data/working/AMS0044T.sav", egrps)


orig_sav_files <- as.data.table(list.files("data/working", pattern = "*.sav", full.names = TRUE))
orig_sav_files <- orig_sav_files[!V1 %like% "_CLEAN_"]
# sav_file_path <- "data/working/AMS0212.sav"
new_file_names <- fread("data/working/update_2021_10_31/SAV_file_matching.csv")


fix <- function(file_paths) {
  for (f in file_paths) {
    #browser()
    print(f)
    qves <- find_sav_E_value_labels(f)
    if (!is.null(qves)) egrps <- find_sav_qs_with_wrong_E_label(f, qves)
    else next
    if (!is.null(egrps)) fixed <- check_fix_egrps(f, egrps)
    else next
    if (!is.null(fixed)) fixed$new_file <- new_file_names[BI==basename(f), DataBridge]
    else next
    if (exists("fixed_all")) fixed_all <- rbindlist(list(fixed_all, fixed))
    else fixed_all <- fixed
  }
  if (exists("fixed_all")) fixed_all
}


all_fixed <- fix(orig_sav_files[, V1])

all_fixed[, new_q_label := str_replace(new_q_label, "S0.*?\\. ", "")]
all_fixed[, new_q_label := str_replace(new_q_label, " +[0-9]+$", "")]
all_fixed <- unique(all_fixed)
all_fixed[, new_file := str_replace(new_file, ".sav", "_nocounts.sav")]
fnl <- all_fixed[, .(file = new_file, qid = new_q_id, new_label = new_q_label)]

write.csv(fnl, "data/working/update_2021_10_31/E_Section_Fixes.csv", row.names = F)


# Write Fixes To SAV Files

readr::write_lines(paste("qid", fnl[i]$qid, "does not exist"),
                   "Rewrite Question Labels Issues",
                   append = FALSE)
for (i in 1:nrow(fnl)) {
  print(i)
  print(fnl[i])
  file_path <- paste0("data/working/update_2021_10_31/", fnl[i]$file)
  tbl_hvn <- haven::read_spss(file_path)
  if (any(names(tbl_hvn) == fnl[i]$qid)) {
    attr(tbl_hvn[names(tbl_hvn) == fnl[i]$qid][[1]], "label") <- fnl[i]$new_label
    haven::write_sav(tbl_hvn, file_path)
  } else {
    readr::write_lines(paste("qid", fnl[i]$qid, "does not exist"),
                       "data/working/update_2021_10_31/q_label_problems.txt",
                       append = TRUE)
  }
}
 

