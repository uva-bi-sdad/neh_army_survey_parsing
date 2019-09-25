
# data files
codebook <- readLines("data/original/Psychoneurotic Study (AMS-126), April-May 1944/Technical Documentation/AMS-126_ Psychoneurotic Study 04-05_1944, codebook.AMS0126.CDBK")
codebook_raw <- readr::read_file("data/original/Psychoneurotic Study (AMS-126), April-May 1944/Technical Documentation/AMS-126_ Psychoneurotic Study 04-05_1944, codebook.AMS0126.CDBK")
answer_file <- "data/original/Psychoneurotic Study (AMS-126), April-May 1944/Electronic Records/AMS-126_ Psychoneurotic Study 04-05_1944, data.AMS126.CLEAN"

# function - return all column widths of a single card
col_pos_lngth <- function(card) {
  multicolumns <- grep("COLS", card, value = TRUE)
  multicolumn_positions <- sub(".* ([0-9]{1,2}-[0-9]{1,2}).*", "\\1", multicolumns)
  first_columns <- 
    as.numeric(substr(multicolumn_positions, 1, regexpr("-", multicolumn_positions) - 1))
  last_columns <- 
    as.numeric(substr(multicolumn_positions, regexpr("-", multicolumn_positions) + 1, nchar(multicolumn_positions)))
  data.table::data.table(col_first_pos = first_columns, col_last_pos = last_columns, col_lngth = last_columns - first_columns + 1)
}


# determine nmber of cards
cards <- grep("CARD [1-9]", codebook)
card_cnt <- length(cards)

# set variables
card_cols <- 80L
card_start <- cards[1]
card_ls <- list()
card_col_widths <- list()

# create list of separate cards
for (i in 1:card_cnt) {
  if (i + 1 <= card_cnt) card_end = cards[i + 1] else card_end = length(codebook)
  card_ls[i] <- list(codebook[card_start:card_end])
  card_start <- card_end
}

# create list of separate card column widths
cnt <- 1

for (c in card_ls) {
  multicolumn_positions_lengths <- col_pos_lngth(c)
  
  dt <- data.table::data.table(col_pos = as.numeric(), col_lngth = as.numeric())
  # browser()
  current_col_pos <- 1
  for (i in 1:80) {
    #dt0 <- data.table::data.table(col_pos = as.numeric(), col_lngth = as.numeric())
    if (i >= current_col_pos) {
      if (nrow(multicolumn_positions_lengths[col_first_pos==i]) == 0) {
        dt1 <- data.table::data.table(col_pos = current_col_pos, col_lngth = 1)
        current_col_pos <-  current_col_pos + 1
      } else {
        dt1 <- data.table::data.table(col_pos = current_col_pos, col_lngth = multicolumn_positions_lengths[col_first_pos==i, col_lngth])
        # current_col_pos <-  current_col_pos + multicolumn_positions_lengths[col_first_pos==i, col_last_pos] -1
        current_col_pos <-  multicolumn_positions_lengths[col_first_pos==i, col_last_pos] + 1
      }
      #dt0 <- data.table::rbindlist(list(dt0, dt1))
      dt <- data.table::rbindlist(list(dt, dt1))
    }
  }
  
  card_col_widths[cnt] <- list(dt)
  
  cnt <- cnt + 1
}

# parse the data file using the separate card column widths to create a combined single data record per respondent
answers_parsed <-  read.fwf(answer_file, list(card_col_widths[[1]][, col_lngth], card_col_widths[[2]][, col_lngth]))



for (i in grep("COL", codebook, value = FALSE)) {
  start <- i - 2
  start2 <- i - 1
  mc <- codebook[start:i]
  mc2 <- codebook[i]
  conct <- rbind(mc, mc2)
  print(conct)
  
  # mc_positions <- sub(".* ([0-9]{1,2}-[0-9]{1,2}).*", "\\1", mc)
  # print(mc_positions)
}

