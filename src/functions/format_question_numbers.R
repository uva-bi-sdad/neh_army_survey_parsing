format_question_numbers <- function(str_v, q_pref) {
  str_replace(str_v, "^(Q|V)\\. ", "\\1\\.") %>% 
    str_replace("^(Q|V)\\.", paste0(q_pref, "\\1")) %>% 
    str_replace("^(CARD|DECK|BALLOT|FORM)", paste0(q_pref, "\\1"))
}
