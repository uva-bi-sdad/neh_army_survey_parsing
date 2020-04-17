answerscsv <- read.csv("data/working/AMS0035_answers.csv")
questionsscsv <- read.csv("data/working/AMS0035_questions.csv", header = F)


codebook_raw <- data.frame(id = 1:nrow(questionsscsv), "raw" = questionsscsv[,1]) %>% 
  mutate(q_nickname = names(answerscsv),
         # q_nickname = stringr::str_extract(raw, 
         # "(Q\\.\\d*\\.?[[:alpha:]]?|V\\.\\d*\\.?[[:alpha:]]?|(CARD|DECK|BALLOT|FORM)\\.?\\s*(\\d*|[[:alpha:]])\\.)"),
         # q_nickname = ifelse(!is.na(q_nickname), q_nickname, stringr::str_extract(raw, "X.([^.]+)")), #X.([^.]+)
         q_asterisk = stringr::str_detect(raw, "\\*"),
         question = stringr::str_extract(raw, "Q\\..*\\*"))


codebook_raw$question <- stringr::str_replace_all(stringr::str_replace_all(codebook_raw$question, "\\(", "\\\\\\("), "\\)", "\\\\\\)")

codebook_raw <- codebook_raw %>% 
  mutate(code_raw_text_minus_question = stringr::str_remove(raw, pattern = question))

codes <- codebook_raw %>% 
  transmute(id, q_nickname, code_raw_text_minus_question, 
            codes_raw = stringr::str_extract_all(code_raw_text_minus_question, "\\d?\\d\\.\\s?")) %>%
  tidyr::unnest(c("codes_raw")) %>% 
  mutate(codes = stringr::str_extract(codes_raw, "^\\d\\d?"))

responses <- codebook_raw %>% 
  transmute(id, q_nickname, code_raw_text_minus_question, 
            responses_raw = stringr::str_split(code_raw_text_minus_question, "\\d\\.")) %>%
  tidyr::unnest(c("responses_raw")) %>%
  mutate(responses_raw = stringr::str_squish(responses_raw)) %>% 
  filter(responses_raw != "*" & dataplumbr::var.is_blank(responses_raw) == FALSE) 


### PRINT EXAMPLE TO SHARE WITH PROJECT GROUP

check = codes %>% count(id, q_nickname) %>% left_join(responses %>% count(id, q_nickname), by = c("id", "q_nickname")) %>% mutate(check = n.x - n.y)

exampleqs <- check %>% filter(check == 0) %>% .$id
exampleas <- answerscsv[,exampleqs]

examplecodebook <- codebook_raw %>% filter(id %in%exampleqs)
examplecodes <- codes %>% filter(id %in% exampleqs)
exampleresponses <- responses %>% filter(id %in% exampleqs)
example_response_text <- data.frame(id = examplecodes$id, q_nickname = examplecodes$q_nickname, codes = examplecodes$codes, labels = exampleresponses$responses_raw)

#
example_data <- as.data.frame(apply(exampleas, MARGIN = 2, FUN = factor))


for (i in 1:ncol(example_data)) {
  # example_response_text %>% filter(q_nickname == colnames(example_data[i])) %>% .$labels
  response_text <- factor(example_response_text %>% filter(q_nickname == colnames(example_data[i])) %>% .$labels )
  levels(example_data[,i]) <- response_text
  attr(example_data[,i], "labels") <- response_text
  names(attr(example_data[,i], "labels")) <- response_text
  attr(example_data[,i], "label") <- examplecodebook$question[i]
  names(attr(example_data[,i], "label")) <- examplecodebook$question[i]
}

example_data_min2cols <- example_data[,colnames(example_data) %in% c("Q.20C.", "Q.27.") == FALSE]

str(example_data_min2cols)

write_sav(example_data_min2cols, "test8.sav")
