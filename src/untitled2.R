AMS032N_sav <- haven::read_sav("data/working/AMS032N.sav")
AMS032N_qlabs <- data.table::fread("data/temp/AMS32NmetaQuestionLabels.txt", quote = "")

AMS032N_qlabs <- jsonlite::read_json("data/temp/AMS32NmetaQuestionLabels.txt")
AMS032N_alabs <- jsonlite::read_json("data/temp/AMS32NmetaAnswerLabels.txt")


AMS35_qlabs <- jsonlite::read_json("data/temp/AMS35metaQuestionLabels.txt")
AMS35_alabs <- jsonlite::read_json("data/temp/AMS35metaAnswerLabels.txt")


qs <- data.table::fread("data/temp/questions.csv")


test1$S032N.Q42a

nchar("VETERANS SHOULD GET FIRST CHANCE AT ANY GOVERNMENT JOBS THEY CAN DO, EVEN IF SOME NON-VETERANS COULD DO THE SAME WORK MU")


haven::write_sav(test1, "data/working/test1.sav")

test2 <- haven::read_sav("data/working/test1.sav")

test2$S032N.Q42a
