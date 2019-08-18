list.of.variables <- read.table(file = paste("data-raw/List.of.Variables.txt", sep = ""), sep = "\t", header = TRUE)

usethis::use_data(list_of_variables, internal = TRUE, overwrite = TRUE)