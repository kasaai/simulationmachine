source("data-raw/utils.R")

.translators <- parse_dir_to_env("data-raw/Translators")
.parameters <- parse_dir_to_env("data-raw/Parameters")

.lob_12 <- parse_dir_to_env("data-raw/LoB1and2")
.lob_34 <- parse_dir_to_env("data-raw/LoB3and4")

.list_of_variables <- read.table(file = paste("data-raw/List.of.Variables.txt", sep = ""), sep = "\t", header = TRUE)

usethis::use_data(
  .lob_12, 
  .lob_34, 
  .translators, 
  .parameters,
  .list_of_variables,
  internal = TRUE, overwrite = TRUE
)
