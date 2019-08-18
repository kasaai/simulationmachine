source("data-raw/utils.R")

.lob_12 <- parse_dir_to_env("data-raw/LoB1and2")
.lob_34 <- parse_dir_to_env("data-raw/LoB3and4")

usethis::use_data(.lob_12, internal = TRUE, overwrite = TRUE)
usethis::use_data(.lob_34, internal = TRUE, overwrite = TRUE)