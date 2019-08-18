source("data-raw/utils.R")

.translators <- parse_dir_to_env("data-raw/Translators")
.parameters <- parse_dir_to_env("data-raw/Parameters")

usethis::use_data(.translators, internal = TRUE, overwrite = TRUE)
usethis::use_data(.parameters, internal = TRUE, overwrite = TRUE)