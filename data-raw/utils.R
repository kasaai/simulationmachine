library(purrr)

read_txt <- function(path) {
  as.matrix(
    utils::read.table(path,  sep = "\t", header = TRUE)
  )
}

parse_dir_to_env <- function(path) {
  fs::dir_ls(path, type = "directory") %>% 
    map(fs::dir_ls) %>% 
    map_depth(2, read_txt) %>%
    set_names(basename(names(.))) %>% 
    map(~ set_names(.x, .x %>% 
                      names() %>% 
                      basename() %>% 
                      tools::file_path_sans_ext())
    ) %>% 
    list2env()
}