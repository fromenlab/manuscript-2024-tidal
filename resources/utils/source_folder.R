source_folder <- function(local_path) {
  paste0(local_path, "/", list.files(here::here(local_path))) %>% 
    here::here()  %>% 
    (function(x) {x[file_test("-f", x)]}) %>% 
    lapply(source)
}