prep_json <- function(path) {
  read_file(file = path) %>% 
    paste0("[", ., "]") %>% 
    fromJSON()
}