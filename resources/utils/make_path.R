make_path <- function(local_path, parent_dir = parent) {
  path = here::here(paste0(parent_dir, "/", local_path))
  
  return(path)
}