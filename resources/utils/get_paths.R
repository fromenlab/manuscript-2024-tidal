# Assuming make_path is sourced

# Output: a list of files with full path
# Input: a local path

get_paths <- function(local_path, parent_dir = parent) {
  # make_path required
  if (!exists("make_path")) {
    source(here::here("resources/utils/make_path.R"))
  }
  base_path <- make_path(local_path, parent_dir)
  files <- list.files(base_path)
  files_paths <- sapply(files, make_path, base_path, simplify = T)
  files <- files[file_test("-f", files_paths)]
  # Above needs to be vectorized
  paths <- here::here(paste0(base_path, "/", files))
  return(paths)
}