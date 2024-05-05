capitalize <- function(string) {
  string = gsub("_", " ", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}