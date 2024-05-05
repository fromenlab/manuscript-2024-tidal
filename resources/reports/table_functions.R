# For dissertation purposes

kbl_base <- function(data, column_names = NULL) {
  if (!missing(column_names)) {
    table <- data %>% 
      kbl(booktabs = T,
          col.names = column_names) %>%
      row_spec(0, bold = T) %>% 
      kable_styling(font_size = 9)
  } else {
    table <- data %>% 
      kbl(booktabs = T) %>%
      row_spec(0, bold = T) %>% 
      kable_styling(font_size = 9)
  }
  
  return(table)
}