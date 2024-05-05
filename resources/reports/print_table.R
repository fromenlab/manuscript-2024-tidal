print_kable <- function(data, transpose = FALSE) {
  
  data %>% 
    mutate_if(is.numeric, format, digits=4, nsmall = 0) %>%
    (function(x) {
      if (transpose) {
        x <- x %>% t()
      } 
      return (x)
    }) %>% 
    kbl(align = "c") %>%
    kable_paper(full_width = F) %>% 
    kable_styling(latex_options = c("hold_position"))
  
}