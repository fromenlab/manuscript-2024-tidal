calc_lr <- function(data) {
  l <- data %>% subset(side %in% "left")
  
  r <- data %>% subset(side %in% "right")
  
  # values are in order of date; pairwise operations
  values = c(l$values %>% unlist() / r$values %>% unlist())
  
  lr <- list(
    mean = mean(values),
    sd = sd(values),
    values = values
  )
  
  return(lr)
}