calc_cp <- function(data, transform = FALSE) {
  c <- data %>% subset(region %in% "c")
  
  p <- data %>% subset(region %in% "p")
  
  # term1  = (c$sd/c$mean)^2
  # term2 = (p$sd/p$mean)^2
  # term3 = 2*cov(c$values %>% unlist(), p$values %>% unlist())/(c$mean*p$mean)
  # 
  # mean = c$mean/p$mean
  # sd = abs(mean) * sqrt(term1 + term2 - term3)
  
  # values are in order of date; pairwise operations
  values = c(c$values %>% unlist() / p$values %>% unlist())
  
  if (transform) {
    values = lapply(values, convert_to_planar) %>% unlist()
  }
  
  cp <- list(
    mean = mean(values),
    sd = sd(values),
    values = values
  )
  
  return(cp)
}