get_summary_average_deposition <- function(data, ...) {
  selected <- data %>% 
    dplyr::group_by(date) %>% 
    mutate(
      pct = mass/sum(mass)*100
    )
  
  summarized <- selected %>%
    replace_na(list(region="")) %>%
    # Get sum percentages of select regions by experiment
    dplyr::group_by(date, ...) %>% 
    summarize(pct = sum(pct)) %>% 
    # Get average percentage
    dplyr::group_by(...) %>% 
    summarize(mean = mean(pct),
              sd = sd(pct),
              values = list(pct))
  
  return(summarized)
}