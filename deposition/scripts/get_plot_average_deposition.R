get_plot_average_deposition <- function(data, dates) {
  # browser()
  
  selected <- data %>% 
    subset(date %in% dates) %>% 
    dplyr::group_by(date) %>% 
    mutate(
      pct = mass/sum(mass)*100
    )
  
  # Preserve the lobe and region for grouped plotting
  summarized <- selected %>%
    dplyr::ungroup() %>%
    replace_na(list(region="")) %>%
    mutate(compound_region = paste0(lobe, "-", region)) %>%
    dplyr::group_by(compound_region) %>% 
    mutate(mean = mean(pct), sd = sd(pct)) %>% 
    select(-c(mass, date, pct)) %>% 
    unique() %>% 
    dplyr::ungroup()
  
  return(summarized)
}