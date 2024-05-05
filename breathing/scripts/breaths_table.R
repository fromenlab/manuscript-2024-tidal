format_mean_sd <- function(mean, sd, digits = 2, nsmall = 0L) {
  paste0(mean %>% format(digits=digits, nsmall = nsmall),
         " (\\textpm", sd %>% format(digits=digits, nsmall = nsmall), ")")
}

print_breath_summary <- function(data, trim = T, formatted_only = F) {
  if (trim) {
    data <- data %>% 
      subset(peak > min(peak) & peak < max(peak))
  }
  
  table1 <- data %>% 
    select(-contains("plot")) %>% 
    kbl(booktabs = T, align = "c") %>% 
    kable_paper(full_width = F) %>% 
    collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>% 
    kable_styling(latex_options = c("hold_position")) %>% 
    row_spec(0, bold = T)
  
  table2 <- data %>% 
    select(-contains("plot")) %>% 
    # group_by(maneuver) %>% 
    summary(digits = 2) %>% 
    kbl(booktabs = T, align = "c") %>% 
    kable_paper(full_width = F) %>% 
    collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>% 
    kable_styling(latex_options = c("hold_position", "scale_down")) %>% 
    row_spec(0, bold = T)
  
  table3 <- data %>% 
    mutate(samples = end - start) %>% 
    select(-contains("plot")) %>% 
    group_by(name, maneuver) %>% 
    summarise(across(c(samples, time_s:avg_q), list(mean = mean, sd = sd)), n = n()) %>% 
    t() %>% 
    kbl(booktabs = T, align = "c") %>% 
    kable_paper(full_width = F) %>% 
    collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")
  
  formatted_table <- data %>% 
    mutate(samples = end - start) %>% 
    select(-contains("plot")) %>% 
    group_by(name, maneuver) %>% 
    summarise(across(c(samples, time_s:avg_q), list(mean = mean, sd = sd)), n = n()) %>% 
    mutate(
      # Ugly but functional
      maneuver = case_when(
        maneuver == "e" ~ "Exhale",
        maneuver == "i" ~ "Inhale",
        TRUE ~ "?"
      ),
      time_formatted = "time_s" %>% (function(x){format_mean_sd(get(paste0(x,"_mean")), get(paste0(x, "_sd")))}),
      volume_formatted = "volume" %>% (function(x){format_mean_sd(get(paste0(x,"_mean")), get(paste0(x, "_sd")), nsmall = 2)}),
      max_q_formatted = "max_q" %>% (function(x){format_mean_sd(get(paste0(x,"_mean")), get(paste0(x, "_sd")))}),
      avg_q_formatted = "avg_q" %>% (function(x){format_mean_sd(get(paste0(x,"_mean")), get(paste0(x, "_sd")))})
    ) %>%
    select(name, maneuver, contains("_formatted"), n) %>% 
    # t() %>% 
    kbl(booktabs = T, align = "c", escape = F,
        col.names = c("Series", "Maneuver", "t (s)", "V (L)", "Q\\textsubscript{max} (SLPM)", "Q\\textsubscript{avg} (SLPM)", "n")) %>% 
    kable_paper(full_width = F) %>% 
    kable_styling(font_size = 9) %>% 
    row_spec(0, bold = T) %>% 
    collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle")
  
  if (formatted_only) {
    output = formatted_table
  } else {
    output = list(table1, table2, table3)
  }
  
  return(output)
}