expand_flow_time <- function(input_data) {
  bind_rows(
    input_data %>% 
      select(c(data_name, peak, data_ex_x, data_ex_y)) %>% 
      unnest(cols = c(data_ex_x, data_ex_y)),
    input_data %>% 
      select(c(data_name, peak, data_in_x, data_in_y)) %>% 
      unnest(cols = c(data_in_x, data_in_y))
  )
}

expand_flow_vol <- function(input_data) {
  bind_rows(
    input_data %>% 
      select(c(data_name, peak, q_ex, v_ex)) %>% 
      unnest(cols = c(q_ex, v_ex)),
    input_data %>% 
      select(c(data_name, peak, q_in, v_in)) %>% 
      unnest(cols = c(q_in, v_in))
  )
}

# Normalized flow-volume (maneuver basis)
expand_flow_vol_norm <- function(input_data) {
  bind_rows(
    input_data %>% 
      select(c(data_name, peak, q_ex_norm, v_ex_norm)) %>% 
      unnest(cols = c(q_ex_norm, v_ex_norm)),
    input_data %>% 
      select(c(data_name, peak, q_in_norm, v_in_norm)) %>% 
      unnest(cols = c(q_in_norm, v_in_norm))
  )
}

expand_flow_abs_vol_norm <- function(input_data) {
  bind_rows(
    input_data %>% 
      select(c(data_name, peak, q_ex, v_ex_norm)) %>% 
      unnest(cols = c(q_ex, v_ex_norm)),
    input_data %>% 
      select(c(data_name, peak, q_in, v_in_norm)) %>% 
      unnest(cols = c(q_in, v_in_norm))
  )
}

plot_flow_index <- function(input_data) {
  input_data %>% ggplot(aes(x = `...1`, y = `0`)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(limits = c(0,900)) + 
    scale_y_continuous(limits = c(0, 50)) +
    labs(x = "Entry", y = "Flow rate (SLPM)") +
    theme_bw()
}

plot_flow_time <- function(input_data) {
  input_data$peak <- factor(input_data$peak)
  
  (input_data %>% ggplot() +
      geom_line(aes(x = data_ex_x, y = data_ex_y, color = peak, group = peak)) +
      geom_line(aes(x = data_in_x, y = data_in_y, color = peak, group = peak)) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Time (min)", y = "Flow rate (SLPM)") +
      theme_bw())
}

plot_flow_vol <- function(input_data) {
  input_data$peak <- factor(input_data$peak)
  
  (input_data %>% ggplot() +
      geom_line(aes(x = v_ex, y = q_ex, color = peak, group = peak)) +
      geom_line(aes(x = v_in, y = -q_in, color = peak, group = peak)) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Volume (L)", y = "Flow rate (SLPM)") +
      theme_bw())
}

plot_flow_vol_norm <- function(input_data) {
  input_data$peak <- factor(input_data$peak)
  
  (input_data %>% ggplot() +
      geom_line(aes(x = v_ex_norm, y = q_ex_norm, color = peak, group = peak)) +
      geom_line(aes(x = v_in_norm, y = -q_in_norm, color = peak, group = peak)) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Normalized volume", y = "Normalized flow rate") +
      theme_bw())
}

peaks_analysis <- function(name) {
  dataset = get(paste0("data_", name))
  peaks = get(paste0("peaks_", name))
  
  peaks <- peaks %>% mutate(volume = plot_peaks(dataset, peaks, name))
  
  try((function() {
    aov_vol <- aov(peaks, formula = volume ~ maneuver) %>% summary()
    
    t.test(volume ~ maneuver, data = peaks) %>% print()
    
    peaks_plot <- peaks %>% 
      ggplot() +
      geom_point(aes(x = maneuver, y = volume)) +
      geom_prismbars(aes(x = maneuver, y = volume)) +
      labs(x = "Maneuver", y = "Volume (L)", 
           title = "Cumulative Volume", subtitle = glue("p = ", aov_vol[[1]]$`Pr(>F)`[1] %>% signif(1))) +
      theme_bw()
    
    peaks_plot %>% print()
  }))
  
  refs <- peaks %>% mutate(dataset_name = glue("data_", name)) %>% pivot_wider(names_from = maneuver, values_from = c(start, end, volume), names_glue = "{maneuver}_{.value}")
  
  flow_vol <- pmap_dfr(list(refs$e_start, refs$e_end, refs$i_start, refs$i_end, 
                            peak = refs$peak, 
                            data_name = refs$dataset_name), 
                       plot_flow_volume_normalized)
  
  # plots = list(
  #   flow_vol %>% expand_flow_time() %>% plot_flow_time(),
  #   flow_vol %>% expand_flow_vol() %>% plot_flow_vol(),
  #   flow_vol %>% expand_flow_vol_norm() %>% plot_flow_vol_norm()
  # )
  
  return(flow_vol)
  
}

plot_breaths <- function(breath_data) {
  plots = list(
    breath_data %>% expand_flow_time() %>% plot_flow_time(),
    breath_data %>% expand_flow_vol() %>% plot_flow_vol(),
    breath_data %>% expand_flow_vol_norm() %>% plot_flow_vol_norm()
  )
  
  return(plots)
  
}

peaks_analysis_corrected <- function(name) {
  dataset = get(paste0("data_", name))
  peaks = get(paste0("peaks_", name))
  
  peaks <- peaks %>% mutate(volume = plot_peaks_corrected(dataset, peaks, name))
  
  aov_vol <- aov(peaks, formula = volume ~ maneuver) %>% summary()
  
  t.test(volume ~ maneuver, data = peaks) %>% print()
  
  peaks_plot <- peaks %>% 
    ggplot() +
    geom_point(aes(x = maneuver, y = volume)) +
    geom_prismbars(aes(x = maneuver, y = volume)) +
    labs(x = "Maneuver", y = "Volume (L)", 
         title = "Cumulative Volume", subtitle = glue("p = ", aov_vol[[1]]$`Pr(>F)`[1] %>% signif(1))) +
    theme_bw()
  
  peaks_plot %>% print()
  
  refs <- peaks %>% mutate(dataset_name = name) %>% pivot_wider(names_from = maneuver, values_from = c(start, end, volume), names_glue = "{maneuver}_{.value}")
  
  flow_vol <- pmap_dfr(list(refs$e_start, refs$e_end, refs$i_start, refs$i_end, 
                            peak = refs$peak, 
                            data_name = refs$dataset_name), 
                       plot_flow_volume_normalized)
  
  list(
    flow_vol %>% expand_flow_time() %>% plot_flow_time(),
    flow_vol %>% expand_flow_vol() %>% plot_flow_vol(),
    flow_vol %>% expand_flow_vol_norm() %>% plot_flow_vol_norm()
  )
  
}