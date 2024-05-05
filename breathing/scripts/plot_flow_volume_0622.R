plot_flow_volume <- function(e_start, e_end, i_start, i_end, d, steps, peak, data_name) {
  # browser()
  
  data <- get(paste(data_name))
  
  # For exhalation
  # Get raw data
  data_e_x <- c(data$t_minutes[e_start],
              data$t_minutes[e_start:e_end],
              data$t_minutes[e_end])
  
  data_e_y <- c(0,
              data$`0`[e_start:e_end],
              0)
  
  # Data for flow-volume loop. X-axis is exhaled volume
  e_vol = cumtrapz(data_e_x, data_e_y)
  
  
  # For inhalation
  # Get raw data
  data_i_x <- c(data$t_minutes[i_start],
                data$t_minutes[i_start:i_end],
                data$t_minutes[i_end])
  
  data_i_y <- c(0,
                data$`0`[i_start:i_end],
                0)
  
  # Data for flow-volume loop. 
  # X-axis is exhaled volume, so subtract from max volume
  i_vol = trapz(data_i_x, data_i_y) - cumtrapz(data_i_x, data_i_y)
  
  plot <- ggplot() +
    geom_line(aes(x = e_vol, y = data_e_y/60)) +
    geom_line(aes(x = i_vol, y = -data_i_y/60)) +
    labs(x = "Volume (L)", y = "Flow (SL/sec)",
         subtitle = glue("{data_name} - Breath {peak}"))
  
  plot
  
}

plot_flow_volume_normalized <- function(e_start, e_end, i_start, i_end, d, steps, peak, data_name) {
  # browser()
  
  data <- get(paste(data_name))
  
  # For exhalation
  # Get raw data
  data_e_x <- c(data$t_minutes[e_start],
                data$t_minutes[e_start:e_end],
                data$t_minutes[e_end])
  
  data_e_y <- c(0,
                data$`0`[e_start:e_end],
                0)
  
  # Data for flow-volume loop. X-axis is exhaled volume
  e_vol = cumtrapz(data_e_x, data_e_y)
  
  
  # For inhalation
  # Get raw data
  data_i_x <- c(data$t_minutes[i_start],
                data$t_minutes[i_start:i_end],
                data$t_minutes[i_end])
  
  data_i_y <- c(0,
                data$`0`[i_start:i_end],
                0)
  
  # Data for flow-volume loop. 
  # X-axis is exhaled volume, so subtract from max volume
  i_vol = trapz(data_i_x, data_i_y) - cumtrapz(data_i_x, data_i_y)
  
  plot <- ggplot() +
    geom_line(aes(x = e_vol/max(e_vol), y = data_e_y/max(data_e_y, data_i_y))) +
    geom_line(aes(x = i_vol/max(i_vol), y = -data_i_y/max(data_e_y, data_i_y))) +
    labs(x = expression(V/V[max]), y = expression(Q/Q[max]),
         subtitle = glue("{data_name} - Breath {peak}"))
  
  tibble::tibble(
    data_name = data_name,
    peak = peak,
    plot = list(plot),
    data_ex_x = list(data_e_x),
    data_ex_y = list(data_e_y),
    data_in_x = list(data_i_x),
    data_in_y = list(-data_i_y),
    v_ex = list(e_vol),
    v_ex_norm = list(e_vol/max(e_vol)),
    v_in = list(i_vol),
    v_in_norm = list(i_vol/max(i_vol)),
    q_ex = list(data_e_y),
    q_ex_norm = list(data_e_y/max(data_e_y, data_i_y)),
    q_in = list(data_i_y),
    q_in_norm = list(data_i_y/max(data_e_y, data_i_y))
  )
  
}