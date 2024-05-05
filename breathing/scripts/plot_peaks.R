plot_peaks <- function(data, intervals, plot_prefix) {
  # browser()
  
  peak_x <- c()
  peak_y <- c()
  
  volume <- c()
  
  for (i in 1:dim(intervals)[1]) {
    start <- intervals$start[i]
    end <- intervals$end[i]
    
    data_x <- c(data$t_minutes[start],
                data$t_minutes[start:end],
                data$t_minutes[end])
    
    data_y <- c(0,
                data$`0`[start:end],
                0)
    
    peak_x <- append(peak_x, data_x)
    peak_y <- append(peak_y, data_y)
    
    flow <- ggplot() +
      geom_line(aes(x = data_x-min(data_x), y = data_y/60)) +
      labs(x = "Time (min)", y = "Flow (SL/sec)", title = glue(plot_prefix, " - ", i))
    
    cumulative <- ggplot() +
      geom_line(aes(x = data_x-min(data_x), y = cumtrapz(data_x, data_y))) +
      labs(x = "Time (min)", y = "Volume (L)")
    
    flow_vol <- ggplot() +
      geom_line(aes(x = cumtrapz(data_x, data_y), y = data_y/60)) +
      labs(x = "Volume (L)", y = "Flow (SL/sec)")
    
    layout = rbind(c(1,2),
                          c(3, NA))
      
    
    # grid.arrange(flow, cumulative, flow_vol, layout_matrix = layout)
    
    peak_volume <- trapz(data_x, data_y)
    volume <- append(volume, peak_volume)
    
    print(paste("Peak", i , ":" , peak_volume))
    # 
    # assign(glue(plot_prefix, "_flow_", i), flow, envir = .GlobalEnv)
    # assign(glue(plot_prefix, "_cumulative_", i), cumulative, envir = .GlobalEnv)
    
  }
  
  return(volume)
}