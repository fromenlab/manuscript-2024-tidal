# Created 202230317
# v0.1.0 - 20230321

make_path <- function(local_path) {
  path = here::here(paste0(parent, local_path))
}

read_flow_folder <- function(local_path, sample_rate_ms, name) {
  # List files in directory
  files <- list.files(make_path(local_path))
  files_paths <- make_path(paste0(local_path, "/", files))
  
  # Get files that exist
  # files <- files[file_test("-f", files_paths)]
  files_paths <- files_paths[file_test("-f", files_paths)]
  
  # Read all data
  data <- files_paths %>% 
    read_csv() %>% 
    mutate(...1 = row_number()-1, # maintain 0-based indexing for compatibility
           index_r = row_number(), # R, 1-based indexing for consistency within future analysis
           t_minutes = ...1 * sample_rate_ms * 1e-3 * 1/60, # X ms/sample * 1e-3 s/ms* 1 min/60s 
           t_seconds = ...1 * sample_rate_ms * 1e-3,
           q_slpm = `0`)
  
  # Save data in global environment
  data %>% 
    assign(paste0("data_", name), value = .,  envir = .GlobalEnv)
  
  return(data)
}

plot_flow_index_r <- function(input_data) {
  # 20230626 - set limits on x axis from NA to NA, to reduce data 
  # to only selected range
  input_data %>% ggplot(aes(x = index_r, y = `0`)) + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(limits = c(NA, NA)) + 
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Entry", y = "Flow rate (SLPM)") +
    theme_bw()
}

make_peaks_index_r <- function(peak_data, name) {
  if (missing(name)) {
    name = get("name", envir = .GlobalEnv)
  }
  
  peak_data <- peak_data %>% mutate(
    name = name
  )

  assign(paste0("peaks_", name), peak_data, envir = .GlobalEnv)
  
  return(peak_data)
}

make_breaths <- function(name, breath_data) {
  assign(paste0("breaths_", name), breath_data, envir = .GlobalEnv)
}

get_data <- function(name) {
  return(get(paste0("data_", name)))
}

get_peaks <- function(name) {
  return(get(paste0("peaks_", name)))
}

get_breaths <- function(name) {
  return(get(paste0("breaths_", name)))
}

# breaths_analysis <- function(name) {
#   dataset = get(paste0("data_", name))
#   peaks = get(paste0("peaks_", name))
#   
#   peaks <- peaks %>% mutate(
#     dataset_name = paste0("data_", name),
#     volume = calculate_breath_volume(dataset, peaks))
#   
#   assign(paste0("peaks_", name), peaks, envir = .GlobalEnv)
# }
# 
# calculate_breath_volume <- function(data, intervals) {
#   volume <- c()
#   
#   # For each of the rows, corresponding to each maneuver...
#   for (i in 1:dim(intervals)[1]) {
#     
#     # Get start index
#     start <- intervals$start[i]
#     
#     # Get end index
#     end <- intervals$end[i]
#     
#     # Get data for time 
#     data_x <- c(data$t_minutes[start],
#                 data$t_minutes[start:end],
#                 data$t_minutes[end])
#     
#     # Get data for flow rate
#     data_y <- c(0,
#                 data$`0`[start:end],
#                 0)
#     
#     peak_volume <- trapz(data_x, data_y)
#     volume <- append(volume, peak_volume)
#   }
#   
#   return(volume)
# }

get_breath_data <- function(name, peak, start, end, maneuver) {
  
  # Get data from global environment
  data <- get_data(name)
  
  # Get data for time 
  data_x <- c(data$t_minutes[start],
              data$t_minutes[start:end],
              data$t_minutes[end])
  
  # Get data for flow rate
  data_y <- c(0,
              data$q_slpm[start:end],
              0)
  
  # Data for flow-volume loop. 
  # X-axis is exhaled volume, so subtract from max volume if inhalation
  if (maneuver == "e") {
    plot_vol = cumtrapz(data_x, data_y)
    plot_q_slpm = data_y
  } else if (maneuver == "i") {
    plot_vol = trapz(data_x, data_y) - cumtrapz(data_x, data_y)
    # plot_vol = cumtrapz(data_x, data_y)
    plot_q_slpm = data_y * -1
  }
  
  breath_data <- tibble::tibble(
    name = name,
    peak = peak,
    maneuver = maneuver,
    start = start,
    end = end,
    time_s = (data$t_minutes[end] - data$t_minutes[start])*60,
    volume = trapz(data_x, data_y),
    max_q = max(data_y),
    avg_q = mean(data_y),
    plot_t_minutes = data_x %>% list(),
    plot_q_slpm = plot_q_slpm %>% list(),
    plot_vol = plot_vol %>% list()
  )
  
  return(breath_data)
}

plot_flow_time <- function(input_data, use_alpha = F, alpha = 0.5) {
  # 20240130 - Better would be to just specify alpha = 1 as default, then let user specify
  plot <- input_data %>% 
    select(c(peak, maneuver, plot_t_minutes, plot_q_slpm)) %>% 
    unnest(cols = c(plot_t_minutes, plot_q_slpm)) %>% 
    (function(input_data) {
      input_data$peak <- factor(input_data$peak)
      
      (input_data %>% ggplot() +
          {if(!use_alpha)list(geom_line(aes(x = plot_t_minutes-min(plot_t_minutes), y = plot_q_slpm, color = maneuver, 
                        group = interaction(peak, maneuver))))} +
          {if(use_alpha)list(geom_line(aes(x = plot_t_minutes-min(plot_t_minutes), y = plot_q_slpm, color = maneuver, 
                                            group = interaction(peak, maneuver)), alpha = alpha))} +
          labs(x = "Time (min)", y = "Flow rate (SLPM)") +
          scale_color_brewer(palette = "Set1", 
                             breaks = c("e", "i"), labels = c("Exhale", "Inhale"),
                             guide = guide_legend(title = "",
                                                  override.aes = list(size = 5))) +
          # guides(color = guide_legend()) +
          theme_bw() +
          theme(legend.position = "bottom"))
    })
  
  return(plot)
}

plot_flow_volume <- function(input_data, use_alpha = F, alpha = 0.5) {
  plot <- input_data %>% 
  select(c(peak, maneuver, plot_vol, plot_q_slpm)) %>% 
    unnest(cols = c(plot_vol, plot_q_slpm)) %>% 
    (function(input_data) {
      input_data$peak <- factor(input_data$peak)
      
      (input_data %>% ggplot() +
          {if(!use_alpha)list(geom_line(aes(x = plot_vol, y = plot_q_slpm, color = peak, 
                        group = interaction(peak, maneuver))),
          scale_color_brewer(palette = "Set1"))} +
          
          # Use no colors for publication plots
          {if(use_alpha)list(geom_line(aes(x = plot_vol, y = plot_q_slpm, 
                                           group = interaction(peak, maneuver)), alpha = alpha))} +
          labs(x = "Volume (L)", y = "Flow rate (SLPM)") +
          theme_bw())
    })
  
  return(plot)
}

plot_volume_time <- function(input_data) {
  plot <- input_data %>% 
    select(c(peak, maneuver, plot_t_minutes, plot_vol)) %>% 
    unnest(cols = c(plot_t_minutes, plot_vol)) %>% 
    (function(input_data) {
      input_data$peak <- factor(input_data$peak)
      
      (input_data %>% ggplot() +
          geom_line(aes(x = plot_t_minutes-min(plot_t_minutes), y = plot_vol, color = maneuver, 
                        group = interaction(peak, maneuver))) +
          scale_color_brewer(palette = "Set1") +
          labs(x = "Time (min)", y = "Volume (L)") +
          theme_bw())
    })
  
  return(plot)
  
  
}

trim_end_breaths <- function(data) {
  data %>% 
    subset(peak > min(peak) & peak < max(peak))
}


zero_list_individual <- function(data) {
  data = data %>% lapply(function(x){x - min(x)})
}

zero_minutes <- function(data) {
  # For combining multiple datasets and plotting flow vs time
  # Takes data frame as input, transforms a copy of the time, and
  # returns the original data frame with updated times
  zeroed <- data %>% 
    select(peak, maneuver, plot_t_minutes) %>% 
    unnest(plot_t_minutes) %>% 
    mutate(plot_t_minutes = plot_t_minutes - min(plot_t_minutes)) %>% 
    nest(plot_t_minutes = plot_t_minutes)
  
  data$plot_t_minutes <- zeroed$plot_t_minutes
  
  return(data)
}
