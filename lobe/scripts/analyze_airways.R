analyze_airways <- function(
    lobe_data,
    descriptor,
    weibel_transform = -1,
    separate_end = TRUE
) {
  # Dependencies
  # - none
  
  # Assumes that the base units supplied are cm. Transform input before running
  # Assumes that the entire table from the reference is loaded (first generation to alveoli)
  # Assumes that the generation numbers supplied are provided in native format
  # weibel_transform is provided to shift the generation index as needed
  # Generation numbering is inclusive
  
  # Index of first airway
  root_airway <- min(which(lobe_data$tubes > 1)) - 1
  lobe_data <- lobe_data %>% slice(root_airway:dim(lobe_data)[1])
  
  # Transform for Weibel generation-numbering scheme
  lobe_data <- lobe_data %>% mutate(
    generation_weibel = generation + weibel_transform
  )
  
  # Separate alveolar from Yeh & Schum
  if (separate_end) {
    alveolar_data <- lobe_data %>% slice(dim(lobe_data)[1])
    lobe_data <- lobe_data %>% slice(1:dim(lobe_data)[1]-1)  
  }
  
  # Calculate properties
  lobe_data <- lobe_data %>% mutate(
    equivalent_diameter = sqrt((4*cross_section)/pi),
    airway_surface = pi*diameter*length*tubes,
    volume_cumulative = cumsum(volume),
    airway_surface_cumulative = cumsum(airway_surface),
    volume_fractional = volume/sum(volume),
    volume_fractional_cumulative = cumsum(volume_fractional),
    airway_surface_fractional = airway_surface/sum(airway_surface),
    airway_surface_fractional_cumulative = cumsum(airway_surface_fractional)
  )
  
  return(lobe_data)
    
}