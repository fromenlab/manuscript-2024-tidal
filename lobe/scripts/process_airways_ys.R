process_airways_ys <- function(
    lobe_data,
    lobe_position,
    approximation_start = NULL,
    terminal_bronchioles_start,
    extrusion_offset,
    peripheral_start,
    weibel_transform = -1,
    output_location
) {
  # Dependencies
  # - lattice_models_wp.R
  
  # Assumes that the base units supplied are cm, from the original Y&S 1980
  # Assumes that the entire table from the reference is loaded (first generation to alveoli)
  # Assumes that the generation numbers supplied are provided in the native format,
  # without correction to Weibel order
  # Calculates cumulative volume, rather than taking from the table
  # Generation numbering is inclusive
  
  # Index of lobar bronchus
  lobe_start <- min(which(lobe_data$tubes > 1))
  
  if (is.null(approximation_start)) {
    approximation_start = lobe_start
  }
  
  # Convert units to millimeter basis
  lobe_data <- lobe_data %>% mutate(
    length = length * 10, # cm * 10 mm/cm
    diameter = diameter * 10, # cm * 10 mm/cm
    cross_section = cross_section * 10^2, # cm2 * (10 mm/cm)^2
    volume = volume * (10)^3, # cm3 * (10 mm/cm)^3
    cumulative_volume = cumsum(volume)
  )
  
  # Calculated morphometric quantities
  # 20230323 - commented out; these are not used
  # lobe_data <- lobe_data %>% mutate(
  #   cross_section_calc = pi*diameter^2/4*tubes,
  #   volume_calc = cross_section_calc*length
  # )
  
  # Transform for Weibel generation-numbering scheme
  lobe_data <- lobe_data %>% mutate(
    generation_weibel = generation + weibel_transform
  )
  
  # Separate alveolar from Yeh & Schum
  alveolar_data <- lobe_data %>% slice(dim(lobe_data)[1])
  lobe_data <- lobe_data %>% slice(1:dim(lobe_data)[1]-1)
  
  # Calculate equivalent diameter
  lobe_data <- lobe_data %>% mutate(
    equivalent_diameter = sqrt((4*cross_section)/pi),
    equivalent_diameter_corrected = equivalent_diameter # set up for the following step
  )
  
  # Correct any decreasing equivalent diameters to allow for lattice removal
  for (i in (lobe_start+1):(length(lobe_data$equivalent_diameter))) {
    d = lobe_data$equivalent_diameter[i]
    d_previous = lobe_data$equivalent_diameter_corrected[i-1]
    if (d < d_previous) {
      lobe_data$equivalent_diameter_corrected[i] = d_previous
    }
  }
  
  # Define Weibel-based numbering
  w_approx_start <- approximation_start + weibel_transform
  w_peripheral_start <- peripheral_start + weibel_transform
  w_terminal_bronch_start <- terminal_bronchioles_start + weibel_transform
  w_extrusion_start <- terminal_bronchioles_start + weibel_transform + extrusion_offset
  
  # Calculate lattice properties
  unit_cell = "Weaire-Phelan"
  # Static declaration
  dimensionless_radius = 0.09
  
  # Lattice region 1 - central
  lattice_region_1 <- tibble(
    startGeneration = w_approx_start,
    endGeneration = w_peripheral_start - 1 # inclusive
  )
  
  lattice_region_1_data <- subset(lobe_data, c(generation_weibel >= lattice_region_1$startGeneration & 
                                                 generation_weibel <= lattice_region_1$endGeneration))
  
  # Update lattice region description
  lattice_region_1 <- lattice_region_1 %>% mutate(
    hydraulicDiameter = mean(lattice_region_1_data$diameter),
    unitCell = unit_cell,
    dimensionlessRadius = dimensionless_radius,
    # dh/lc = dh_dimensionless; lc = dh/dh_dimensionless
    cellDimension = hydraulicDiameter/hydraulic_diameter_dimensionless_wp(dimensionless_radius),
    porosity = porosity_wp(dimensionless_radius)
  )
  
  # Lattice region 2 - peripheral
  lattice_region_2 <- tibble(
    startGeneration = w_peripheral_start,  # inclusive
    endGeneration = lobe_data$generation_weibel %>% tail(1)
  )
  
  lattice_region_2_data <- subset(lobe_data, c(generation_weibel >= lattice_region_2$startGeneration))
  
  # Update lattice region description
  lattice_region_2 <- lattice_region_2 %>% mutate(
    hydraulicDiameter = mean(lattice_region_2_data$diameter),
    unitCell = unit_cell,
    dimensionlessRadius = dimensionless_radius,
    # dh/lc = dh_dimensionless; lc = dh/dh_dimensionless
    cellDimension = hydraulicDiameter/hydraulic_diameter_dimensionless_wp(dimensionless_radius),
    porosity = porosity_wp(dimensionless_radius)
  )
  
  # Prepare lobe and airway data for JSON format
  lobe_properties <- tibble(
    lobePosition = lobe_position,
    generationNumbering = "Weibel",
    approximationStartGeneration = w_approx_start,
    peripheralStartGeneration = w_peripheral_start,
    terminalBronchiolesStartGeneration = w_terminal_bronch_start,
    extrusionStartOffset = extrusion_offset,
    
    lobeVolume = alveolar_data$cumulative_volume,
    alveolarVolume = alveolar_data$volume
  )
  
  airway_properties <- lobe_data %>% select(
    generation = generation_weibel,
    length = length,
    equivalentDiameter = equivalent_diameter_corrected,
    generationVolume = volume
  )
  
  # Construct JSON output
  json_data <- list(
    baseUnits = unbox("mm"),
    lobeProperties = unbox(lobe_properties),
    airwayProperties = airway_properties,
    latticeProperties = bind_rows(lattice_region_1, lattice_region_2))
  
  # Write output to file when provided a path
  if (!missing(output_location)) {
    json_data %>% 
      toJSON(pretty = TRUE) %>% 
      write_lines(file = here::here(output_location))  
  }
  
  return(json_data)
    
}