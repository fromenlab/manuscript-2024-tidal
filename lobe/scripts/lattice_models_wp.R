# WP
# Derived from 20230302-lobe-shell/01-lattice-properties

ssa_dimensionless_wp <- function(dimensionless_radius) {
  a0 =  -0.02
  a1 =  133.742
  a2 = -685.869
  
  ssa_dimensionless = a0 + 
    a1*dimensionless_radius + 
    a2*dimensionless_radius^2
  
  return(ssa_dimensionless)
}

porosity_wp <- function(dimensionless_radius) {
  a0 =  0.996
  a1 =  0.292
  a2 = -70.517
  a3 = 238.040
  
  porosity = a0 + 
    a1*dimensionless_radius + 
    a2*dimensionless_radius^2 + 
    a3*dimensionless_radius^3
  
  return(porosity)
}

hydraulic_diameter_dimensionless_wp <- function(dimensionless_radius) {
  a0 = 0.016
  a1 = 0.033
  
  hydraulic_diameter_dimensionless = a0 + 
    a1 * (1/dimensionless_radius)
  
  return(hydraulic_diameter_dimensionless)
}