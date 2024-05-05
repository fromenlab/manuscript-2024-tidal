constants <- new.env(parent = emptyenv())


set_constants <- function(constants_environment) {
#var_name <- 1 # value, units

unicode_delta_upper = "\u0394"
unicode_mu_lower = "\u03BC"

# ---- Conversion factors ----
conv_inches_H2O_to_Pascal = 248.84 #Pa/inH2O
conv_feet_to_meters = 0.3048 #m/ft 1ft*(12in/1ft)*(25.4mm/1in)*(1m/1000mm)
conv_liters_to_m3 = 0.001 #m3/L



# ---- Pipe dimensions ----
# Spec values
# size_38_id_inches = 0.493 #inches 	  49035K22
# size_114_id_inches = 1.48 #inches	  49035K26
# size_2_id_inches = 2.067 #inches     49035K48
# 
# size_38_id_m_spec  = size_38_id_inches / 12 * conv_feet_to_meters # in * 1ft/12in * 0.3048m/ft
# size_114_id_m_spec = size_114_id_inches / 12 * conv_feet_to_meters # in * 1ft/12in * 0.3048m/ft
# size_2_id_m_spec   = size_2_id_inches / 12 * conv_feet_to_meters # in * 1ft/12in * 0.3048m/ft

# Measured values
size_38_id_m = 12.2 / 1000 # mm * 1m/1000mm
size_114_id_m = 34.1 / 1000 # mm * 1m/1000mm
size_2_id_m = 52.05 / 1000 # mm * 1m/1000mm

# Pipe cross sectional area
size_38_area_m2 = pi*size_38_id_m^2/4
size_114_area_m2 = pi*size_114_id_m^2/4
size_2_area_m2 = pi*size_2_id_m^2/4



# ---- Plot Constants ----
# Parity
parity_exp_lab <- expression(Delta*"P/L"["exp"]*" (Pa/m)")
parity_model_lab <- expression(Delta*"P/L"["model"]*" (Pa/m)")

# Pressure Gradient
label_pressure_gradient_pa_m <- expression(Delta*"P/L (Pa/m)")
label_superficial_velocity_ms <- expression("u (m/s)")

# Log axes
log_minor_breaks <- rep(1:9, 4)*(10^rep(0:3, each = 9))

# ---- Figure Size Constants ----

# Last updated 20210504
# https://authorservices.wiley.com/asset/photos/electronic_artwork_guidelines.pdf
# Units: inches
wiley_small_w_in <- 80 / 25.4
wiley_full_w_in <- 180 / 25.4


# Last updated 20240216
# https://www.elsevier.com/about/policies-and-standards/author/artwork-and-media-instructions/artwork-sizing
# Units: inches
elsevier_col1 = 90 / 25.4
elsevier_col15 = 140 / 25.4
elsevier_col2 = 190 / 25.4

# Last updated 20240307
# https://www.cell.com/figure-guidelines
cell_col1 = 85 / 25.4
cell_col15 = 114 / 25.4
cell_col2 = 174 / 25.4


# Save all of the variables in the constants environment
for (name in names(as.list(environment()))) {
  if (name != "constants_environment") {
  assign(name,
         get(name, envir = environment(), inherits = FALSE),
         envir = constants_environment)
  }
}

}

set_constants(constants_environment = constants)
rm("set_constants", envir = .GlobalEnv)
