cip_frac <- readr::read_csv(here::here("deposition/cp-transform/res/map.csv"))

cip_transform <- cip_frac %>% select(-G) %>% t()

# Split I equally between c, p
cp_frac <- cip_frac %>% 
  mutate(
    c = C+I/2,
    p = P+I/2,
    s = c+p
  )


# Average generation regions used in TIDAL algorithm
# 
# C: G3-G8
# P: G9+
  
# Convention: trueRegion_tidalRegion

g_start = 3
g_end = 8

# Get the fraction of regional deposition from the c region in TIDAL
cp_frac_ctidal <- cp_frac %>% 
  slice(g_start:g_end) %>% 
  select(c,p) %>% 
  summarize(across(.cols = c(c,p), mean, .names = "{.col}"))

# Get the fraction of regional deposition from the p region in TIDAL
cp_frac_ptidal <- cp_frac %>% 
  slice(g_end+1:n()) %>% 
  select(c,p) %>% 
  summarize(across(.cols = c(c,p), mean, .names = "{.col}"))

cp_transform <- bind_rows(
  cp_frac_ctidal,
  cp_frac_ptidal
) %>% 
  t() 

# Confirm that fractions add to 1
# cp_transform[,1] %>% sum()
# cp_transform[,2] %>% sum()

# Check condition number
# cp_transform %>% kappa()

####
# Function definition
####

convert_to_planar <- function(cp_tidal, p_basis = 1) {
  c_tidal <- cp_tidal * p_basis
  p_tidal <- p_basis
  
  cp_planar <- cp_transform %*% matrix(c(c_tidal, p_tidal)) %>% 
    (function(x) {x[1]/x[2]})
  
  return(cp_planar)
}