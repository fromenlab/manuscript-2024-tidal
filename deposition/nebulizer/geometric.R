# IRW

data <- tibble(lower_bounds, upper_bounds, counts) %>% 
  mutate(midpoint = lower_bounds + upper_bounds / 2, 
         frac = counts/sum(counts),
         pct = counts/sum(counts) * 100, 
         pct_scaled = pct * 7, 
         dn = midpoint ^ pct, 
         dn_scaled = midpoint ^ pct_scaled,
         nlnD = counts * log(midpoint))


# Confirm scaling applies

# Basis of 1
data$midpoint^data$frac %>% prod() %>% .^(1 / data$frac %>% sum())

# Basis of 100
data$dn %>% prod() %>% .^(1 / data$pct %>% sum())

# Basis of 100 * factor
data$dn_scaled %>% prod() %>% .^ (1 / data$frac %>% sum())

# Check against manual calc
total_count = data$counts %>% sum()

(data$nlnD %>% sum() / total_count) %>% exp()

# Use manual calc
lndg = data$nlnD %>% sum() / total_count

dg = exp(lndg)

lngsd = data %>% mutate(
  gsd_term = counts * (log(midpoint) - lndg) ^ 2
) %>% 
  (function(data) {
    sqrt(
      1 / total_count * data$gsd_term %>% sum()
    )
  })

gsd = exp(lngsd)
