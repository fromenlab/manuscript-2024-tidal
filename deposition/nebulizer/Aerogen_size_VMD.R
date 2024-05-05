
library(tidyverse)

########Data loading
#low VMD 
counts <- c(31626,	12704,	26771,	42798,	48574,	48396,	34186,	35092,	34994,	46674,	71137,	45545)



############# 
lower_bounds <- c(0.3,
                  0.6,
                  1,
                  1.5,
                  2,
                  2.5,
                  3,
                  3.525,
                  4.168,
                  5,
                  6,
                  8
)
upper_bounds <- c(0.6,
                  1,
                  1.5,
                  2,
                  2.5,
                  3,
                  3.525,
                  4.168,
                  5,
                  6,
                  8,
                  10
)

# Assume lower_bounds and upper_bounds are vectors containing the bounds of the bins
# Assume counts is a vector containing the particle counts for each bin

# Calculate midpoints diameter of each bin
midpoints <- (lower_bounds + upper_bounds) / 2

# Calculate the volume of particles in each bin
volumes <- counts * (4/3) * pi * (midpoints / 2)^3

# Calculate cumulative volume
cumulative_volume <- cumsum(volumes)

# Total volume
total_volume <- sum(volumes)

# Find the bin where cumulative volume exceeds 50% of total volume
vmd_bin <- which(cumulative_volume >= total_volume / 2)[1]

# Interpolate to find the exact VMD, if necessary
# (More complex interpolation methods can be used depending on accuracy requirements)
if (vmd_bin > 1) {
  vmd <- midpoints[vmd_bin - 1] + 
    (midpoints[vmd_bin] - midpoints[vmd_bin - 1]) * 
    ((total_volume / 2 - cumulative_volume[vmd_bin - 1]) / 
       (cumulative_volume[vmd_bin] - cumulative_volume[vmd_bin - 1]))
} else {
  vmd <- midpoints[vmd_bin]
}


###############MMAD

density <- 1  # Density of the particles in g/cm³

# if non-uniform, input an array for densities
#density <- c(1,2,3,4,5,6,7,8,9,10,11)

# Calculate the mass of particles in each bin
masses <- counts * (4/3) * pi * ((midpoints / 2) * 1e-4)^3 * density

# Calculate cumulative mass
cumulative_mass <- cumsum(masses)

# Total mass
total_mass <- sum(masses)

# Find the bin where cumulative mass exceeds 50% of total mass
mmad_bin <- which(cumulative_mass >= total_mass / 2)[1]

# Interpolate to find the exact MMAD
if (mmad_bin > 1) {
  mmad <- midpoints[mmad_bin - 1] + 
    (midpoints[mmad_bin] - midpoints[mmad_bin - 1]) * 
    ((total_mass / 2 - cumulative_mass[mmad_bin - 1]) / 
       (cumulative_mass[mmad_bin] - cumulative_mass[mmad_bin - 1]))
} else {
  mmad <- midpoints[mmad_bin]
}




# Creating a dataframe for ggplot
df <- tibble(
  Bin = 1:length(counts),
  LowerBound = lower_bounds,
  UpperBound = upper_bounds,
  Midpoint = midpoints,
  Count = counts,
  CumulativeCount = cumsum(counts)
)

# Total count for normalization
total_count <- sum(counts)

# Convert Cumulative Count to percentage
df$CumulativePercent <- (df$CumulativeCount / total_count) * 100

# Plotting Particle Count Distribution
ggplot(df, aes(x = Bin, y = Count, label = Count)) +
  geom_col() +
  geom_text(vjust = -0.3) +
  labs(title = "Particle Count Distribution - Low VMD", x = "Bin (LowerBound - UpperBound)", y = "Count") +
  scale_x_continuous(breaks = df$Bin, labels = paste(df$LowerBound, df$UpperBound, sep = " - ")) +
  theme_minimal()

# Plotting Cumulative Particle Distribution as a Percentage
ggplot(df, aes(x = Bin, y = CumulativePercent, label = sprintf("%.1f%%", CumulativePercent))) +
  geom_col() +
  geom_text(vjust = -0.3) +
  labs(title = "Cumulative Particle Distribution - Low VMD", x = "Bin (LowerBound - UpperBound)", y = "Cumulative Percent") +
  scale_x_continuous(breaks = df$Bin, labels = paste(df$LowerBound, df$UpperBound, sep = " - ")) +
  theme_minimal()


# Print the VMD
print(vmd)
# Print the MMAD
print(mmad)


# IRW
ggplot(df, aes(x = Bin, y = Count, label = Count)) +
  geom_col() +
  # geom_text(vjust = -0.5, hjust = -1, angle = 45) +
  labs(title = "Particle Count Distribution - Low VMD", x = expression("Bin (LowerBound - UpperBound, ("*mu*"m)"), y = "Count") +
  scale_x_continuous(breaks = df$Bin, labels = paste(df$LowerBound, df$UpperBound, sep = " - ")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(here::here("deposition/nebulizer/nebulizer-counts.png"), 
       dpi = 500, width = 5, height = 5,
       units = "in")

# Plotting Cumulative Particle Distribution as a Percentage
ggplot(df, aes(x = Bin, y = CumulativePercent, label = sprintf("%.1f%%", CumulativePercent))) +
  geom_col() +
  geom_text(vjust = -0.3) +
  labs(title = "Cumulative Particle Distribution - Low VMD", x = expression("Bin (LowerBound - UpperBound, ("*mu*"m)"), y = "Cumulative Percent") +
  scale_x_continuous(breaks = df$Bin, labels = paste(df$LowerBound, df$UpperBound, sep = " - ")) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(here::here("deposition/nebulizer/nebulizer-cumulative.png"), 
       dpi = 500, width = 5, height = 5,
       units = "in")
