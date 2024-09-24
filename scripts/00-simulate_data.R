#### Preamble ####
# Purpose: Simulates... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
# [...UPDATE THIS...]

#### Simulate data ####
# [...ADD CODE HERE...]
# scripts/00-simulate_data.R

# Author: [Your Name]
# Date: [Current Date]
# Purpose: Simulate data for testing analysis pipeline

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(sf)

# Simulate Income Data
set.seed(123)
sim_income_data <- data.frame(
  Neighbourhood = paste("Neighbourhood", 1:140),
  median_income = rnorm(140, mean = 60000, sd = 15000)
)

# Simulate Parks Data
set.seed(456)
sim_parks_data <- data.frame(
  PARK_ID = 1:500,
  LATITUDE = runif(500, min = 43.6, max = 43.85),
  LONGITUDE = runif(500, min = -79.5, max = -79.2)
)

# Simulate Neighbourhood Boundaries (Simplified)
sim_neigh_data <- st_make_grid(
  st_bbox(c(xmin = -79.5, xmax = -79.2, ymin = 43.6, ymax = 43.85)),
  n = c(14, 10)
)
sim_neigh_data <- st_sf(
  Neighbourhood = paste("Neighbourhood", 1:length(sim_neigh_data)),
  geometry = sim_neigh_data
)

# Create directories if they don't exist
if (!dir.exists("data/simulated_data")) {
  dir.create("data/simulated_data", recursive = TRUE)
}

# Save Simulated Data
write.csv(sim_income_data, "data/simulated_data/sim_income_data.csv", row.names = FALSE)
write.csv(sim_parks_data, "data/simulated_data/sim_parks_data.csv", row.names = FALSE)
st_write(sim_neigh_data, "data/simulated_data/sim_neigh_data.geojson", delete_dsn = TRUE)

# Basic Tests on Simulated Data
# Test income data mean and standard deviation
income_mean <- mean(sim_income_data$median_income)
income_sd <- sd(sim_income_data$median_income)

print(paste("Simulated Income Data - Mean:", round(income_mean, 2)))
print(paste("Simulated Income Data - SD:", round(income_sd, 2)))

# Plot histogram of simulated income data
ggplot(sim_income_data, aes(x = median_income)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Simulated Median Income",
       x = "Median Household Income",
       y = "Frequency") +
  theme_minimal()
