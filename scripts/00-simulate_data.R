#### Preamble ####
# Purpose: Simulates data of crimes by neighbourhood
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####

# Simulate Neighbourhood Improvement Areas data
nia_simulated <- tibble(
  AREA_ID = 1:100,  # Simulate 100 area IDs
  AREA_NAME = paste("Neighbourhood", 1:100),  # Create names as Neighbourhood 1 to 100
  POPULATION = round(runif(100, 1000, 5000)),  # Random population between 1000 and 5000
  FEATURE_CODE_DESC = sample(c("Residential", "Commercial", "Industrial"), 100, replace = TRUE)
)

# Simulate Neighbourhood Crime Rates data
crime_simulated <- tibble(
  HOOD_ID = 1:100,
  YEAR = rep(2014:2023, each = 10),
  ASSAULTS = round(rnorm(100, mean = 150, sd = 30)),  # Normally distributed, mean 150, sd 30
  AUTO_THEFTS = round(rnorm(100, mean = 50, sd = 10)),  # Normally distributed, mean 50, sd 10
  ROBBERIES = round(rnorm(100, mean = 30, sd = 5))  # Normally distributed, mean 30, sd 5
)

#### Save simulated data ####

# Save simulated Neighbourhood Improvement Areas data
write_csv(nia_simulated, "data/simulated_data/neighbourhood_improvement_areas_simulated.csv")

# Save simulated Neighbourhood Crime Rates data
write_csv(crime_simulated, "data/simulated_data/neighbourhood_crime_rates_simulated.csv")

cat("Simulated data for Neighbourhood Improvement Areas and Crime Rates have been saved to 'data/simulated_data/'.\n")



