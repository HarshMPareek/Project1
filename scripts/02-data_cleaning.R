#### Preamble ####
# Purpose: Only keep NIA neighbourhoods in crime data and cleans it
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Load data ####
nia_data <- read_csv("data/raw_data/neighbourhood_improvement_areas.csv")
crime_data <- read_csv("data/raw_data/neighbourhood_crime_rates.csv")

#### Filter Crime Data for NIA Areas by AREA_NAME ####
nia_areas <- nia_data %>%
  select(AREA_NAME) %>%
  mutate(AREA_NAME = tolower(AREA_NAME)) # Standardize NIA area names to lowercase

# Join the crime data with the NIA areas based on AREA_NAME
crime_in_nia <- crime_data %>%
  mutate(AREA_NAME = tolower(AREA_NAME)) %>% # Standardize crime data area names to lowercase
  filter(AREA_NAME %in% nia_areas$AREA_NAME)

#### Reshape Crime Data to Long Format ####
# Reshape the crime columns (ASSAULT, AUTO_THEFT, ROBBERY, HOMICIDE, BREAKENTER, etc.) from wide to long format
crime_long <- crime_in_nia %>%
  pivot_longer(
    cols = starts_with("ASSAULT_") | starts_with("AUTO_THEFT_") | starts_with("ROBBERY_") |
      starts_with("HOMICIDE_") | starts_with("BREAKENTER_") | starts_with("BIKETHEFT_") |
      starts_with("SHOOTING_") | starts_with("THEFTFROMMV_") | starts_with("THEFTOVER_"),
    names_to = c("crime_type", "year"),
    names_pattern = "(.*)_(\\d{4})",
    values_to = "count"
  ) %>%
  mutate(year = as.numeric(year)) # Convert year to numeric

#### Summarize Crime Statistics for NIA Areas ####
crime_summary <- crime_long %>%
  group_by(AREA_NAME, year, crime_type) %>%
  summarise(total_count = sum(count, na.rm = TRUE)) %>% # Summarize total counts for each crime type per year
  pivot_wider(names_from = crime_type, values_from = total_count) # Convert back to wide format

#### Save Summarized Data ####
write_csv(crime_summary, "data/analysis_data/crime_statistics_nia_summary.csv")

cat("Summarized crime statistics for NIA areas (including homicides, break and enters, etc.) have been saved to 'data/analysis_data/crime_statistics_nia_summary.csv'.\n")
