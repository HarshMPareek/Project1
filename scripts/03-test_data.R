#### Preamble ####
# Purpose: Tests if NIA neighborhoods are included and negative or missing values
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: Harsh M Pareek
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### Test data ####

#### Load cleaned data ####
crime_cleaned <- read_csv("data/analysis_data/crime_statistics_nia.csv")

#### Test 1: Check that only NIA neighborhoods are included ####
nia_data <- read_csv("data/raw_data/neighbourhood_improvement_areas.csv")
nia_areas <- nia_data %>% pull(AREA_NAME)

non_nia_areas <- crime_cleaned %>%
  filter(!AREA_NAME %in% nia_areas) %>%
  distinct(AREA_NAME)

if (nrow(non_nia_areas) == 0) {
  cat("Test 1 Passed: All areas in the crime data are in NIA areas.\n")
} else {
  cat("Test 1 Failed: Non-NIA areas found in the crime data.\n")
  print(non_nia_areas)
}

#### Test 2: Check for negative values in crime counts ####
# Check only count columns (not rate columns)
negative_values <- crime_cleaned %>%
  filter(if_any(contains("ASSAULT") | contains("AUTO_THEFT") | contains("ROBBERY"), ~ . < 0))

if (nrow(negative_values) == 0) {
  cat("Test 2 Passed: No negative crime counts found.\n")
} else {
  cat("Test 2 Failed: Negative crime counts found.\n")
  print(negative_values)
}

#### Test 3: Summary statistics for key crime types ####
# Dynamically select all columns related to crime counts (excluding rate columns)
crime_summary_stats <- crime_cleaned %>%
  summarise(
    total_assaults = sum(across(starts_with("ASSAULT_"), ~ sum(.x, na.rm = TRUE))),
    total_auto_thefts = sum(across(starts_with("AUTO_THEFT_"), ~ sum(.x, na.rm = TRUE))),
    total_robberies = sum(across(starts_with("ROBBERY_"), ~ sum(.x, na.rm = TRUE)))
  )

cat("Summary statistics:\n")
print(crime_summary_stats)

#### Test 4: Check for missing values ####
missing_values <- crime_cleaned %>%
  summarise(across(everything(), ~ sum(is.na(.))))

if (all(missing_values == 0)) {
  cat("Test 4 Passed: No missing values in the cleaned data.\n")
} else {
  cat("Test 4 Failed: Missing values found.\n")
  print(missing_values)
}
