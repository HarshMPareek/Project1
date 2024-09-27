#### Preamble ####
# Purpose: Simulates data of crimes by neighbourhood
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None

# Load required libraries
library(dplyr)
library(tidyr)

# Step 1: Simulate NIA Data for 31 Neighborhoods (ensure 31 names, exclude HOOD_ID and geometry)
set.seed(123) # For reproducibility
nia_data <- data.frame(
  `_id` = 1:33,
  AREA_NAME = tolower(c(
    "beechborough-greenbrook", "black creek", "downsview", "eglinton east", "elms-old rexdale",
    "flemingdon park", "glenfield-jane heights", "golfdale-cedarbrae-woburn", "humber summit", "humbermede",
    "ionview", "keelesdale-eglinton west", "kennedy park", "kingsview village-the westway", "morningside",
    "mount dennis", "mount olive-silverstone-jamestown", "oakdale-beverley heights", "oakridge", "regent park",
    "rockcliffe-smythe", "rustic", "scarborough village", "south parkdale", "taylor-massey",
    "thistletown-beaumond heights", "thorncliffe park", "victoria village", "west hill", "weston",
    "weston-pelham park", "woburn north", "york university heights"
  ))
)

# Step 2: Simulate Neighborhood Crime Rates Data for 31 Neighborhoods from 2014 to 2023
years <- 2014:2023
crime_types <- c("ASSAULT", "AUTO_THEFT", "BIKE_THEFT", "BREAK_ENTER", "HOMICIDE", "ROBBERY", "SHOOTING", "THEFT_FROM_MV", "THEFT_OVER")

# Create wide-format crime data for simulation with a mix of values, excluding geometry and HOOD_ID
crime_data <- expand.grid(
  AREA_NAME = nia_data$AREA_NAME,
  YEAR = years,
  CRIME_TYPE = crime_types
) %>%
  mutate(CRIME_COUNT = sample(0:150, n(), replace = TRUE))

# Reshape to wide format with year-specific columns for different crimes
crime_wide <- crime_data %>%
  pivot_wider(names_from = c(CRIME_TYPE, YEAR), values_from = CRIME_COUNT)

# Step 3: Clean Data
# Convert NIA AREA_NAME to lowercase for consistent comparison
nia_data <- nia_data %>%
  mutate(AREA_NAME = tolower(AREA_NAME))

# Step 4: Join the data by AREA_NAME
merged_data <- left_join(nia_data, crime_wide, by = "AREA_NAME")

# Step 5: Reshape Crime Data to Long Format
crime_long <- merged_data %>%
  pivot_longer(
    cols = starts_with("ASSAULT_") | starts_with("AUTO_THEFT_") | starts_with("BIKE_THEFT_") |
      starts_with("BREAK_ENTER_") | starts_with("HOMICIDE_") | starts_with("ROBBERY_") |
      starts_with("SHOOTING_") | starts_with("THEFT_FROM_MV_") | starts_with("THEFT_OVER_"),
    names_to = "Crime_Type_Year", values_to = "Crime_Count"
  )

# Step 6: Summarize Crime Statistics by NIA Area
crime_summary <- crime_long %>%
  group_by(AREA_NAME) %>%
  summarize(Total_Crime_Count = sum(Crime_Count, na.rm = TRUE))

# Step 7: Save the simulated data
# Create the directory if it doesn't exist
dir.create("data/simulated_data", recursive = TRUE, showWarnings = FALSE)

# Save nia_data and crime_wide to the specified directory
write.csv(nia_data, "data/simulated_data/simulated_nia_data.csv", row.names = FALSE)
write.csv(crime_wide, "data/simulated_data/simulated_crime_data.csv", row.names = FALSE)

# Display the summarized crime statistics
print(crime_summary)
