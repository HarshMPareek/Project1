#### Preamble ####
# Purpose: Models to see if NIA reduced crimes
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)

#### Workspace setup ####
library(tidyverse)
library(broom)
library(lubridate)

#### Load the cleaned crime data ####
crime_data <- read_csv("data/analysis_data/crime_statistics_nia_summary.csv")

# Summarize crime trends by year
crime_data$year <- ymd(paste(crime_data$year, "-01-01", sep=""))

# Summarize the data by year and crime rate for visual analysis
annual_summary <- crime_data %>%
  group_by(year) %>%
  summarise(across(contains("_RATE"), mean, na.rm = TRUE))

# Plotting trends for each crime rate
crime_types <- names(annual_summary)[2:ncol(annual_summary)]  # Skip the first column which is 'year'

plots <- lapply(crime_types, function(crime) {
  ggplot(data = annual_summary, aes_string(x = "year", y = crime)) +
    geom_line() +
    geom_point() +
    expand_limits(y = 0) +
    labs(title = paste("Trend in", gsub("_RATE", "", crime, fixed = TRUE), "Rate"),
         x = "Year",
         y = "Rate per 100,000") +
    theme_minimal()
})

# Display all plots
print(plots)