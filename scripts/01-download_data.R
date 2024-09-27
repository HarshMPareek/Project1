#### Preamble ####
# Purpose: Downloads and saves the data from open data toronto
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)

#### Download data ####

# Download Neighbourhood Improvement Areas data (Code from Open data Toronto)
nia_package <- show_package("3b471f62-dc01-4a96-bb76-f794e4c6b860")
nia_resources <- list_package_resources("3b471f62-dc01-4a96-bb76-f794e4c6b860")
nia_datastore_resources <- filter(nia_resources, tolower(format) %in% c("csv", "geojson"))
nia_data <- filter(nia_datastore_resources, row_number() == 1) %>% get_resource()

# Download Neighbourhood Crime Rates data (Code from Open data Toronto)
crime_package <- show_package("neighbourhood-crime-rates")
crime_resources <- list_package_resources("neighbourhood-crime-rates")
crime_datastore_resources <- filter(crime_resources, tolower(format) %in% c("csv", "geojson"))
crime_data <- filter(crime_datastore_resources, row_number() == 1) %>% get_resource()

#### Save data ####

# Save Neighbourhood Improvement Areas data
write_csv(nia_data, "data/raw_data/neighbourhood_improvement_areas.csv")

# Save Neighbourhood Crime Rates data
write_csv(crime_data, "data/raw_data/neighbourhood_crime_rates.csv")

cat("Neighbourhood Improvement Areas and Crime Rates data have been saved to 'data/raw_data/'.\n")
