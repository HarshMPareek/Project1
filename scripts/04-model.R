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

#### Load the cleaned crime data ####
crime_data <- read_csv("data/analysis_data/crime_statistics_nia_summary.csv")

#### 1. Visualization: Line and Bar Graph for Each Crime in Each Neighborhood ####
plot_crime_trends <- function(crime_type) {
  crime_rate_col <- paste0(crime_type, "_RATE")  # Construct rate column based on crime type
  crime_count_col <- crime_type  # The actual crime count column
  
  if (crime_rate_col %in% colnames(crime_data)) {
    crime_data %>%
      filter(!is.na(!!sym(crime_rate_col))) %>%
      ggplot(aes(x = year)) +
      geom_bar(aes(y = !!sym(crime_count_col)), stat = "identity", fill = "lightblue", alpha = 0.7) +
      geom_line(aes(y = !!sym(crime_rate_col), group = AREA_NAME, color = AREA_NAME)) +
      labs(title = paste(crime_type, "Trends by Neighborhood"),
           y = paste(crime_type, "Rate per 100,000"),
           x = "Year") +
      theme_minimal()
  } else {
    cat(paste("Column", crime_rate_col, "not found in the dataset.\n"))
  }
}

# Create plots for each crime type based on your data
plot_crime_trends("ASSAULT")
plot_crime_trends("BIKETHEFT")
plot_crime_trends("BREAKENTER")
plot_crime_trends("HOMICIDE")
plot_crime_trends("ROBBERY")
plot_crime_trends("SHOOTING")
plot_crime_trends("THEFTFROMMV")
plot_crime_trends("THEFTOVER")

#### 2. Averaging Crime Rates Across Neighborhoods ####
# Function to calculate average crime rates across neighborhoods by year
average_crime_rate <- crime_data %>%
  group_by(year) %>%
  summarise(across(starts_with("ASSAULT_RATE") | starts_with("AUTO_THEFT_RATE") | 
                     starts_with("ROBBERY_RATE") | starts_with("HOMICIDE_RATE") | 
                     starts_with("BREAKENTER_RATE"), 
                   mean, na.rm = TRUE))

# Save the averaged crime rates
write_csv(average_crime_rate, "data/analysis_data/average_crime_rate_nia.csv")

#### 3. Linear Regression on Averaged Crime Rates ####
# Function to run linear regression for a given crime type based on averaged data
run_regression_avg <- function(crime_type) {
  crime_rate_col <- paste0(crime_type, "_RATE")  # Construct rate column based on crime type
  
  # Check if the crime rate column exists in the data
  if (crime_rate_col %in% colnames(average_crime_rate)) {
    lm_fit <- lm(as.formula(paste(crime_rate_col, "~ year")), data = average_crime_rate)
    return(summary(lm_fit))
  } else {
    cat(paste("Column", crime_rate_col, "not found in the dataset.\n"))
    return(NULL)
  }
}

# Run linear regression for each crime type
assault_regression_avg <- run_regression_avg("ASSAULT")
biketheft_regression_avg <- run_regression_avg("BIKETHEFT")
breakenter_regression_avg <- run_regression_avg("BREAKENTER")
homicide_regression_avg <- run_regression_avg("HOMICIDE")
robbery_regression_avg <- run_regression_avg("ROBBERY")
shooting_regression_avg <- run_regression_avg("SHOOTING")
theftfrommv_regression_avg <- run_regression_avg("THEFTFROMMV")
theftover_regression_avg <- run_regression_avg("THEFTOVER")

# Combine all regression results
regression_avg_results <- bind_rows(
  tidy(assault_regression_avg) %>% mutate(crime_type = "Assault"),
  tidy(biketheft_regression_avg) %>% mutate(crime_type = "Bike Theft"),
  tidy(breakenter_regression_avg) %>% mutate(crime_type = "Break and Enter"),
  tidy(homicide_regression_avg) %>% mutate(crime_type = "Homicide"),
  tidy(robbery_regression_avg) %>% mutate(crime_type = "Robbery"),
  tidy(shooting_regression_avg) %>% mutate(crime_type = "Shooting"),
  tidy(theftfrommv_regression_avg) %>% mutate(crime_type = "Theft from Motor Vehicle"),
  tidy(theftover_regression_avg) %>% mutate(crime_type = "Theft Over")
)

# Save regression results
write_csv(regression_avg_results, "data/analysis_data/linear_regression_avg_results.csv")

#### Print Summary ####
print("Linear regression results for crime rate trends have been saved.")