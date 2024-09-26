#### Preamble ####
# Purpose: Models to see if NIA reduced crimes
# Author: Harsh M Pareek
# Date: 24 September 2024
# Contact: harsh.pareek@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace Setup ####
library(tidyverse)
library(lubridate)
library(broom)

#### Load the Crime Data ####
# Replace the file path with your actual data file path
crime_data <- read_csv("data/analysis_data/crime_statistics_nia_summary.csv")

#### Data Preparation ####
# Convert 'year' to numeric if not already
crime_data$year <- as.numeric(crime_data$year)

# View column names
print("Column names:")
print(names(crime_data))

# Reshape data to long format
crime_long <- crime_data %>%
  pivot_longer(
    cols = -c(AREA_NAME, year),
    names_to = "crime_metric",
    values_to = "value"
  )

# Separate 'crime_metric' into 'crime_type' and 'metric' (e.g., 'ASSAULT' and 'RATE')
crime_long <- crime_long %>%
  mutate(
    metric = if_else(str_detect(crime_metric, "_RATE$"), "RATE", "COUNT"),
    crime_type = str_replace(crime_metric, "_RATE$", "")
  )

# Verify unique metrics
print("Unique metrics:")
print(unique(crime_long$metric))

# Filter to keep only RATE metrics
crime_rates <- crime_long %>%
  filter(metric == "RATE") %>%
  select(AREA_NAME, year, crime_type, value)

# Aggregate data across all NIA neighborhoods to get average crime rates per year
annual_crime_rates <- crime_rates %>%
  group_by(year, crime_type) %>%
  summarize(avg_rate = mean(value, na.rm = TRUE), .groups = "drop")

#### Time Series Analysis ####
# Count the number of observations per crime type
obs_count <- annual_crime_rates %>%
  group_by(crime_type) %>%
  summarize(n = n(), .groups = "drop")

print("Observations per crime type:")
print(obs_count)

# Filter out crime types with less than 3 observations (minimum needed for a trend)
sufficient_data_crime_types <- obs_count %>%
  filter(n >= 3) %>%
  pull(crime_type)

# Filter the annual_crime_rates data to include only these crime types
annual_crime_rates_filtered <- annual_crime_rates %>%
  filter(crime_type %in% sufficient_data_crime_types)

# Prepare time series data for each crime type
crime_ts_list <- annual_crime_rates_filtered %>%
  group_by(crime_type) %>%
  arrange(year) %>%
  summarize(
    data = list(data.frame(year = year, avg_rate = avg_rate)),
    .groups = "drop"
  )

# Function to perform linear trend analysis and plot
analyze_time_series <- function(data, crime_type) {
  df <- data

  # Check if there are at least three data points
  if (nrow(df) >= 3) {
    # Fit a linear model to the time series
    lm_model <- lm(avg_rate ~ year, data = df)
    lm_summary <- summary(lm_model)

    # Plot the time series and fitted trend line
    trend_plot <- ggplot(df, aes(x = year, y = avg_rate)) +
      geom_point(color = "blue") +
      geom_line(color = "blue") +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        title = paste("Trend in", crime_type, "Rate"),
        x = "Year",
        y = "Average Crime Rate per 100,000"
      ) +
      theme_minimal()

    # Ensure the plots directory exists
    if (!dir.exists("other/plots")) {
      dir.create("other/plots", recursive = TRUE)
    }

    # Save the plot
    ggsave(
      filename = paste0("other/plots/trend_", crime_type, ".png"),
      plot = trend_plot,
      width = 8,
      height = 6,
      dpi = 300
    )

    # Return the results
    return(list(
      crime_type = crime_type,
      lm_model = lm_model,
      lm_summary = lm_summary
    ))
  } else {
    warning(paste("Not enough data to fit a model for", crime_type))
    return(NULL)
  }
}

# Apply the function to each crime type
ts_results <- crime_ts_list %>%
  mutate(
    results = map2(data, crime_type, analyze_time_series)
  )

#### Extracting and Saving Model Summaries ####
# Filter out NULL results
ts_results_filtered <- ts_results %>%
  filter(!map_lgl(results, is.null))

# Extract coefficients and p-values from linear models
model_summaries <- ts_results_filtered %>%
  mutate(
    intercept = map_dbl(results, ~ coef(.x$lm_model)[1]),
    slope = map_dbl(results, ~ coef(.x$lm_model)[2]),
    p_value = map_dbl(results, ~ summary(.x$lm_model)$coefficients[2, 4]),
    r_squared = map_dbl(results, ~ summary(.x$lm_model)$r.squared)
  ) %>%
  select(crime_type, intercept, slope, p_value, r_squared)

# Ensure the datasheet directory exists
if (!dir.exists("other/datasheet")) {
  dir.create("other/datasheet", recursive = TRUE)
}

# Save model summaries to CSV
write_csv(model_summaries, "other/datasheet/time_series_model_summaries.csv")

#### Visualization ####
# If you want to create a combined plot for all crime types
combined_plot <- annual_crime_rates_filtered %>%
  ggplot(aes(x = year, y = avg_rate)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  facet_wrap(~crime_type, scales = "free_y") +
  labs(
    title = "Crime Rates Over Time in NIA Neighborhoods",
    x = "Year",
    y = "Average Crime Rate per 100,000"
  ) +
  theme_minimal()

# Save the combined plot
ggsave("other/plots/combined_crime_rates_over_time.png", plot = combined_plot, width = 12, height = 8, dpi = 300)

#### Interpretation ####
# You can interpret the slope and p-value in the model_summaries.csv file
# - A positive slope indicates an increasing trend over the years
# - A negative slope indicates a decreasing trend
# - A p-value less than 0.05 suggests the trend is statistically significant
