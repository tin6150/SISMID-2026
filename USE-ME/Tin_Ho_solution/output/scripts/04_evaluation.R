#!/usr/bin/env Rscript

# 04_evaluation.R - Forecast Evaluation using scoringutils
# Following instructions in rules.md

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(scoringutils)
})

# Setup paths
forecast_csv <- "output/data/03_forecast/flusight_forecasts.csv"
observed_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_dir <- "output/data/04_evaluation"

# Ensure output directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Load input data
if (!file.exists(forecast_csv)) {
  stop("Forecast CSV not found: ", forecast_csv)
}
if (!file.exists(observed_csv)) {
  stop("Observed CSV not found: ", observed_csv)
}

# Load forecasts using explicit formats
fc <- read_csv(forecast_csv, col_types = cols(
  reference_date = col_date(),
  target = col_character(),
  horizon = col_integer(),
  target_end_date = col_date(),
  location = col_character(),
  output_type = col_character(),
  output_type_id = col_double(),
  value = col_double()
))

# Load observed data
obs <- read_csv(observed_csv, col_types = cols(
  week = col_date(),
  location = col_character(),
  value = col_double()
))


# 2. Join forecast and observed data and find dropped combinations
# Find how many combinations exist in the forecast data initially
fc_combos <- fc %>%
  select(reference_date, horizon, target_end_date, location) %>%
  distinct()

# Identify combinations that were dropped
dropped_combos <- anti_join(fc_combos, obs, by = c("target_end_date" = "week", "location"))
num_dropped_combos <- nrow(dropped_combos)

# Print drop notice
message("Number of (reference_date, horizon) combinations dropped for lack of observed target: ", num_dropped_combos)


# 3. Create merged data for scoring
# Merge and rename columns for scoringutils compatibility
scoring_data <- fc %>%
  inner_join(obs, by = c("target_end_date" = "week", "location")) %>%
  rename(
    observed = value.y,
    predicted = value.x,
    quantile_level = output_type_id
  ) %>%
  mutate(model = "ARIMA")


# 4. Cast to forecast_quantile and score with scoringutils
formatted_fc <- as_forecast_quantile(
  scoring_data,
  observed = "observed",
  predicted = "predicted",
  quantile_level = "quantile_level"
)

# Custom metrics list including standard metrics and custom 95% interval coverage
metrics_list <- get_metrics(formatted_fc)
metrics_list$interval_coverage_95 <- function(observed, predicted, quantile_level, ...) {
  interval_coverage(observed, predicted, quantile_level, interval_range = 95, ...)
}

# Compute scores using the custom metrics list
scores <- score(formatted_fc, metrics = metrics_list)


# 5. Build Output Tables

# Join with observed data to add truth to the forecast table
scores_with_obs <- scores %>%
  inner_join(obs, by = c("target_end_date" = "week", "location")) %>%
  rename(observed = value)

# Per-forecast scores table
forecast_scores_table <- scores_with_obs %>%
  select(
    reference_date,
    horizon,
    target_end_date,
    observed,
    WIS = wis,
    AE = ae_median,
    coverage_95 = interval_coverage_95
  ) %>%
  mutate(
    WIS = round(WIS, 1),
    AE = round(AE, 1)
  ) %>%
  arrange(reference_date, horizon)

# Summary table grouped by horizon
summary_table <- forecast_scores_table %>%
  group_by(horizon) %>%
  summarize(
    n = n(),
    WIS_mean = round(mean(WIS), 1),
    WIS_min = round(min(WIS), 1),
    WIS_max = round(max(WIS), 1),
    MAE_mean = round(mean(AE), 1),
    MAE_min = round(min(AE), 1),
    MAE_max = round(max(AE), 1),
    coverage_95_mean = round(mean(as.numeric(coverage_95)), 2),
    .groups = "drop"
  ) %>%
  arrange(horizon)


# 6. Print Tables to Console
cat("\n====================================================================\n")
cat("Per-Reference-Date Forecast Scores Table (First 10 Rows):\n")
cat("====================================================================\n")
print(head(forecast_scores_table, 10))
cat("====================================================================\n\n")

cat("====================================================================\n")
cat("Forecast Evaluation Summary (scoringutils) by Horizon:\n")
cat("====================================================================\n")
print(as.data.frame(summary_table))
cat("====================================================================\n\n")


# 7. Write to CSV Files
forecast_scores_path <- file.path(output_dir, "forecast_scores.csv")
summary_scores_path <- file.path(output_dir, "forecast_scores_by_horizon.csv")

write_csv(forecast_scores_table, forecast_scores_path)
write_csv(summary_table, summary_scores_path)

message("Evaluation script completed successfully.")
message(paste("Per-reference-date scores written to", forecast_scores_path))
message(paste("Summary scores by horizon written to", summary_scores_path))
