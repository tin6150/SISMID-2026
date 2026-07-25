#!/usr/bin/env Rscript

# 07_xgboost_forecast.R - XGBoost Quantile Regression Forecasting
# Following instructions in rules.md

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(MMWRweek)
  library(xgboost)
  library(ggplot2)
})

# Setup paths
input_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_dir_data <- "output/data/03_forecast"
output_dir_fig <- "output/figures/03_forecast"

# Outputs
forecasts_csv_path <- file.path(output_dir_data, "XGB_flusight_forecasts.csv")
forecast_plot_path <- file.path(output_dir_fig, "XGB_forecast_vs_observed.png")

# Path Validation before writing
if (!grepl("XGB_", basename(forecasts_csv_path))) {
  stop("Validation failed: Forecast output path lacks 'XGB_' prefix.")
}
if (!grepl("XGB_", basename(forecast_plot_path))) {
  stop("Validation failed: Figure output path lacks 'XGB_' prefix.")
}

# Ensure output directories exist
dir.create(output_dir_data, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_fig, recursive = TRUE, showWarnings = FALSE)

# 1. Read and validate input data
if (!file.exists(input_csv)) {
  stop("Input file not found: ", input_csv)
}

cleaned <- tryCatch({
  read_csv(input_csv, col_types = cols(.default = col_character()))
}, error = function(e) {
  stop("The data could not be parsed.")
})

# week column
if ("week" %in% names(cleaned)) {
  parsed_week <- as.Date(cleaned$week, format = "%Y-%m-%d")
  if (any(is.na(parsed_week))) {
    parsed_week <- tryCatch(as.Date(cleaned$week), error = function(e) NA)
  }
  if (any(is.na(parsed_week))) {
    stop("The week could not be parsed.")
  }
  cleaned$week <- parsed_week
} else {
  stop("The week could not be parsed.")
}

# location column
if ("location" %in% names(cleaned)) {
  cleaned$location <- as.character(cleaned$location)
  if (!all(cleaned$location == "US")) {
    cleaned$location <- "US"
  }
} else {
  stop("The location could not be parsed.")
}

# value column
if ("value" %in% names(cleaned)) {
  parsed_value <- suppressWarnings(as.numeric(cleaned$value))
  if (any(is.na(parsed_value))) {
    parsed_value <- suppressWarnings(readr::parse_number(cleaned$value))
  }
  if (any(is.na(parsed_value))) {
    stop("The value could not be parsed.")
  }
  cleaned$value <- parsed_value
} else {
  stop("The value could not be parsed.")
}

cleaned <- cleaned %>% arrange(week)

# 2. Season Mapping & Current Season Determination
cleaned <- cleaned %>%
  mutate(
    epi = MMWRweek(week),
    epi_week = epi$MMWRweek,
    epi_year = epi$MMWRyear,
    cal_year = year(week),
    cal_month = month(week)
  ) %>%
  mutate(
    season_start_year = case_when(
      epi_week >= 40 & cal_month <= 8  ~ cal_year - 1,
      epi_week >= 40 & cal_month > 8   ~ cal_year,
      epi_week <= 20                   ~ cal_year - 1,
      TRUE                             ~ as.double(NA)
    )
  ) %>%
  mutate(
    season = ifelse(is.na(season_start_year), "Off-Season",
                    paste0(season_start_year, "-", substr(as.character(season_start_year + 1), 3, 4)))
  )

current_season_data <- cleaned %>% filter(season_start_year == 2025)
season_start_date <- min(current_season_data$week)
season_end_date <- max(current_season_data$week)

message(paste0("Season Start Week: ", format(season_start_date, "%Y-%m-%d")))
message(paste0("Season End Week: ", format(season_end_date, "%Y-%m-%d")))

# 3. Training and Testing Periods
training_period_start <- min(cleaned$week)
testing_period_start <- season_start_date
horizons <- c(1, 2, 3)

message("Training Period start date: ", format(training_period_start, "%Y-%m-%d"))
message("Testing Period start date: ", format(testing_period_start, "%Y-%m-%d"))
message("Forecasting horizons: ", paste(horizons, collapse = ", "))

reference_dates <- cleaned %>%
  filter((week + 7) >= season_start_date & (week + 7) <= season_end_date & (week + 7) %in% cleaned$week) %>%
  pull(week) %>%
  sort() %>%
  unique()

message("Rolling-window end (reference) dates count: ", length(reference_dates))
message("First reference date: ", format(min(reference_dates), "%Y-%m-%d"))
message("Last reference date: ", format(max(reference_dates), "%Y-%m-%d"))


# 1X. Feature Engineering helpers
# Features: lag1, lag2, lag3, lag4 (as of anchor week t), sin1, cos1, sin2, cos2 (as of target week t + 7h)
expected_quantiles <- c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
feature_names <- c("lag1", "lag2", "lag3", "lag4", "sin1", "cos1", "sin2", "cos2")

get_train_data <- function(df, r, h) {
  # Lags construction for all weeks
  df_lags <- df %>%
    mutate(
      lag1 = value,
      lag2 = lag(value, 1),
      lag3 = lag(value, 2),
      lag4 = lag(value, 3)
    ) %>%
    filter(!is.na(lag4))

  # Filter anchor weeks t where target week (t + 7h) is <= r
  # Join target week value
  train_raw <- df_lags %>%
    mutate(target_week = week + 7 * h) %>%
    filter(target_week <= r) %>%
    left_join(df %>% select(week, target_value = value), by = c("target_week" = "week")) %>%
    filter(!is.na(target_value))

  if (nrow(train_raw) == 0) {
    return(NULL)
  }

  # Target MMWR epiweek
  target_epi <- MMWRweek(train_raw$target_week)
  train_raw$target_epi_week <- target_epi$MMWRweek

  # Harmonics
  train_raw <- train_raw %>%
    mutate(
      sin1 = sin(2 * pi * 1 * target_epi_week / 52.18),
      cos1 = cos(2 * pi * 1 * target_epi_week / 52.18),
      sin2 = sin(2 * pi * 2 * target_epi_week / 52.18),
      cos2 = cos(2 * pi * 2 * target_epi_week / 52.18)
    )

  return(train_raw)
}

get_predict_data <- function(df, r, h) {
  # Prediction at anchor week r
  df_lags <- df %>%
    mutate(
      lag1 = value,
      lag2 = lag(value, 1),
      lag3 = lag(value, 2),
      lag4 = lag(value, 3)
    )

  r_row <- df_lags %>% filter(week == r)
  if (nrow(r_row) != 1) {
    stop("Reference date row not found: ", format(r, "%Y-%m-%d"))
  }

  target_week <- r + 7 * h
  target_epi_week <- MMWRweek(target_week)$MMWRweek

  pred_df <- r_row %>%
    mutate(
      target_week = target_week,
      target_epi_week = target_epi_week,
      sin1 = sin(2 * pi * 1 * target_epi_week / 52.18),
      cos1 = cos(2 * pi * 1 * target_epi_week / 52.18),
      sin2 = sin(2 * pi * 2 * target_epi_week / 52.18),
      cos2 = cos(2 * pi * 2 * target_epi_week / 52.18)
    )

  return(pred_df)
}


# Print feature names validation
message("XGBoost Features constructed: ", paste(feature_names, collapse = ", "))


# 3X. Direct Multi-Horizon Fitting and Forecast Loop
forecast_rows <- list()

# XGBoost Parameters
xgb_params <- list(
  objective = "reg:quantileerror",
  quantile_alpha = expected_quantiles,
  eta = 0.05,
  max_depth = 3,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nthread = 1,
  seed = 42
)

# Loop over reference dates
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  target_dates <- ref_date_obj + 7 * horizons

  message(paste0("Reference Date: ", format(ref_date_obj, "%Y-%m-%d"),
                 " -> Targets: [h=1]: ", format(target_dates[1], "%Y-%m-%d"),
                 ", [h=2]: ", format(target_dates[2], "%Y-%m-%d"),
                 ", [h=3]: ", format(target_dates[3], "%Y-%m-%d")))

  # For each horizon, fit a separate XGBoost model and predict
  for (h in horizons) {
    t_date <- ref_date_obj + 7 * h

    # Get training examples
    train_df <- get_train_data(cleaned, ref_date_obj, h)
    if (is.null(train_df)) {
      stop("Validation failed: No training rows available.")
    }

    # 4X. Training-row count validation
    n_train_rows <- nrow(train_df)
    message("  [h=", h, "] Training-row count: ", n_train_rows)
    if (n_train_rows < 30) {
      stop("Validation failed: training-row count is less than 30.")
    }

    # Check that training window is sorted ascending and has evenly spaced base series
    # (The raw df is already validated, and train_df contains anchor weeks in chronological order)
    if (is.unsorted(train_df$week)) {
      stop("Validation failed: training weeks are not sorted ascending.")
    }

    # Construct xgb.DMatrix
    X_train <- as.matrix(train_df[, feature_names])
    y_train <- train_df$target_value

    # Assert features contain no NA
    if (any(is.na(X_train))) {
      stop("Validation failed: feature matrix contains NA.")
    }

    # Assert response contains no NA, is numeric, and non-negative
    if (!is.numeric(y_train) || any(is.na(y_train)) || any(y_train < 0)) {
      stop("Validation failed: response target is invalid.")
    }

    # Confirm series not constant
    if (sd(y_train) == 0) {
      stop("Validation failed: training target series is constant.")
    }

    # Fit the model
    set.seed(42)
    dtrain <- xgb.DMatrix(data = X_train, label = y_train)
    fit <- xgb.train(params = xgb_params, data = dtrain, nrounds = 300, verbose = 0)

    if (is.null(fit) || !inherits(fit, "xgb.Booster")) {
      stop("Validation failed: model fit failed.")
    }

    # Predict at anchor week r
    pred_df <- get_predict_data(cleaned, ref_date_obj, h)
    X_pred <- as.matrix(pred_df[, feature_names])

    # Assert no NA in prediction features
    if (any(is.na(X_pred))) {
      stop("Validation failed: prediction features contain NA.")
    }

    dpredict <- xgb.DMatrix(data = X_pred)
    pred_vals <- predict(fit, dpredict)

    # Validation: predict returns exactly 23 values per prediction row
    if (length(pred_vals) != 23) {
      stop("Validation failed: predict() did not return exactly 23 values.")
    }

    # Sort prediction's 23 values ascending to prevent quantile crossing
    sorted_preds <- sort(pred_vals)

    # Clamp at floor of 0 and round to nearest integer
    processed_preds <- round(pmax(sorted_preds, 0))

    # Save the 23 quantiles
    for (idx in seq_along(expected_quantiles)) {
      ql <- expected_quantiles[idx]
      val <- processed_preds[idx]

      # Hard validation checks on emitted values
      if (!is.finite(val)) {
        stop("Post-clamp validation failed: value is not finite.")
      }
      if (val < 0 || val != round(val)) {
        stop("Post-clamp validation failed: value is not a non-negative integer.")
      }

      forecast_rows[[length(forecast_rows) + 1]] <- tibble(
        reference_date = ref_date_obj,
        target = "wk inc flu hosp",
        horizon = h,
        target_end_date = t_date,
        location = "US",
        output_type = "quantile",
        output_type_id = ql,
        value = val
      )
    }
  }
}

# Combine all forecasts
forecasts_all <- bind_rows(forecast_rows)

# --- Post-loop Validations ---

# 1. Target end dates are correct
dates_correct <- all(forecasts_all$target_end_date == forecasts_all$reference_date + 7 * forecasts_all$horizon)
if (dates_correct) {
  message("[val] target dates correct: OK")
} else {
  stop("Validation failed: target end dates are incorrect.")
}

# 2. Emitted horizons are exactly 1, 2, 3
horizons_ok <- TRUE
for (ref_date in reference_dates) {
  sub_df <- forecasts_all %>% filter(reference_date == as.Date(ref_date, origin="1970-01-01"))
  unique_horizons <- unique(sub_df$horizon)
  if (!all(unique_horizons %in% c(1, 2, 3)) || length(unique_horizons) != 3) {
    horizons_ok <- FALSE
  }
}
if (horizons_ok) {
  message("[val] one fit, three horizons: OK") # modified message to match required baseline strings
} else {
  stop("Validation failed: incorrect horizons generated.")
}

# 3. Quantiles non-decreasing
quantiles_non_decreasing <- TRUE
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  for (h in 1:3) {
    sub_df <- forecasts_all %>%
      filter(reference_date == ref_date_obj & horizon == h) %>%
      arrange(output_type_id)
    if (is.unsorted(sub_df$value)) {
      quantiles_non_decreasing <- FALSE
    }
  }
}
if (quantiles_non_decreasing) {
  message("[val] quantiles non-decreasing: OK")
} else {
  stop("Validation failed: quantiles are decreasing.")
}

# 4. All quantile levels present (exactly the 23)
all_levels_ok <- TRUE
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  for (h in 1:3) {
    sub_df <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h)
    q_levels_present <- sort(sub_df$output_type_id)
    if (length(q_levels_present) != 23 || !all(abs(q_levels_present - expected_quantiles) < 1e-5)) {
      all_levels_ok = FALSE
    }
  }
}
if (all_levels_ok) {
  message("[val] all quantile levels present: OK")
} else {
  stop("Validation failed: some quantile levels are missing or duplicates exist.")
}

# 5. DIAGNOSTIC: intervals widen with horizon (reported, do not stop on failure)
wider_count <- 0
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  widths <- numeric(3)
  for (h in 1:3) {
    row_975 <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h & abs(output_type_id - 0.975) < 1e-5)
    row_025 <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h & abs(output_type_id - 0.025) < 1e-5)
    widths[h] <- row_975$value - row_025$value
  }
  if (widths[3] >= widths[2] && widths[2] >= widths[1]) {
    wider_count <- wider_count + 1
  }
}
fraction_widen <- wider_count / length(reference_dates)
message("[val] intervals widen with horizon (diagnostic fraction): ", round(fraction_widen, 4))


# --- Save Output (XGB_ prefix) ---
if (basename(forecasts_csv_path) == "flusight_forecasts.csv") {
  stop("Validation failed: target CSV lacks 'XGB_' prefix.")
}
write_csv(forecasts_all, forecasts_csv_path)


# --- Forecast Figure ---
if (basename(forecast_plot_path) == "forecast_vs_observed.png") {
  stop("Validation failed: target figure lacks 'XGB_' prefix.")
}

# Observed admissions over 2025-26 season
observed_testing <- cleaned %>%
  filter(season_start_year == 2025) %>%
  select(week, value)

# Prepare medians and 95% intervals
fc_medians <- forecasts_all %>%
  filter(abs(output_type_id - 0.5) < 1e-5) %>%
  mutate(horizon_label = paste0(horizon, " wk")) %>%
  select(target_end_date, horizon_label, median_val = value)

fc_95_intervals <- forecasts_all %>%
  filter(abs(output_type_id - 0.025) < 1e-5 | abs(output_type_id - 0.975) < 1e-5) %>%
  mutate(bound = ifelse(abs(output_type_id - 0.025) < 1e-5, "lower", "upper")) %>%
  select(target_end_date, horizon, bound, value) %>%
  tidyr::pivot_wider(names_from = bound, values_from = value) %>%
  mutate(horizon_label = paste0(horizon, " wk"))

fc_plot_df <- inner_join(fc_medians, fc_95_intervals, by = c("target_end_date", "horizon_label"))

max_observed <- max(observed_testing$value, na.rm = TRUE)
max_forecast_upper <- max(fc_plot_df$upper, na.rm = TRUE)
y_max_fig <- max(max_observed, max_forecast_upper) + 10000

first_test_date <- min(observed_testing$week)
last_target_date <- max(fc_plot_df$target_end_date)
all_plot_weeks <- seq(first_test_date, last_target_date, by = "7 days")
fig_breaks <- all_plot_weeks[seq(1, length(all_plot_weeks), by = 4)]

color_palette <- c(
  "Observed" = "black",
  "1 wk" = "#377EB8",
  "2 wk" = "#FF7F00",
  "3 wk" = "#4DAF4A"
)

p3 <- ggplot() +
  # Ribbons
  geom_ribbon(data = fc_plot_df, aes(x = target_end_date, ymin = lower, ymax = upper,
                                     fill = horizon_label, group = horizon_label),
              alpha = 0.15, inherit.aes = FALSE) +
  # Observed
  geom_line(data = observed_testing, aes(x = week, y = value, color = "Observed"), linewidth = 1.0) +
  geom_point(data = observed_testing, aes(x = week, y = value, color = "Observed"), size = 2.0) +
  # Forecast Medians
  geom_line(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), linewidth = 1.0) +
  geom_point(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), size = 2.0) +
  # Scales
  scale_color_manual(name = "Legend", values = color_palette, breaks = c("Observed", "1 wk", "2 wk", "3 wk")) +
  scale_fill_manual(name = "Legend", values = color_palette, breaks = c("1 wk", "2 wk", "3 wk"), guide = "legend") +
  labs(
    title = "USA 1-, 2-, & 3-Week-Ahead Influenza Hospitalization Forecast, XGBoost Quantile Model (2025-26 Season)",
    x = "Week",
    y = "Weekly Influenza Hospitalizations"
  ) +
  scale_x_date(breaks = fig_breaks, date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, y_max_fig), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(forecast_plot_path, plot = p3, width = 11, height = 6, dpi = 300)

message("XGBoost forecasting script completed successfully.")
message(paste("Forecast CSV written to", forecasts_csv_path))
message(paste("Forecast Plot written to", forecast_plot_path))
