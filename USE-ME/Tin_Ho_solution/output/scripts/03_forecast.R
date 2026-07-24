#!/usr/bin/env Rscript

# 03_forecast.R - Rolling ARIMA Forecasting
# Following instructions in rules.md

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(MMWRweek)
  library(forecast)
  library(ggplot2)
})

# Setup paths
input_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_dir_data <- "output/data/03_forecast"
output_dir_fig <- "output/figures/03_forecast"

# Ensure output directories exist
dir.create(output_dir_data, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_fig, recursive = TRUE, showWarnings = FALSE)

# 1. Read and validate input data
if (!file.exists(input_csv)) {
  stop("Input file not found: ", input_csv)
}

raw_data <- tryCatch({
  read_csv(input_csv, col_types = cols(.default = col_character()))
}, error = function(e) {
  stop("The data could not be parsed.")
})

cleaned <- raw_data

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
    message("Notice: non-US locations or formatting issues found; converting location to 'US'")
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

# Ensure chronological sorting
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
      TRUE                             ~ as.double(NA) # off-season
    )
  ) %>%
  mutate(
    season = ifelse(is.na(season_start_year), "Off-Season",
                    paste0(season_start_year, "-", substr(as.character(season_start_year + 1), 3, 4)))
  )

# Current Season details (2025-26)
current_season_data <- cleaned %>% filter(season_start_year == 2025)
season_start_date <- min(current_season_data$week)
season_end_date <- max(current_season_data$week)

# Print current season determination message
message(paste0("Season Start Week: ", format(season_start_date, "%Y-%m-%d")))
message(paste0("Season End Week: ", format(season_end_date, "%Y-%m-%d")))


# 3. Training and Testing Periods
training_period_start <- min(cleaned$week)
testing_period_start <- season_start_date

# Horizons
horizons <- c(1, 2, 3)

# Print Validations
message("Training Period start date: ", format(training_period_start, "%Y-%m-%d"))
message("Testing Period start date: ", format(testing_period_start, "%Y-%m-%d"))
message("Forecasting horizons: ", paste(horizons, collapse = ", "))

# Identify reference dates (observed weeks r such that r + 7 falls in testing period and is present in data)
reference_dates <- cleaned %>%
  filter((week + 7) >= season_start_date & (week + 7) <= season_end_date & (week + 7) %in% cleaned$week) %>%
  pull(week) %>%
  sort() %>%
  unique()

message("Rolling-window end (reference) dates count: ", length(reference_dates))
message("First reference date: ", format(min(reference_dates), "%Y-%m-%d"))
message("Last reference date: ", format(max(reference_dates), "%Y-%m-%d"))


# 4. Model Fit & Forecast Loop
forecast_rows <- list()
fit_count_check <- list() # To verify one fit per reference date

# Map levels of coverage to quantiles
levels_list <- c(98, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10)

# Pre-clamping symmetry validation helper
validate_symmetry <- function(mean_val, lower_mat, upper_mat, levels) {
  for (h in 1:3) {
    for (C in levels) {
      col_name <- paste0(C, "%")
      lb <- lower_mat[h, col_name]
      ub <- upper_mat[h, col_name]
      diff_lower <- mean_val[h] - lb
      diff_upper <- ub - mean_val[h]
      if (abs(diff_lower - diff_upper) > 1e-5) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Run loop over reference dates
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")

  # Print validations for ref date and its targets
  target_dates <- ref_date_obj + 7 * horizons
  message(paste0("Reference Date: ", format(ref_date_obj, "%Y-%m-%d"),
                 " -> Targets: [h=1]: ", format(target_dates[1], "%Y-%m-%d"),
                 ", [h=2]: ", format(target_dates[2], "%Y-%m-%d"),
                 ", [h=3]: ", format(target_dates[3], "%Y-%m-%d")))

  # Filter training series: week <= reference_date
  train_data <- cleaned %>% filter(week <= ref_date_obj) %>% arrange(week)

  # Validations: Training sorted ascending, no missing/duplicate weeks
  is_sorted <- !is.unsorted(train_data$week)
  no_duplicates <- length(unique(train_data$week)) == nrow(train_data)
  no_missing_dates <- all(!is.na(train_data$week))
  if (!(is_sorted && no_duplicates && no_missing_dates)) {
    stop("Validation failed: training window is not sorted ascending or contains missing/duplicate weeks.")
  }

  # Validations: evenly spaced (no skipped weeks)
  week_diffs <- diff(as.numeric(train_data$week))
  if (!all(week_diffs == 7)) {
    stop("Validation failed: week index has skipped weeks (is not evenly spaced).")
  }

  # Validations: Extract response vector, confirm numeric, non-negative, free of NA
  y <- train_data$value
  if (!is.numeric(y) || any(y < 0) || any(is.na(y))) {
    stop("Validation failed: response series is not a numeric, non-negative, NA-free vector.")
  }

  # Validations: Series is not constant
  if (sd(y) == 0) {
    stop("Validation failed: series is constant before fitting.")
  }

  # Fit non-seasonal ARIMA using auto.arima() on numeric vector
  fit <- auto.arima(as.numeric(y))
  if (is.null(fit) || !inherits(fit, "ARIMA")) {
    stop("Validation failed: auto.arima fit returned NULL or invalid object.")
  }

  # Print the order
  ord <- arimaorder(fit)
  message(paste0("  ARIMA order selected: (", ord["p"], ",", ord["d"], ",", ord["q"], ")"))

  # Keep track of fit count to check one fit per ref date
  fit_count_check[[format(ref_date_obj, "%Y-%m-%d")]] <- 1

  # Forecast h = 3 horizons
  fc <- forecast(fit, h = 3, level = levels_list)

  # Pre-clamp validation: symmetry and median centering
  sym_ok <- validate_symmetry(fc$mean, fc$lower, fc$upper, levels_list)
  if (!sym_ok) {
    stop("Pre-clamp validation failed: point forecast/median is not centered, or quantiles are asymmetric.")
  }

  # Build long data frame for the 23 quantiles at each horizon
  for (h in horizons) {
    t_date <- ref_date_obj + 7 * h

    # 23 quantiles representation
    # Extract lower bounds, median, upper bounds
    q_data <- list()

    # Lower bounds
    for (C in levels_list) {
      col_name <- paste0(C, "%")
      # lb quantile
      q_val <- (1 - C/100)/2
      val_raw <- fc$lower[h, col_name]
      # Clamp at floor of 0 and round to nearest integer
      val_processed <- round(pmax(val_raw, 0))
      q_data[[as.character(q_val)]] <- val_processed
    }

    # Median
    val_raw_med <- fc$mean[h]
    val_processed_med <- round(pmax(val_raw_med, 0))
    q_data[["0.5"]] <- val_processed_med

    # Upper bounds
    for (C in levels_list) {
      col_name <- paste0(C, "%")
      # ub quantile
      q_val <- 1 - (1 - C/100)/2
      val_raw <- fc$upper[h, col_name]
      # Clamp at floor of 0 and round to nearest integer
      val_processed <- round(pmax(val_raw, 0))
      q_data[[as.character(q_val)]] <- val_processed
    }

    # Sort by quantile level to ensure ladder validation checks
    q_levels <- sort(as.numeric(names(q_data)))

    # Validate finite values, non-decreasing, non-negative integers
    prev_val <- -Inf
    for (ql in q_levels) {
      val <- q_data[[as.character(ql)]]
      if (!is.finite(val)) {
        stop("Post-clamp validation failed: value is not finite.")
      }
      if (val < 0 || val != round(val)) {
        stop("Post-clamp validation failed: value is not a non-negative integer.")
      }
      if (val < prev_val) {
        stop("Post-clamp validation failed: quantile ladder is decreasing.")
      }
      prev_val <- val

      # Save row to final collection
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

# 1. Confirm interval widths get wider (or stay equal) as horizon grows
intervals_widen <- TRUE
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")

  # 95% PI width = (0.975 quantile) - (0.025 quantile)
  widths <- numeric(3)
  for (h in 1:3) {
    row_975 <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h & abs(output_type_id - 0.975) < 1e-5)
    row_025 <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h & abs(output_type_id - 0.025) < 1e-5)
    if (nrow(row_975) == 1 && nrow(row_025) == 1) {
      widths[h] <- row_975$value - row_025$value
    } else {
      stop("Failed to retrieve 95% PI bounds for validation.")
    }
  }
  if (!(widths[3] >= widths[2] && widths[2] >= widths[1])) {
    intervals_widen <- FALSE
  }
}

if (intervals_widen) {
  message("[val] intervals widen with horizon: OK")
} else {
  stop("Validation failed: intervals do not widen with horizon.")
}

# 2. Confirm target dates correct
dates_correct <- all(forecasts_all$target_end_date == forecasts_all$reference_date + 7 * forecasts_all$horizon)
if (dates_correct) {
  message("[val] target dates correct: OK")
} else {
  stop("Validation failed: target end dates are incorrect.")
}

# 3. Confirm one fit, three horizons
one_fit_three_horizons <- TRUE
for (ref_date in reference_dates) {
  ref_date_str <- format(as.Date(ref_date, origin="1970-01-01"), "%Y-%m-%d")
  if (is.null(fit_count_check[[ref_date_str]]) || fit_count_check[[ref_date_str]] != 1) {
    one_fit_three_horizons <- FALSE
  }
  sub_df <- forecasts_all %>% filter(reference_date == as.Date(ref_date, origin="1970-01-01"))
  unique_horizons <- unique(sub_df$horizon)
  if (!all(unique_horizons %in% c(1, 2, 3)) || length(unique_horizons) != 3) {
    one_fit_three_horizons <- FALSE
  }
}

if (one_fit_three_horizons) {
  message("[val] one fit, three horizons: OK")
} else {
  stop("Validation failed: multiple fits or incorrect horizons generated.")
}

# 4. Confirm quantiles non-decreasing
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

# 5. Confirm all quantile levels present (exactly the 23 output_type_id values, no duplicates)
all_levels_ok <- TRUE
expected_quantiles <- c(0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99)
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  for (h in 1:3) {
    sub_df <- forecasts_all %>% filter(reference_date == ref_date_obj & horizon == h)
    q_levels_present <- sort(sub_df$output_type_id)
    if (length(q_levels_present) != 23 || !all(abs(q_levels_present - expected_quantiles) < 1e-5)) {
      all_levels_ok <- FALSE
    }
  }
}

if (all_levels_ok) {
  message("[val] all quantile levels present: OK")
} else {
  stop("Validation failed: some quantile levels are missing or duplicates exist.")
}

# 6. Confirm pre-clamp symmetry is verified
# (We already threw an error during the loop if symmetry wasn't met on raw forecasts)
message("[val] median centered, quantiles symmetric: OK")


# --- Save Output ---
forecasts_csv_path <- file.path(output_dir_data, "flusight_forecasts.csv")
write_csv(forecasts_all, forecasts_csv_path)


# --- Forecast Figure ---
forecast_plot_path <- file.path(output_dir_fig, "forecast_vs_observed.png")

# Get Observed admissions over the testing period
observed_testing <- cleaned %>%
  filter(season_start_year == 2025) %>%
  select(week, value)

# Prepare forecast medians and 95% PIs per horizon
fc_medians <- forecasts_all %>%
  filter(abs(output_type_id - 0.5) < 1e-5) %>%
  mutate(horizon_label = paste0(horizon, " wk")) %>%
  select(target_end_date, horizon_label, median_val = value)

fc_95_intervals <- forecasts_all %>%
  filter(abs(output_type_id - 0.025) < 1e-5 | abs(output_type_id - 0.975) < 1e-5) %>%
  # Reshape wide to get lower and upper
  mutate(bound = ifelse(abs(output_type_id - 0.025) < 1e-5, "lower", "upper")) %>%
  select(target_end_date, horizon, bound, value) %>%
  tidyr::pivot_wider(names_from = bound, values_from = value) %>%
  mutate(horizon_label = paste0(horizon, " wk"))

# Merge medians and intervals
fc_plot_df <- inner_join(fc_medians, fc_95_intervals, by = c("target_end_date", "horizon_label"))

# Configure Y-axis limits
max_observed <- max(observed_testing$value, na.rm = TRUE)
max_forecast_upper <- max(fc_plot_df$upper, na.rm = TRUE)
y_max_fig <- max(max_observed, max_forecast_upper) + 10000

# Plot dates range (weekly spacing from first week in 2025-26 testing period to last target_end_date)
first_test_date <- min(observed_testing$week)
last_target_date <- max(fc_plot_df$target_end_date)
all_plot_weeks <- seq(first_test_date, last_target_date, by = "7 days")

# Show every 4th label
fig_breaks <- all_plot_weeks[seq(1, length(all_plot_weeks), by = 4)]

# Custom Colors
color_palette <- c(
  "Observed" = "black",
  "1 wk" = "#377EB8",  # Blue
  "2 wk" = "#FF7F00",  # Orange
  "3 wk" = "#4DAF4A"   # Green
)

p3 <- ggplot() +
  # Ribbons first to sit in background
  geom_ribbon(data = fc_plot_df, aes(x = target_end_date, ymin = lower, ymax = upper,
                                     fill = horizon_label, group = horizon_label),
              alpha = 0.15, inherit.aes = FALSE) +
  # Observed admissions line and points
  geom_line(data = observed_testing, aes(x = week, y = value, color = "Observed"), linewidth = 1.0) +
  geom_point(data = observed_testing, aes(x = week, y = value, color = "Observed"), size = 2.0) +
  # Forecast median lines and points
  geom_line(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), linewidth = 1.0) +
  geom_point(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), size = 2.0) +
  # Manual scales
  scale_color_manual(name = "Legend", values = color_palette, breaks = c("Observed", "1 wk", "2 wk", "3 wk")) +
  scale_fill_manual(name = "Legend", values = color_palette, breaks = c("1 wk", "2 wk", "3 wk"), guide = "legend") +
  labs(
    title = "USA 1-, 2-, & 3-Week-Ahead Influenza Hospitalization Forecast (2025-26 Season)",
    x = "Week",
    y = "Weekly Influenza Hospitalizations"
  ) +
  scale_x_date(breaks = fig_breaks, date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, y_max_fig), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(forecast_plot_path, plot = p3, width = 11, height = 6, dpi = 300)

message("Forecasting script completed successfully.")
message(paste("Forecast CSV written to", forecasts_csv_path))
message(paste("Forecast Plot written to", forecast_plot_path))
