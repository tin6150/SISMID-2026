#!/usr/bin/env Rscript

# 05_incremental_changes.R - Wastewater-informed ARIMAX Forecasting
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
flu_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
wval_csv <- "data/NWSSWVALNational.csv"
output_dir_data <- "output/data/03_forecast"
output_dir_fig <- "output/figures/03_forecast"

# Ensure output directories exist
dir.create(output_dir_data, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_fig, recursive = TRUE, showWarnings = FALSE)

# 1. Read and validate flu input data
if (!file.exists(flu_csv)) {
  stop("Flu input file not found: ", flu_csv)
}

cleaned_flu <- tryCatch({
  read_csv(flu_csv, col_types = cols(.default = col_character()))
}, error = function(e) {
  stop("The flu data could not be parsed.")
})

# Parse flu columns
if ("week" %in% names(cleaned_flu)) {
  parsed_week <- as.Date(cleaned_flu$week, format = "%Y-%m-%d")
  if (any(is.na(parsed_week))) {
    parsed_week <- tryCatch(as.Date(cleaned_flu$week), error = function(e) NA)
  }
  if (any(is.na(parsed_week))) {
    stop("The week could not be parsed.")
  }
  cleaned_flu$week <- parsed_week
} else {
  stop("The week could not be parsed.")
}

if ("location" %in% names(cleaned_flu)) {
  cleaned_flu$location <- as.character(cleaned_flu$location)
  if (!all(cleaned_flu$location == "US")) {
    cleaned_flu$location <- "US"
  }
} else {
  stop("The location could not be parsed.")
}

if ("value" %in% names(cleaned_flu)) {
  parsed_value <- suppressWarnings(as.numeric(cleaned_flu$value))
  if (any(is.na(parsed_value))) {
    parsed_value <- suppressWarnings(readr::parse_number(cleaned_flu$value))
  }
  if (any(is.na(parsed_value))) {
    stop("The value could not be parsed.")
  }
  cleaned_flu$value <- parsed_value
} else {
  stop("The value could not be parsed.")
}

cleaned_flu <- cleaned_flu %>% arrange(week)

# 2. Season Mapping & Current Season Determination for Flu
cleaned_flu <- cleaned_flu %>%
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

current_season_data <- cleaned_flu %>% filter(season_start_year == 2025)
season_start_date <- min(current_season_data$week)
season_end_date <- max(current_season_data$week)

message(paste0("Season Start Week: ", format(season_start_date, "%Y-%m-%d")))
message(paste0("Season End Week: ", format(season_end_date, "%Y-%m-%d")))

# 0W. Read and Process National Wastewater (NWSS WVAL)
if (!file.exists(wval_csv)) {
  stop("Wastewater input file not found: ", wval_csv)
}

raw_wval <- tryCatch({
  read_csv(wval_csv, col_types = cols(.default = col_character()))
}, error = function(e) {
  stop("The wastewater data could not be parsed.")
})

# Filter to Influenza A virus
wval_filtered <- raw_wval %>%
  filter(`Pathogen Target` == "Influenza A virus")

if (nrow(wval_filtered) == 0) {
  stop("Zero rows remain after filtering to 'Influenza A virus'.")
}

# Parse Week End to week Date
parsed_wval_week <- as.Date(wval_filtered$`Week End`, format = "%m/%d/%Y")
if (any(is.na(parsed_wval_week))) {
  stop("The Week End could not be parsed.")
}
wval_filtered$week <- parsed_wval_week

# Parse National WVAL to numeric wval
parsed_wval_val <- suppressWarnings(readr::parse_number(wval_filtered$`National WVAL`))
if (any(is.na(parsed_wval_val))) {
  stop("The National WVAL could not be parsed.")
}
wval_filtered$wval <- parsed_wval_val

# Sort and de-duplicate
wastewater <- wval_filtered %>%
  select(week, wval) %>%
  arrange(week) %>%
  distinct(week, .keep_all = TRUE)

# Validations: wastewater range and count
wval_range <- range(wastewater$week)
message("Wastewater series date range: ", format(wval_range[1], "%Y-%m-%d"), " to ", format(wval_range[2], "%Y-%m-%d"))
message("Wastewater series row count: ", nrow(wastewater))

# Validate weekday and spacing (7-day intervals)
wval_weekdays <- weekdays(wastewater$week)
flu_weekdays <- weekdays(cleaned_flu$week)
if (length(unique(wval_weekdays)) != 1 || unique(wval_weekdays) != unique(flu_weekdays)[1]) {
  stop("Wastewater dates are not on the same weekday as flu admissions.")
}

wval_diffs <- diff(as.numeric(wastewater$week))
if (!all(wval_diffs == 7)) {
  stop("Wastewater weeks are not evenly spaced at 7 days.")
}

# Overlap validation
overlap_weeks <- intersect(cleaned_flu$week, wastewater$week)
overlap_range <- range(as.Date(overlap_weeks, origin = "1970-01-01"))
message("Number of overlapping weeks: ", length(overlap_weeks))
message("Overlap date range: ", format(overlap_range[1], "%Y-%m-%d"), " to ", format(overlap_range[2], "%Y-%m-%d"))


# 1W. Lagged Regressor Construction
WVAL_LAG <- 3
# Regressor construction: wval lagged by 3 weeks (t - 21)
# Create a helper dataframe where the date is shifted forward by 21 days
wval_lagged <- wastewater %>%
  mutate(week_join = week + 21) %>%
  select(week = week_join, wval_lag3 = wval)

# Rationale and validation table display
message("WVAL_LAG chosen: ", WVAL_LAG)
message("Lagged Regressor rationale: For any week t, wval_lag3(t) = wval at week (t - 21)")
message("Horizon | Target Week | Regressor Week | Status at reference date r")
message("   1    |   r + 7     |     r - 14     | Observed")
message("   2    |   r + 14    |     r - 7      | Observed")
message("   3    |   r + 21    |     r          | Observed")

# Reference dates set from baseline
reference_dates <- cleaned_flu %>%
  filter((week + 7) >= season_start_date & (week + 7) <= season_end_date & (week + 7) %in% cleaned_flu$week) %>%
  pull(week) %>%
  sort() %>%
  unique()

# Confirm that for every reference date, all three regressor weeks (r - 14, r - 7, r) are present in wastewater
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  required_weeks <- ref_date_obj - c(14, 7, 0)
  missing_weeks <- required_weeks[!required_weeks %in% wastewater$week]
  if (length(missing_weeks) > 0) {
    stop("Wastewater series is missing required regressor weeks for reference date: ", format(ref_date_obj, "%Y-%m-%d"),
         ". Missing: ", paste(format(missing_weeks, "%Y-%m-%d"), collapse = ", "))
  }
}


# 2W. Training Window Restriction
# Join lagged wastewater to the flu series
flu_with_wval <- cleaned_flu %>%
  left_join(wval_lagged, by = "week")


# --- ARIMAX Model Fit & Forecast Loop ---
forecast_rows <- list()
fit_count_check <- list()
levels_list <- c(98, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10)
horizons <- c(1, 2, 3)

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

# Keep track of dropped training weeks info
first_ref <- as.Date(reference_dates[1], origin = "1970-01-01")
univariate_train_start <- min(cleaned_flu$week)

# Run loop over reference dates
for (ref_date in reference_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  target_dates <- ref_date_obj + 7 * horizons

  message(paste0("Reference Date: ", format(ref_date_obj, "%Y-%m-%d"),
                 " -> Targets: [h=1]: ", format(target_dates[1], "%Y-%m-%d"),
                 ", [h=2]: ", format(target_dates[2], "%Y-%m-%d"),
                 ", [h=3]: ", format(target_dates[3], "%Y-%m-%d")))

  # Filter training: week <= r and wval_lag3 is non-NA
  train_full <- flu_with_wval %>% filter(week <= ref_date_obj) %>% arrange(week)
  train_data <- train_full %>% filter(!is.na(wval_lag3))

  # Print training start and dropped history at first reference date as validation
  if (ref_date_obj == first_ref) {
    arimax_train_start <- min(train_data$week)
    dropped_weeks <- as.integer(difftime(arimax_train_start, univariate_train_start, units = "weeks"))
    message("ARIMAX Training Start Date: ", format(arimax_train_start, "%Y-%m-%d"))
    message("Weeks of flu history dropped relative to univariate training: ", dropped_weeks)

    # Confirm at least 52 training rows remain
    if (nrow(train_data) < 52) {
      stop("Validation failed: restricted training window has fewer than 52 rows.")
    }
  }

  # Validations: Training sorted ascending, no missing/duplicate weeks
  is_sorted <- !is.unsorted(train_data$week)
  no_duplicates <- length(unique(train_data$week)) == nrow(train_data)
  no_missing_dates <- all(!is.na(train_data$week))
  if (!(is_sorted && no_duplicates && no_missing_dates)) {
    stop("Validation failed: training window is not sorted ascending or contains missing/duplicate weeks.")
  }

  # Confirm the restricted training window is contiguous
  train_diffs <- diff(as.numeric(train_data$week))
  if (!all(train_diffs == 7)) {
    stop("Validation failed: restricted training window is not contiguous.")
  }

  # Validations: Response series
  y <- train_data$value
  if (!is.numeric(y) || any(y < 0) || any(is.na(y))) {
    stop("Validation failed: response series is not a numeric, non-negative, NA-free vector.")
  }

  # Series is not constant
  if (sd(y) == 0) {
    stop("Validation failed: series is constant before fitting.")
  }

  # 4W. Model Specification & Fit
  xreg_train <- matrix(train_data$wval_lag3, ncol = 1)
  colnames(xreg_train) <- "wval_lag3"

  # Validate xreg has same length and no NA
  if (nrow(xreg_train) != length(y) || any(is.na(xreg_train))) {
    stop("Validation failed: xreg size does not match response or contains NA.")
  }

  # Validate xreg is not constant
  if (sd(xreg_train) == 0) {
    stop("Validation failed: external regressor is constant.")
  }

  # Fit ARIMA with external regressor
  fit <- auto.arima(as.numeric(y), xreg = xreg_train)
  if (is.null(fit) || !inherits(fit, "ARIMA")) {
    stop("Validation failed: ARIMAX auto.arima fit returned NULL.")
  }

  # Print order and fitted coefficient
  ord <- arimaorder(fit)
  coeff <- coef(fit)["wval_lag3"]
  message(paste0("  ARIMAX order: (", ord["p"], ",", ord["d"], ",", ord["q"], ") | wval_lag3 coef: ", round(coeff, 4)))

  fit_count_check[[format(ref_date_obj, "%Y-%m-%d")]] <- 1

  # Construct xreg_future
  wval_r_minus_14 <- wastewater$wval[wastewater$week == (ref_date_obj - 14)]
  wval_r_minus_7  <- wastewater$wval[wastewater$week == (ref_date_obj - 7)]
  wval_r          <- wastewater$wval[wastewater$week == ref_date_obj]

  xreg_future <- matrix(c(wval_r_minus_14, wval_r_minus_7, wval_r), nrow = 3, ncol = 1)
  colnames(xreg_future) <- "wval_lag3"

  if (nrow(xreg_future) != 3 || ncol(xreg_future) != 1 || any(is.na(xreg_future))) {
    stop("Validation failed: xreg_future size or NA violation.")
  }

  # Forecast
  fc <- forecast(fit, h = 3, xreg = xreg_future, level = levels_list)

  # Pre-clamp validation: symmetry and median centering
  sym_ok <- validate_symmetry(fc$mean, fc$lower, fc$upper, levels_list)
  if (!sym_ok) {
    stop("Pre-clamp validation failed: point forecast/median is not centered, or quantiles are asymmetric.")
  }

  # 5W. Forecast Generation
  for (h in horizons) {
    t_date <- ref_date_obj + 7 * h
    q_data <- list()

    # Lower bounds
    for (C in levels_list) {
      col_name <- paste0(C, "%")
      q_val <- (1 - C/100)/2
      val_raw <- fc$lower[h, col_name]
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
      q_val <- 1 - (1 - C/100)/2
      val_raw <- fc$upper[h, col_name]
      val_processed <- round(pmax(val_raw, 0))
      q_data[[as.character(q_val)]] <- val_processed
    }

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

      # Collect rows
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

# 5. Confirm all quantile levels present (exactly 23)
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
message("[val] median centered, quantiles symmetric: OK")


# --- Save Output (WVAL_ prefix) ---
forecasts_csv_path <- file.path(output_dir_data, "WVAL_flusight_forecasts.csv")

# 7W. Path Validation before writing
if (basename(forecasts_csv_path) == "flusight_forecasts.csv") {
  stop("Validation failed: Output path resolves to baseline unprefixed filename.")
}
write_csv(forecasts_all, forecasts_csv_path)


# --- Forecast Figure ---
forecast_plot_path <- file.path(output_dir_fig, "WVAL_forecast_vs_observed.png")

# Path Validation before writing
if (basename(forecast_plot_path) == "forecast_vs_observed.png") {
  stop("Validation failed: Figure path resolves to baseline unprefixed filename.")
}

# Get Observed admissions over the testing period
observed_testing <- cleaned_flu %>%
  filter(season_start_year == 2025) %>%
  select(week, value)

# Prepare forecast medians and 95% PIs per horizon
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
  "1 wk" = "#377EB8",  # Blue
  "2 wk" = "#FF7F00",  # Orange
  "3 wk" = "#4DAF4A"   # Green
)

p3 <- ggplot() +
  geom_ribbon(data = fc_plot_df, aes(x = target_end_date, ymin = lower, ymax = upper,
                                     fill = horizon_label, group = horizon_label),
              alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = observed_testing, aes(x = week, y = value, color = "Observed"), linewidth = 1.0) +
  geom_point(data = observed_testing, aes(x = week, y = value, color = "Observed"), size = 2.0) +
  geom_line(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), linewidth = 1.0) +
  geom_point(data = fc_plot_df, aes(x = target_end_date, y = median_val, color = horizon_label, group = horizon_label), size = 2.0) +
  scale_color_manual(name = "Legend", values = color_palette, breaks = c("Observed", "1 wk", "2 wk", "3 wk")) +
  scale_fill_manual(name = "Legend", values = color_palette, breaks = c("1 wk", "2 wk", "3 wk"), guide = "legend") +
  labs(
    title = "USA 1-, 2-, & 3-Week-Ahead Influenza Hospitalization Forecast, Wastewater-Informed ARIMAX (2025-26 Season)",
    x = "Week",
    y = "Weekly Influenza Hospitalizations"
  ) +
  scale_x_date(breaks = fig_breaks, date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, y_max_fig), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10), # smaller font to fit long title comfortably
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(forecast_plot_path, plot = p3, width = 11, height = 6, dpi = 300)

message("Wastewater ARIMAX forecasting script completed successfully.")
message(paste("Forecast CSV written to", forecasts_csv_path))
message(paste("Forecast Plot written to", forecast_plot_path))
