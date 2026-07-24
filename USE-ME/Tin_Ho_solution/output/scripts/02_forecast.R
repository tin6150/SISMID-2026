#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
})

input_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_csv <- "output/data/02_forecast/forecasted_flu_admissions.csv"
output_plot <- "output/figures/02_forecast/forecast_us_flu_admissions.png"

# Create output directories.
dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_plot), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_csv)) {
  stop("Input cleaned data not found: ", input_csv)
}

cleaned <- read_csv(input_csv, col_types = cols(
  week = col_date(),
  location = col_character(),
  value = col_double()
))

if (nrow(cleaned) == 0) stop("Cleaned data must contain at least one row.")
if (!identical(names(cleaned), c("week", "location", "value"))) {
  stop("Cleaned data must have columns: week, location, value")
}
if (!all(cleaned$location == "US")) stop("All location values must be US.")
if (!inherits(cleaned$week, "Date")) stop("week must be a Date column.")
if (!is.numeric(cleaned$value)) stop("value must be numeric.")
if (any(is.na(cleaned$value))) stop("value contains NA after parsing.")

cleaned <- cleaned[order(cleaned$week), ]

# Build a weekly time series of influenza admissions.
weekly_ts <- ts(cleaned$value, frequency = 52)
forecast_horizon <- 12

fit <- tryCatch(
  stats::HoltWinters(weekly_ts),
  error = function(e) stop("Forecast model fit failed: ", e$message)
)

forecast_values <- as.numeric(predict(fit, n.ahead = forecast_horizon))
forecast_weeks <- seq(max(cleaned$week) + 7, by = 7, length.out = forecast_horizon)
forecast_df <- data.frame(
  week = forecast_weeks,
  location = rep("US", forecast_horizon),
  value = forecast_values,
  stringsAsFactors = FALSE
)

write_csv(forecast_df, output_csv)

png(output_plot, width = 1200, height = 600)
plot(
  cleaned$week,
  cleaned$value,
  type = "l",
  col = "steelblue",
  lwd = 2,
  xlab = "Week",
  ylab = "Influenza admissions",
  main = "US Weekly Influenza Admissions and 12-Week Forecast"
)
lines(forecast_weeks, forecast_values, col = "firebrick", lwd = 2, lty = 2)
points(forecast_weeks, forecast_values, col = "firebrick", pch = 19)
legend(
  "topleft",
  legend = c("Observed admissions", "Forecast"),
  col = c("steelblue", "firebrick"),
  lwd = 2,
  lty = c(1, 2),
  bty = "n"
)
dev.off()

if (!file.exists(output_csv)) stop("Forecast output CSV was not written: ", output_csv)
if (!file.exists(output_plot)) stop("Forecast plot was not written: ", output_plot)
