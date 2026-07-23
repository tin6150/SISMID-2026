library(readr)
library(dplyr)
library(lubridate)

input_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_plot <- "output/figures/02_barplot.png"

# Ensure output directory exists
dir.create(dirname(output_plot), recursive = TRUE, showWarnings = FALSE)

# Load cleaned data
cleaned_data <- read_csv(input_csv, col_types = cols(week = col_date(), location = col_character(), value = col_double()))

# Ensure the data is in the expected shape for plotting
if (!all(c("week", "location", "value") %in% names(cleaned_data))) {
  stop("Cleaned data is missing required columns for plotting.")
}

# Keep only US data and sort by week
plot_data <- cleaned_data %>%
  filter(location == "US") %>%
  arrange(week)

if (nrow(plot_data) == 0) {
  stop("No US data available for plotting.")
}

# Determine MMWR season for each date using the rule described in rules.md
get_season_label <- function(date) {
  epiweek <- isoweek(date)
  year_val <- year(date)

  if (epiweek >= 40 && month(date) %in% c(1:8)) {
    season_start_year <- year_val - 1
  } else {
    season_start_year <- year_val
  }

  season_end_year <- season_start_year + 1
  paste0(season_start_year, "-", season_end_year)
}

plot_data <- plot_data %>%
  mutate(
    epiweek = isoweek(week),
    season = vapply(week, get_season_label, character(1))
  )

# Identify the 2025-2026 season rows
season_rows <- which(plot_data$season == "2025-2026")

# Create the bar plot
png(filename = output_plot, width = 1600, height = 900, res = 200)
par(mar = c(8, 5, 4, 2))

barplot(
  height = plot_data$value,
  names.arg = format(plot_data$week, "%Y-%m-%d"),
  col = "green",
  main = "All US Influenza hospitalization",
  xlab = "week",
  ylab = "hospitalization",
  las = 2
)

# Shade the season window if rows exist
if (length(season_rows) > 0) {
  x_left <- season_rows[1] - 0.5
  x_right <- season_rows[length(season_rows)] + 0.5
  rect(xleft = x_left, xright = x_right, ybottom = -1e9, ytop = 1e9, col = rgb(1, 0.8, 0.8, 0.4), border = NA)
}

dev.off()

message(paste("Exploratory plot written to", output_plot))
