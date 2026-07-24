#!/usr/bin/env Rscript

# 02_data_explore.R - Visualization and Peak Analysis
# Following instructions in rules.md

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(MMWRweek)
  library(ggplot2)
})

# Setup paths
input_csv <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
output_dir_fig <- "output/figures/02_data_explore"
output_dir_data <- "output/data/02_data_explore"

# Ensure output directories exist
dir.create(output_dir_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir_data, recursive = TRUE, showWarnings = FALSE)

# 1. Read and validate input data
if (!file.exists(input_csv)) {
  stop("Input file not found: ", input_csv)
}

raw_data <- tryCatch({
  read_csv(input_csv, col_types = cols(.default = col_character()))
}, error = function(e) {
  stop("The data could not be parsed.")
})

# Parse columns with rules.md specifications
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
# Season start year base year is calendar year, January-August MMWRweek >= 40 belongs to year - 1
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


# 3. National Plotting
# Output: output/figures/02_data_explore/national_trend.png
national_plot_path <- file.path(output_dir_fig, "national_trend.png")

# Setup axis specifications
max_val <- max(cleaned$value, na.rm = TRUE)
y_max_lim <- max_val + 10000

# Selection of every 6th date for x-axis ticks
x_breaks <- cleaned$week[seq(1, nrow(cleaned), by = 6)]
x_labels <- format(x_breaks, "%m-%Y")

p1 <- ggplot(cleaned, aes(x = week, y = value)) +
  # Season highlight grey bar in background (under lines)
  annotate("rect", xmin = season_start_date, xmax = season_end_date, ymin = -Inf, ymax = Inf,
           fill = "lightgrey", color = NA, alpha = 0.5) +
  # Highlight text label
  annotate("text", x = season_start_date + (season_end_date - season_start_date)/2,
           y = y_max_lim * 0.95, label = "2025-26 Season", fontface = "bold", size = 4) +
  # Line chart
  geom_line(color = "blue", linewidth = 0.7) +
  # Axis and Labels
  labs(
    title = "USA Weekly Influenza Hospitalization Admissions",
    x = "Week",
    y = "Weekly Influenza Hospitalizations"
  ) +
  scale_x_date(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(limits = c(0, y_max_lim), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title = element_text(size = 11)
  )

ggsave(national_plot_path, plot = p1, width = 10, height = 6, dpi = 300)


# 4. Seasonal Plotting
# Output: output/figures/02_data_explore/seasonal_comparison.png
seasonal_plot_path <- file.path(output_dir_fig, "seasonal_comparison.png")

# Filter data to weeks 40-20
seasonal_data <- cleaned %>%
  filter(!is.na(season_start_year)) %>%
  filter(epi_week >= 40 | epi_week <= 20)

# Factor epiweek to ensure weeks 1-20 display chronologically after weeks 40-53
seasonal_data <- seasonal_data %>%
  mutate(epi_week_factor = factor(epi_week, levels = c(40:53, 1:20)))

# Define design scales
all_seasons <- sort(unique(seasonal_data$season))
colors_map <- c(
  "2020-21" = "#E41A1C",
  "2021-22" = "#377EB8",
  "2022-23" = "#4DAF4A",
  "2023-24" = "#984EA3",
  "2024-25" = "#FF7F00",
  "2025-26" = "black"
)
linetype_map <- c(
  "2020-21" = "dashed",
  "2021-22" = "dashed",
  "2022-23" = "dashed",
  "2023-24" = "dashed",
  "2024-25" = "dashed",
  "2025-26" = "solid"
)
size_map <- c(
  "2020-21" = 0.7,
  "2021-22" = 0.7,
  "2022-23" = 0.7,
  "2023-24" = 0.7,
  "2024-25" = 0.7,
  "2025-26" = 1.4
)

# Align palettes to seasons present in data
colors_map <- colors_map[all_seasons]
linetype_map <- linetype_map[all_seasons]
size_map <- size_map[all_seasons]

# X-axis ticks (every other week number)
all_levels <- c(40:53, 1:20)
x_breaks_season <- as.character(all_levels[seq(1, length(all_levels), by = 2)])

p2 <- ggplot(seasonal_data, aes(x = epi_week_factor, y = value,
                               color = season, linetype = season, linewidth = season,
                               group = season)) +
  geom_line() +
  scale_color_manual(name = "Season", values = colors_map) +
  scale_linetype_manual(name = "Season", values = linetype_map) +
  scale_linewidth_manual(name = "Season", values = size_map) +
  labs(
    title = "USA Weekly Influenza Hospitalization Admissions",
    x = "Week of Season",
    y = "Weekly Total Influenza Admissions (USA)"
  ) +
  scale_x_discrete(breaks = x_breaks_season) +
  scale_y_continuous(limits = c(0, max(seasonal_data$value, na.rm = TRUE) + 10000), expand = c(0, 0)) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave(seasonal_plot_path, plot = p2, width = 10, height = 6, dpi = 300)


# 5. Peak Analysis
# Output: output/data/02_data_explore/peak_description.csv
peak_csv_path <- file.path(output_dir_data, "peak_description.csv")

# Filter to 2025-26 season only
peak_season_data <- current_season_data %>% arrange(week)

# Peak Time and Intensity
peak_idx <- which.max(peak_season_data$value)
peak_time <- peak_season_data$week[peak_idx]
peak_intensity <- peak_season_data$value[peak_idx]

# Decline Start Week (first week after Peak_Time where value < previous week value)
post_peak_data <- peak_season_data %>% filter(week > peak_time)
decline_start <- as.Date(NA)

if (nrow(post_peak_data) > 0) {
  for (i in 1:nrow(post_peak_data)) {
    curr_date <- post_peak_data$week[i]
    curr_val <- post_peak_data$value[i]

    # Get previous week's value in peak_season_data
    prev_week_date <- curr_date - 7
    prev_row <- peak_season_data %>% filter(week == prev_week_date)
    if (nrow(prev_row) > 0) {
      if (curr_val < prev_row$value) {
        decline_start <- curr_date
        break
      }
    }
  }
}

# Season Start and End Week (using Saturday-based dates from the dataset)
season_start <- min(peak_season_data$week)
season_end <- max(peak_season_data$week)

# Format and write CSV
peak_df <- data.frame(
  Peak_Time = peak_time,
  Peak_Intensity = peak_intensity,
  Decline_Start = decline_start,
  Season_Start = season_start,
  Season_End = season_end,
  stringsAsFactors = FALSE
)

write_csv(peak_df, peak_csv_path)

message("Visualization script completed successfully.")
message(paste("National Trend written to", national_plot_path))
message(paste("Seasonal Comparison written to", seasonal_plot_path))
message(paste("Peak Description CSV written to", peak_csv_path))
