library(readr)
library(dplyr)

input_path <- "data/Weekly Hospital Respiratory Data (HRD) Metrics by Jurisdiction.csv"
output_csv_path <- "output/data/01_cleaning/cleaned_flu_admissions.csv"
epicurve_path <- "output/figures/01_cleaning/epicurve_us_flu_admissions.png"

# Ensure output directories exist
for (dir_path in c(dirname(output_csv_path), dirname(epicurve_path))) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
}

# Read the CSV header to discover the available influenza admissions column
header_data <- read_csv(input_path, n_max = 0, col_types = cols(.default = col_character()))
available_columns <- names(header_data)
allowed_columns <- c("Total.Influenza.Admissions", "Total Influenza Admissions")
target_col <- intersect(allowed_columns, available_columns)

if (length(target_col) == 0) {
  stop("Neither Total.Influenza.Admissions nor Total Influenza Admissions exists in the input data.")
}

target_col <- target_col[1]

# Import only the required columns as character first
required_cols <- c("Week Ending Date", "Geographic aggregation", target_col)
data_raw <- read_csv(
  input_path,
  col_select = all_of(required_cols),
  col_types = cols(.default = col_character())
)

# Rename for easier processing
names(data_raw) <- c("week_raw", "location_raw", "value_raw")

# Filter to US rows and prepare the tidy output
cleaned_data <- data_raw %>%
  filter(location_raw == "USA") %>%
  mutate(
    week = readr::parse_date(week_raw, format = "%Y-%m-%d"),
    value = readr::parse_number(value_raw),
    location = "US"
  ) %>%
  select(week, location, value) %>%
  arrange(week)

# Validation checks
if (nrow(cleaned_data) <= 0) {
  stop("No rows remained after filtering to USA.")
}

if (!identical(names(cleaned_data), c("week", "location", "value"))) {
  stop("Column names are not exactly week, location, value in that order.")
}

if (any(cleaned_data$location != "US")) {
  stop("location contains values other than US.")
}

if (!inherits(cleaned_data$week, "Date")) {
  stop("week does not inherit class Date.")
}

if (!is.numeric(cleaned_data$value)) {
  stop("value is not numeric.")
}

if (anyNA(cleaned_data$value)) {
  stop("value contains missing values after parsing.")
}

# Save cleaned data
readr::write_csv(cleaned_data, output_csv_path)

# Create epicurve figure
png(filename = epicurve_path, width = 1600, height = 900, res = 200)
barplot(
  height = as.numeric(cleaned_data$value),
  names.arg = format(cleaned_data$week, "%Y-%m-%d"),
  main = "US Influenza Admissions by Week",
  xlab = "Week",
  ylab = "Influenza admissions",
  col = "steelblue",
  las = 2
)
dev.off()

# Final output existence checks
if (!file.exists(output_csv_path)) {
  stop("Output CSV was not created.")
}

if (!file.exists(epicurve_path)) {
  stop("Epicurve PNG was not created.")
}

message("Cleaning script completed successfully.")
message(paste("Cleaned CSV written to", output_csv_path))
message(paste("Epicurve written to", epicurve_path))
