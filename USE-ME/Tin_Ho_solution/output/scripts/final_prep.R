#!/usr/bin/env Rscript

# final_prep.R - Final FluSight Submission Preparation
# Following instructions in rules.md

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Setup paths
xgb_forecasts_csv <- "output/data/03_forecast/XGB_flusight_forecasts.csv"
submission_dir <- "output/data/final_flusight_submission"

# 1. Input and validation of columns
if (!file.exists(xgb_forecasts_csv)) {
  stop("Input forecast file not found: ", xgb_forecasts_csv)
}

# Expected columns
expected_cols <- c("reference_date", "target", "horizon", "target_end_date", "location", "output_type", "output_type_id", "value")

# Load forecasts
fc <- tryCatch({
  read_csv(xgb_forecasts_csv, col_types = cols(
    reference_date = col_date(),
    target = col_character(),
    horizon = col_integer(),
    target_end_date = col_date(),
    location = col_character(),
    output_type = col_character(),
    output_type_id = col_double(),
    value = col_double()
  ))
}, error = function(e) {
  stop("The forecasts could not be parsed.")
})

# Validate that all expected columns are present
if (!all(expected_cols %in% names(fc))) {
  stop("Validation failed: missing required FluSight columns in forecast input.")
}

# 2. Output folder creation & validation
if (!grepl("final_flusight_submission$", submission_dir)) {
  stop("Validation failed: output directory path must end in 'final_flusight_submission'.")
}
dir.create(submission_dir, recursive = TRUE, showWarnings = FALSE)


# 3. Split and write
unique_ref_dates <- unique(fc$reference_date)
n_ref_dates <- length(unique_ref_dates)

total_rows_written <- 0
written_files <- character()

# Clean existing submission files first to ensure a clean validation
existing_files <- list.files(submission_dir, pattern = "-AmandaXGBoost\\.csv$", full.names = TRUE)
if (length(existing_files) > 0) {
  file.remove(existing_files)
}

for (ref_date in unique_ref_dates) {
  ref_date_obj <- as.Date(ref_date, origin = "1970-01-01")
  ref_date_str <- format(ref_date_obj, "%Y-%m-%d")

  # Filter and sort
  sub_df <- fc %>%
    filter(reference_date == ref_date_obj) %>%
    arrange(horizon, output_type_id) %>%
    select(all_of(expected_cols))

  # Set target filename
  filename <- paste0(ref_date_str, "-AmandaXGBoost.csv")
  file_path <- file.path(submission_dir, filename)

  # Write file
  write_csv(sub_df, file_path)

  # --- Validations per file ---

  # Hard validation: filename format
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}-AmandaXGBoost\\.csv$", filename)) {
    stop("Validation failed: Emitted filename does not match pattern: ", filename)
  }

  # Hard validation: file exists on disk
  if (!file.exists(file_path)) {
    stop("Validation failed: File was not successfully written to disk: ", file_path)
  }

  # Hard validation: exactly 69 rows and single unique reference_date
  row_count <- nrow(sub_df)
  if (row_count != 69) {
    stop("Validation failed: File ", filename, " does not contain exactly 69 rows (contains ", row_count, ").")
  }

  unique_ref_in_file <- unique(sub_df$reference_date)
  if (length(unique_ref_in_file) != 1 || unique_ref_in_file != ref_date_obj) {
    stop("Validation failed: File ", filename, " contains incorrect or multiple reference dates.")
  }

  # Hard validation: columns are exactly the eight FluSight columns, in order
  if (!all(names(sub_df) == expected_cols)) {
    stop("Validation failed: File ", filename, " does not match exactly the expected column structure/ordering.")
  }

  # Track status
  total_rows_written <- total_rows_written + row_count
  written_files <- c(written_files, file_path)

  # Print one line per written file
  cat(paste0(ref_date_str, " -> ", filename, ", ", row_count, " rows\n"))
}


# --- Summary Validations ---

# Hard validation: file count equals number of distinct reference dates
if (length(written_files) != n_ref_dates) {
  stop("Validation failed: file count written (", length(written_files), ") does not equal the number of distinct reference dates (", n_ref_dates, ").")
}

# Hard validation: summed rows equals source file's row count
if (total_rows_written != nrow(fc)) {
  stop("Validation failed: summed rows across all written files (", total_rows_written, ") does not equal source file's row count (", nrow(fc), ").")
}

# Print closing summary
cat("\n====================================================================\n")
cat("Final Submission Preparation Summary:\n")
cat("====================================================================\n")
cat("Number of submission files written: ", length(written_files), "\n")
cat("Total forecast rows written:        ", total_rows_written, "\n")
cat("Submission directory ends in:      ", basename(submission_dir), "\n")
cat("====================================================================\n\n")

message("Submission prep script completed successfully.")
