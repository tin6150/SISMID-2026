# Agent Instructions

This agent must generate `output/scripts/01_cleaning.R` to clean NHSN HRD influenza data and produce a tidy dataset plus an epicurve figure.

## Primary task

- Read the raw CSV from `data/Weekly Hospital Respiratory Data (HRD) Metrics by Jurisdiction.csv`.
- Keep only the required columns:
  - `Week Ending Date`
  - `Geographic aggregation`
  - one influenza admissions column (`Total.Influenza.Admissions` or `Total Influenza Admissions`).
- Filter to rows where `Geographic aggregation` is exactly `USA`.
- Fail clearly if neither allowed influenza admissions column exists.

## Output dataset

- Reshape data into exactly three columns, in this order:
  1. `week`
  2. `location`
  3. `value`
- Set `location` to the constant string `US` for all rows.
- Convert `value` using `readr::parse_number()` to handle comma-formatted counts.
- Convert `Week Ending Date` to an R `Date` object and store it in `week`.
- Sort rows by `week` ascending.
- Save the cleaned table to `output/data/01_cleaning/cleaned_flu_admissions.csv`.

## Epicurve figure

- Create an epicurve plot using the cleaned data.
- Save the figure to `output/figures/01_cleaning/epicurve_us_flu_admissions.png`.
- Plot requirements:
  - X-axis: `week`
  - Y-axis: `value`
  - Ensure the plotting input is numeric (for example, use `as.numeric(value)` if needed).

## Validation checks

The script must include checks that stop execution if any of these fail:

- Row count is greater than 0.
- Column names are exactly `week`, `location`, `value` in that order.
- All `location` values are `US`.
- `week` inherits class `Date`.
- `value` is numeric.
- `value` contains no `NA` values after parsing.
- The output CSV exists at `output/data/01_cleaning/cleaned_flu_admissions.csv`.
- The epicurve file exists at `output/figures/01_cleaning/epicurve_us_flu_admissions.png`.

## Implementation notes

- Use `readr::read_csv()` to read data and import selected columns as character.
- Use explicit parsing and conversion rather than relying on automatic type guessing.
- Do not use `parse_double()` directly on comma-formatted counts.
- The script should be self-contained and should create output directories if needed.