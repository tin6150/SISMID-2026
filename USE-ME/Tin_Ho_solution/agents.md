# Agent Instructions

This document defines the rules and steps for an autonomous agent to execute the respiratory hospitalization analysis pipeline. Follow each section in sequence.

---

## 1. Data Cleaning (`01_cleaning.R`)

Generate a clean, three-column dataset and an epicurve figure from the raw NHSN HRD influenza file.

### Task Specifications
- **Input**: `data/Weekly Hospital Respiratory Data (HRD) Metrics by Jurisdiction.csv`
- **Output CSV**: `output/data/01_cleaning/cleaned_flu_admissions.csv`
- **Output Plot**: `output/figures/01_cleaning/epicurve_us_flu_admissions.png`

### Pipeline Steps
1. **Load data**: Read specified columns as character (`Week Ending Date`, `Geographic aggregation`, and either `Total.Influenza.Admissions` or `Total Influenza Admissions`).
2. **Filter to US**: Retain rows where `Geographic aggregation` is `"USA"`.
3. **Select target**: Extract the target column, or fail with a clear error if neither is found.
4. **Reshape and Clean**: Name columns exactly `week`, `location` (value is `"US"`), and `value` (numeric parsed with `readr::parse_number()`).
5. **Format & Sort**: Parse `week` to an R `Date` and sort ascending.
6. **Save**: Export the CSV and generate an epicurve figure using `barplot()`.

### Validations
Halt execution if any of the following checks fail:
- Row count > 0.
- Column names are exactly `week`, `location`, `value` in order.
- `location` is always `"US"`.
- `week` inherits class `Date`.
- `value` is numeric, non-negative, and has no `NA`s.
- Cleaned CSV and epicurve PNG are successfully written on disk.

---

## 2. Visualization (`02_data_explore.R`)

Parse the cleaned admissions, compute season mapping, and generate trend comparisons and peak metrics.

### Task Specifications
- **Input**: `output/data/01_cleaning/cleaned_flu_admissions.csv`
- **Output Figures**:
  - `output/figures/02_data_explore/national_trend.png` (300 DPI)
  - `output/figures/02_data_explore/seasonal_comparison.png` (300 DPI)
- **Output CSV**: `output/data/02_data_explore/peak_description.csv`

### Season Mapping rules
- A season spans MMWR week 40 through week 20 of the following year, labeled in `YYYY-YY` format.
- Let `year` be calendar year of `week`. If `epiweek >= 40` and calendar month is January-August, then `season_start_year = year - 1`. If `epiweek <= 20`, then `season_start_year = year - 1`.
- Print `Season Start Week` and `Season End Week` for current season (`2025-26`).

### Figure Specifications
- **National Trend**:
  - Line plot of all observed weeks in blue.
  - Lightgrey background bar highlighting the `2025-26 Season` with a centered, bold label at the top.
  - X-axis format `MM-YYYY`, showing every 6th date, rotated 45 degrees.
  - Y-axis range from 0 to `max(value) + 10000`.
- **Seasonal Comparison**:
  - Show points only for MMWR weeks 40-20.
  - Plot weeks 40-53 followed by weeks 1-20 chronologically on a shared x-axis using `factor(epi_week, levels = c(40:53, 1:20))`.
  - Unified legend titled **Season** mapping color, linetype, and thickness (linewidth) to each season. The current season `2025-26` is solid black and thicker.

### Peak & Decline Analysis
- For the `2025-26` season:
  - `Peak_Time`: Date of global maximum.
  - `Peak_Intensity`: Maximum value.
  - `Decline_Start`: First date after `Peak_Time` on which the value is strictly less than the previous week's value.
  - Save `Peak_Time`, `Peak_Intensity`, `Decline_Start`, `Season_Start`, and `Season_End` to `peak_description.csv`.

---

## 3. Forecasting (`03_forecast.R`)

Perform multi-horizon rolling ARIMA forecasting and produce quantiles in FluSight format.

### Task Specifications
- **Input**: `output/data/01_cleaning/cleaned_flu_admissions.csv`
- **Output CSV**: `output/data/03_forecast/flusight_forecasts.csv`
- **Output Figure**: `output/figures/03_forecast/forecast_vs_observed.png` (300 DPI)

### Forecast Generation Rules
- **Reference Dates**: Every observed week `r` such that target `r + 7` falls in testing period (`2025-26` season) and is present in data.
- **Model fitting**: For each reference date, fit a non-seasonal ARIMA model on plain numeric values for `week <= r` exactly once.
- **Forecast Generation**: Forecast 3 horizons ahead (`h = 1, 2, 3`) for 23 standard central-interval quantile levels: `c(98, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10)`.
- **Formatting**: Clamp forecast values at 0 (`pmax(value, 0)`) and round to nearest integer. Reshape to FluSight long format.

### Validations
Halt execution if any of the following checks fail:
- Sorted training window with no duplicate, missing, or skipped weeks.
- Non-constant series before fitting.
- Quantiles non-decreasing within horizons and symmetric on raw forecasts.
- Emitted forecast intervals widen (or stay equal) with horizon: `[val] intervals widen with horizon: OK`.
- Target end dates are exactly `reference_date + 7 * horizon`: `[val] target dates correct: OK`.
- One model fit per reference date, emitting exactly horizons 1, 2, 3: `[val] one fit, three horizons: OK`.
- Value entries are non-negative integers: `[val] quantiles non-decreasing: OK`.
- Emitted output contains exactly the 23 FluSight quantile levels: `[val] all quantile levels present: OK`.
- Point forecast is centered: `[val] median centered, quantiles symmetric: OK`.

### Forecast Overlay Figure
- Observed admissions over `2025-26` season in black.
- Horizon medians plotted with color-coded lines and points (e.g. h1 = blue, h2 = orange, h3 = green).
- Shaded 95% PI ribbons behind the median lines matching horizon colors.
- X-axis weekly spacing, showing every 4th label, rotated 45 degrees.
- Unified legend combining Observed, and all horizon lines and PI bands.

---

## 4. Forecast Evaluation (`04_evaluation.R`)

Evaluate the accuracy of the rolling ARIMA forecasts using the `scoringutils` package.

### Task Specifications
- **Input Forecasts**: `output/data/03_forecast/flusight_forecasts.csv`
- **Input Observed**: `output/data/01_cleaning/cleaned_flu_admissions.csv`
- **Output Script**: `output/scripts/04_evaluation.R`

### Evaluation Rules
1. **Join Tables**: Merge forecast and observed data on common keys (`target_end_date == week` and `location`).
2. **Filter & Align**: Drop any forecasts that do not have a matching observed value (e.g., 2- and 3-week-ahead forecasts that extend beyond the observed data).
3. **Drop Notice**: Print a console message reporting the number of `(reference_date, horizon)` combinations dropped due to missing observed targets.
4. **Scoring**: Cast the data to `forecast_quantile` type using `scoringutils::as_forecast_quantile()` and run the `score()` function to compute metrics including Weighted Interval Score (WIS), bias, overprediction, underprediction, and interval coverages.
5. **Reporting**: Print a summary of metrics grouped by forecast horizon to the terminal.
