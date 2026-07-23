# Data Cleaning Rules

These rules define how the agent should generate `output/scripts/01_cleaning.R`. Follow each
step in order. The goal is to turn the raw NHSN HRD influenza file into a tidy,
three-column dataset ready for downstream modeling and produce an epicurve
figure.

## 1. Load the data

Read the NHSN HRD CSV from the `data/` folder using **readr** (`read_csv()`).
To avoid parsing warnings from unrelated fields, import only required columns:

- `Week Ending Date`
- `Geographic aggregation`
- Influenza admissions column (see Rule 3)

Import these columns as character first, then parse/convert explicitly.

## 2. Filter to US only

Use the `Geographic aggregation` column. Keep only the rows where the value is
`"USA"`.

## 3. Select the target column

Use influenza admissions from one of these allowed column names:

- `Total.Influenza.Admissions`
- `Total Influenza Admissions`

Fail with a clear error if neither exists.

## 4. Reshape to three columns

Rename and restructure the data to exactly three columns:

- `week`
- `location` — set to `"US"`
- `value`

Convert `value` with `readr::parse_number()` so values like `1,110` are valid.
Do not use `parse_double()` directly on comma-formatted counts.

## 5. Format dates

Convert `Week Ending Date` to an R `Date` object in `week`. Sort ascending by
`week`.

## 6. Save the output

Write the cleaned data to `cleaned_flu_admissions.csv` in the `output/data/01_cleaning` folder.

## 7. Generate epicurve figure

Create an epicurve from the cleaned data and save to:

- `output/figures/01_cleaning/epicurve_us_flu_admissions.png`

Plot requirements:

- X-axis: `week`
- Y-axis: `value`
- Ensure plotting input is a numeric vector (for example `as.numeric(value)`) so
	`barplot()` does not fail with height-type errors.

## 8. Required validation checks

The script must include checks that stop execution on failure:

- Row count is greater than 0
- Column names are exactly `week`, `location`, `value` in that order
- `location` is always `"US"`
- `week` inherits class `Date`
- `value` is numeric
- `value` has no `NA` values after parsing
- Output CSV exists at `output/data/01_cleaning/cleaned_flu_admissions.csv`
- Epicurve file exists at `output/figures/01_cleaning/epicurve_us_flu_admissions.png`



# Visualizations 
These rules define how the agent should generate `output/scripts/02_data_explore.R.`.


## National Plot

- create a bar plot
- plot all hospitlization from `output/data/01_cleaning/cleaned_flu_admissions.csv`
- title the plot "All US Influenza hospitalization"
- 
- color: green
- x-axis: week
- y-axis: hospitalization
- shade the 2025-2026 season with a light pink color
- save plot to `output/figures/02_barplot.png`

## Season definition

- Time span: MMWR week 40 (≈ early October) through week 20 (≈ mid-May) of the next year. A season spans two calendar years and is named for both — e.g. 2025-2026.
- Season start: First calendar date of first year with Epiweek = 40. This is a fixed calendar week, the same every season, so the agent sets it directly rather than detecting it from the data. Please account for any leap years in this determination.
- Season end: Earliest calendar date in the second year with Epiweek = 20.

- When mapping calendar dates to MMWR epi-weeks, handle week numbers that fall in early January (for example epi-week 53) by using the calendar month to assign the season start year. Concretely, if a date's epiweek is >= 40 but its calendar month is January–August, attribute that date to the previous year's season (i.e., season_start_year = year - 1).


## Forcasting

create or update forecasting script as `output/scripts/03_forecast.R`
If the folder pathway is not present, create the folder pathway
prior to saving the script.

Read in the input data set from
`output/data/01_cleaning/cleaned_flu_admissions.csv`.


## 2. Data input and validation

Ensure the column names and column types/formats match the
following:
- `week`: Date column, YYYY-MM-DD
- `location`: Character; all values should be "US"
- `value`: Numeric
If any columns have violations please first try to parse to the
correct format, and then notify the user. If any columns can not
be parsed, please return the following error:
'The {column} could not be parsed.'


Training period:
Fit the model on all observed weeks
strictly before the first week of the
testing period present in the data. Use
these rows as the fixed initial training
set; subsequent forecasts will use an
‘expanding window’.


**Validations:** Print the `Training Period` start date and rolling window end dates.
**Validations:** Print the `Testing Period` start date, and each forecast date.
**Validations:** Print the specified forecasting horizon that will be used.

