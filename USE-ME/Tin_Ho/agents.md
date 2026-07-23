# Agent Instructions

Follow these instructions when working on the NHSN HRD influenza data cleaning and exploration workflow for this workspace.

## Primary goal
Create and maintain the cleaning workflow described in rules.md so the raw CSV is converted into a tidy, three-column dataset and an epicurve figure is produced. Then build an exploratory visualization from the cleaned data.

## Required workflow for cleaning
1. Create or update the R script at scripts/01_cleaning.R.
2. Read the NHSN HRD CSV from the data/ folder with readr::read_csv().
3. Import only the required columns:
   - Week Ending Date
   - Geographic aggregation
   - One influenza admissions column, using either Total.Influenza.Admissions or Total Influenza Admissions
4. Read these columns as character first, then parse them explicitly.
5. Filter to rows where Geographic aggregation is USA.
6. Select the influenza admissions column. If neither allowed column exists, stop with a clear error.
7. Reshape the data into exactly three columns:
   - week
   - location (set to US)
   - value
8. Convert values with readr::parse_number() so comma-formatted counts are parsed correctly.
9. Convert Week Ending Date to an R Date object in week and sort ascending.
10. Write the cleaned data to output/data/01_cleaning/cleaned_flu_admissions.csv.
11. Create an epicurve figure and save it to output/figures/01_cleaning/epicurve_us_flu_admissions.png.

## Validation requirements for cleaning
The cleaning script must stop on failure if any of these checks fail:
- Row count is greater than 0
- Column names are exactly week, location, value in that order
- location is always US
- week inherits class Date
- value is numeric
- value has no missing values after parsing
- The output CSV exists at output/data/01_cleaning/cleaned_flu_admissions.csv
- The epicurve file exists at output/figures/01_cleaning/epicurve_us_flu_admissions.png

## Required workflow for exploratory visualization
1. Create or update the R script at scripts/02_data_explore.R.
2. Read the cleaned data from output/data/01_cleaning/cleaned_flu_admissions.csv.
3. Create a bar plot of all US influenza hospitalizations from the cleaned data.
4. Title the plot "All US Influenza hospitalization".
5. Use green bars, x-axis week, and y-axis hospitalization.
6. Shade the 2025-2026 season with a light pink color.
7. Save the plot to output/figures/02_barplot.png.
8. Use MMWR season logic so the 2025-2026 shading follows the rules in rules.md:
   - A season spans MMWR week 40 through week 20 of the following year.
   - Assign dates in early January with epiweek >= 40 to the previous year's season.
   - Use the season start year and end year naming convention such as 2025-2026.

## Required workflow for forecasting
1. Create or update the R script at scripts/03_forecast.R.
2. Read the cleaned data from output/data/01_cleaning/cleaned_flu_admissions.csv.
3. Validate that week is a Date column, location is character with values of US, and value is numeric.
4. If any column cannot be parsed, stop with the message "The {column} could not be parsed."
5. Use all observed weeks strictly before the first week of the testing period as the fixed initial training set and use an expanding window for later forecasts.
6. Print the training period start date, the rolling window end dates, the testing period start date, each forecast date, and the forecasting horizon.
7. Save the forecast output to output/data/03_forecast/forecast.csv.
8. Save a forecast plot to output/figures/03_forecast/forecast.png.

## Implementation notes
- Use explicit parsing rather than relying on automatic type inference.
- Do not use parse_double() directly on comma-formatted counts.
- Ensure the plotting input is numeric, for example by using as.numeric(value), so barplot() does not fail.
- Keep the workflow tidy, reproducible, and suitable for downstream modeling.
