# Boston Police Crime Data Analysis (2018–2022)

## Required Libraries
- `dplyr`
- `lubridate`
- `readr`
- `ggplot2`

## Overview
- Combines and cleans five years of Boston Police crime data (CSV format).
- Engineers date/time features (Year, Month, Hour, Day of Week, Shift).
- Computes summaries (shootings per year, top districts, etc.).
- Visualizes crime distribution by hour, day of week, and monthly shooting trends.

## How to Run
1. Install required R packages: `dplyr`, `readr`, `lubridate`, `ggplot2`.  
2. Place `2018.csv` through `2022.csv` in the same directory as `boston_crime_analysis.R`.  
3. Open `boston_crime_analysis.R` in RStudio and source/Run all lines.  
4. View printed summary tables and plots.

## Data Source
- [Boston Police Department Open Data](https://www.boston.gov/departments/boston-police/open-data)
- Each CSV contains all reported incidents for a calendar year (2018–2022).

## Key Findings
- Districts B2, D4, C11, A1, B3 had the highest crime rates.
- District C11 had the most auto thefts.
- District B3 had the most shootings.
- In 2019 and 2020, District B2 had the most shootings; however, in 2021 and 2022, District B3 had the most shootings.
- More crime was reported during the day than in the evening.

## Author
- Julissa Romero (Data Analytics Intern Candidate)

