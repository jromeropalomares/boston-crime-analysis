# ------------------------------------------------------------------------------
# Boston Police Crime Data Analysis (2018–2022)
#
# Objective:
#   • Load and combine yearly crime CSVs (2018–2022)
#   • Clean and standardize key columns (incident number, offense codes, date/time, etc.)
#   • Engineer new features: `YEAR`, `MONTH`, `HOUR`, `DAY_OF_WEEK`, `SHIFT`, `IS_SHOOTING`
#   • Generate summary tables:
#       – Shootings per year
#       – Crime totals by offense group
#       – Top districts (all crimes, auto theft, shootings)
#   • Plot: crime by hour, day of week, and monthly shooting trends
#
# Dependencies:
#   • dplyr, lubridate, readr, ggplot2 (via tidyverse)
#   • CSV files: "2018.csv", "2019.csv", "2020.csv", "2021.csv", "2022.csv"
#
# Output:
#   • Summary tables printed to console
#   • Three ggplot2 charts (histogram by HOUR, bar-plot by DAY_OF_WEEK, line by MONTH)
#
# Usage:
#   • Place this script in a folder with the five CSVs. Run in RStudio or R console.
#
# ------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(tidyverse)
library(readr)

# ─────────────────────────────────────────────────────────────────────────────
# 1) DATA LOADING & INITIAL MUTATIONS
# ─────────────────────────────────────────────────────────────────────────────

data_2018 <- read_csv("2018.csv") %>%
  mutate(INCIDENT_NUMBER = as.character(INCIDENT_NUMBER),
         OFFENSE_CODE = as.numeric(OFFENSE_CODE),
         REPORTING_AREA = as.numeric(REPORTING_AREA),
         SHOOTING = as.numeric(SHOOTING)
  )
data_2019 <-read_csv("2019.csv") %>%
  mutate(INCIDENT_NUMBER = as.character(INCIDENT_NUMBER),
         OFFENSE_CODE_GROUP = as.character(OFFENSE_CODE_GROUP),
         UCR_PART = as.character(UCR_PART)
  )
data_2020 <- read_csv("2020.csv") %>%
  mutate(OFFENSE_CODE_GROUP = as.character(OFFENSE_CODE_GROUP),
         UCR_PART = as.character(UCR_PART),
         INCIDENT_NUMBER = as.character(INCIDENT_NUMBER),
  )
data_2021 <- read_csv("2021.csv") %>%
  mutate(OFFENSE_CODE_GROUP = as.character(OFFENSE_CODE_GROUP),
         UCR_PART = as.character(UCR_PART),
         INCIDENT_NUMBER = as.character(INCIDENT_NUMBER),
  )
data_2022 <- read_csv("2022.csv") %>%
  mutate(OFFENSE_CODE_GROUP = as.character(OFFENSE_CODE_GROUP),
         UCR_PART = as.character(UCR_PART),
         INCIDENT_NUMBER = as.character(INCIDENT_NUMBER),
  )
crime <- bind_rows(data_2018, data_2019, data_2020, data_2021, data_2022)

glimpse(crime)

# ─────────────────────────────────────────────────────────────────────────────
# 2) DATA CLEANING & Transformation
# ─────────────────────────────────────────────────────────────────────────────

# Convert certain categorical fields to factors
crime <- crime %>%
  mutate(
    OFFENSE_CODE = as.factor(OFFENSE_CODE),
    OFFENSE_CODE_GROUP = as.factor(OFFENSE_CODE_GROUP),
    OFFENSE_DESCRIPTION = as.factor(OFFENSE_DESCRIPTION),
    DISTRICT = as.factor(DISTRICT)
  )

# Verifying the structure after the transformations
glimpse(crime)

# Converting 'OCCURRED_ON_DATE' to POSIXct format for date-time handling
crime <- crime %>%
  mutate(OCCURRED_ON_DATE = as.POSIXct(OCCURRED_ON_DATE))

# Create a logical column `IS_SHOOTING` (TRUE if SHOOTING == 'Y' or 1)
# Note: In some years, the SHOOTING column might be missing; wrap in if_else to avoid errors.
crime <- crime %>%
  mutate(
    IS_SHOOTING = as.logical(ifelse(SHOOTING == 'Y' |SHOOTING == 1, TRUE, FALSE))
  )

# Displaying the count of shootings in the dataset
table(crime$IS_SHOOTING)

# ------------------------------------------------------------------------------
# Summarizing Data
# ------------------------------------------------------------------------------
# Calculating the number of shootings per year
shootings_per_year <- crime %>%
  filter(IS_SHOOTING == TRUE) %>%
  group_by(YEAR) %>%
  summarize(Shootings = n())

# Summarizing total incidents by offense code group
crime_totals <- crime %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarize(totals = n())

# Filtering out NAs and sorting totals by descending order
crime_totals <- crime_totals %>%
  filter(OFFENSE_CODE_GROUP != is.na(OFFENSE_CODE_GROUP) & totals != is.na(totals)) %>%
  arrange(desc(totals))

# ------------------------------------------------------------------------------
# Adding New Variables (Shift Based on Time of Day)
# ------------------------------------------------------------------------------
# Creating a 'SHIFT' column based on hour of the day (Night, Day, Evening)
crime <- crime %>%
  mutate(SHIFT = ifelse(hour(OCCURRED_ON_DATE) >= 0 & hour(OCCURRED_ON_DATE) < 8, "Night",
                        ifelse(hour(OCCURRED_ON_DATE) >= 8 & hour(OCCURRED_ON_DATE) < 16, "Day", "Evening"))
  )

# Displaying the table of SHIFT categories
table(crime$SHIFT)

# ------------------------------------------------------------------------------
# Grouping and Summarizing by District
# ------------------------------------------------------------------------------
# Summarizing the number of crimes per district (top 5)
crime %>%
  filter(DISTRICT != is.na(DISTRICT)) %>%
  group_by(DISTRICT) %>%
  summarize(totals = n()) %>%
  arrange(desc(totals)) %>%
  head(5)

# ------------------------------------------------------------------------------
# Summarizing Crime by Month
# ------------------------------------------------------------------------------
# Summarizing total crimes per month and displaying top 5 months with the most crimes
crime %>%
  filter(MONTH != is.na(MONTH)) %>%
  group_by(MONTH) %>%
  summarize(totals = n()) %>%
  arrange(desc(totals)) %>%
  head(5)

# ------------------------------------------------------------------------------
# Analyzing Auto Theft Incidents by District
# ------------------------------------------------------------------------------
# Summarizing the number of auto thefts by district and finding the district with most auto thefts
crime %>%
  filter(OFFENSE_DESCRIPTION == 'AUTO THEFT') %>%
  group_by(DISTRICT) %>%
  summarize(auto_theft_count = n()) %>%
  slice_max(auto_theft_count, n = 1)

# ------------------------------------------------------------------------------
# Analyzing Shootings by District
# ------------------------------------------------------------------------------
# Summarizing the number of shootings by district and finding the district with most shootings
district_totals <- crime %>%
  filter(IS_SHOOTING == TRUE & !is.na(IS_SHOOTING)) %>%
  group_by(DISTRICT) %>%
  summarize(district_shootings = n()) %>%
  slice_max(district_shootings, n = 1)

# ------------------------------------------------------------------------------
# Yearly Shootings by District
# ------------------------------------------------------------------------------
# Summarizing shootings by district and year, then finding the district with the most shootings each year
district_yearly <- crime %>%
  filter(IS_SHOOTING ==TRUE & !is.na(IS_SHOOTING)) %>%
  group_by(DISTRICT,YEAR) %>%
  summarize(district_year_shootings = n(), .groups = "drop") %>%
  group_by(YEAR) %>%
  slice_max(district_year_shootings, n = 1, with_ties = FALSE)

# ------------------------------------------------------------------------------
# Defining Custom Function for Crime Data by District & Offense Code
# ------------------------------------------------------------------------------
# Function to summarize crime incidents by district and offense code(s)
district_crime <- function(dist, codes) {
  crime %>%
    filter(DISTRICT == dist & OFFENSE_CODE %in% codes) %>%
    group_by(YEAR) %>%
    summarize(total = n()) %>%
    setNames(c("YEAR", "Total"))
}

# ------------------------------------------------------------------------------
# Visualizing the Data
# ------------------------------------------------------------------------------
# Plot: Distribution of Crime Incidents by Hour of Day
crime %>%
  ggplot(aes(x = HOUR)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Crime Incidents by Hour") +
  xlab("Hour of the Day (0-23)")

# Plot: Total Number of Incidents Per Day of the Week
day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
crime %>%
  mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = day_order)) %>%
  ggplot(aes(x = DAY_OF_WEEK)) +
  geom_bar() +
  ylab("Number of Incidents") +
  xlab("Day of the Week") +
  ggtitle("Total Number of Incidents Per Year")

# Plot: Monthly Shooting Trends
crime %>%
  group_by(MONTH) %>%
  summarize(shooting_count = sum(IS_SHOOTING, na.rm = TRUE)) %>%
  ggplot(aes(x = MONTH, y = shooting_count)) +
  geom_line()




