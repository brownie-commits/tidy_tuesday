# 4/20 Traffic Fatality Analysis - Data Cleaning
# Analysis of traffic fatalities on 4/20 vs control dates
# Based on Harper and Palayew (2019) methodology

#### Load packages -------------------------------------------------------------

library(haven)
library(tidyverse)
library(fs)
library(lubridate)

#### Acquire raw data ----------------------------------------------------------

# Crash data (from Harper and Palayew)
# 4/20 Traffic Fatality Analysis - Data Cleaning
# Analysis of traffic fatalities on 4/20 vs control dates
# Based on Harper and Palayew (2019) methodology

# Crash data (from Harper and Palayew)
dl_path <- withr::local_tempfile(fileext = ".zip")

download.file("https://osf.io/kj7ub/download", dl_path, mode = "wb")

unzip_path <- withr::local_tempdir()

unzip(dl_path, exdir = unzip_path)

dta_files <- fs::dir_ls(unzip_path, glob = "*.dta")

fars <- purrr::map(dta_files, haven::read_dta) |> 
  purrr::list_rbind(names_to = "id") |> 
  dplyr::mutate(id = basename(id))

# Geographic lookup

geog <- readr::read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = c(
    "state_name",
    "state_code",
    "county_code",
    "county_name",
    "FIPS_class_code"
  )
) |> 
  dplyr::mutate(
    state = as.numeric(state_code),
    count = as.numeric(county_code),
    FIPS = paste0(state_code, county_code)
  )

#### Data wrangling ------------------------------------------------------------

# Process all accidents - filter to drivers only
all_accidents <- fars |> 
  dplyr::select(
    "id", 
    "state", 
    "county", 
    "month", 
    "day", 
    "hour",
    "minute", 
    "st_case",
    "per_no",
    "veh_no",
    "per_typ", 
    "age",
    "sex", 
    "death_da",
    "death_mo",
    "death_yr",
    "death_hr", 
    "death_mn",
    "death_tm",
    "inj_sev", 
    "mod_year",
    "lag_hrs",
    "lag_mins"
  ) |> 
  dplyr::mutate(
    # Code 99 represents missing data
    dplyr::across(
      c("month", "day", "hour", "minute"),
      ~ na_if(.x, 99)
    ),
    year = readr::parse_number(id)
  ) |> 
  dplyr::filter(
    per_typ == 1,  # Drivers only
    !is.na(year),
    !is.na(month),
    !is.na(day)
  ) |> 
  dplyr::mutate(
    # Create date variable (CRITICAL FIX)
    date = as.Date(paste(year, month, day, sep = "-")),
    
    # Time variables
    crashtime = hour * 100 + minute,
    time = paste(hour, minute, sep = ":"),
    timestamp = as.POSIXct(
      paste(date, time),
      format = "%Y-%m-%d %H:%M"
    ),
    
    # Day of week for controlling day-of-week effects
    dow = wday(date, label = TRUE),
    
    # Treatment indicators
    is_420 = (month == 4 & day == 20),
    is_evening = (crashtime >= 1620 & crashtime <= 2359),
    is_420_evening = is_420 & is_evening,
    
    # Control weeks (week before and after 4/20)
    is_control_week = (month == 4 & day %in% c(13, 27)),
    is_control_evening = is_control_week & is_evening,
    
    # Demographic variables
    sex = factor(
      dplyr::case_when(
        sex == 2 ~ "F",
        sex == 1 ~ "M",
        .default = NA_character_
      )
    ),
    period = factor(
      dplyr::case_when(
        year < 2004  ~ "Remote (1992-2003)",
        year >= 2004 ~ "Recent (2004-2016)",
        .default = NA_character_
      )
    ),
    age_group = factor(
      dplyr::case_when(
        age <= 20 ~ "<20y",
        age <= 30 ~ "21-30y",
        age <= 40 ~ "31-40y",
        age <= 50 ~ "41-50y",
        age <= 97 ~ "51-97y",
        .default = NA_character_
      )
    )
  )

#### Create analysis datasets --------------------------------------------------

# Dataset 1: Daily fatalities (all times, all days)
# Use this for overall trends and time series
daily_fatalities_all <- all_accidents |> 
  dplyr::filter(year > 1991) |> 
  dplyr::summarize(
    total_fatalities = n(),
    .by = c(date, year, month, day, dow)
  )

# Dataset 2: Daily fatalities with treatment indicators
# Use this for main analysis comparing 4/20 to control dates
daily_fatalities_treatment <- all_accidents |> 
  dplyr::filter(year > 1991) |> 
  dplyr::summarize(
    total_fatalities = n(),
    evening_fatalities = sum(is_evening),
    fatalities_420_evening = sum(is_420_evening),
    .by = c(date, year, month, day, dow, is_420, is_control_week)
  )

# Dataset 3: Evening-only fatalities for focused analysis
# Use this for the specific 4:20pm-midnight comparison
evening_fatalities <- all_accidents |> 
  dplyr::filter(year > 1991, is_evening) |> 
  dplyr::summarize(
    evening_fatalities = n(),
    .by = c(date, year, month, day, dow, is_420, is_control_week)
  )

# Dataset 4: April dates only (treatment and control)
# Use this for regression models with matched controls
april_analysis <- all_accidents |> 
  dplyr::filter(
    year > 1991,
    month == 4,
    day %in% c(13, 20, 27)  # Treatment date and same-day-of-week controls
  ) |> 
  dplyr::summarize(
    total_fatalities = n(),
    evening_fatalities = sum(is_evening),
    .by = c(date, year, month, day, dow, is_420)
  )

#### Data quality checks -------------------------------------------------------

# Check for missing time data
missing_summary <- all_accidents %>%
  summarise(
    missing_hour = sum(is.na(hour)),
    missing_minute = sum(is.na(minute)),
    missing_date = sum(is.na(date)),
    total_records = n(),
    pct_missing_hour = round(100 * missing_hour / total_records, 2)
  )

print("Missing data summary:")
print(missing_summary)

# Verify treatment assignment
treatment_summary <- daily_fatalities_treatment %>%
  filter(is_420 | is_control_week) %>%
  group_by(is_420, dow) %>%
  summarise(
    n_days = n(),
    mean_evening_fatalities = mean(evening_fatalities),
    .groups = "drop"
  )

print("Treatment and control summary:")
print(treatment_summary)

# Save cleaned data
save(
  daily_fatalities_all,
  daily_fatalities_treatment,
  evening_fatalities,
  april_analysis,
  all_accidents,
  file = "cleaned_420_data.RData"
)

print("Data cleaning complete! Datasets saved to 'cleaned_420_data.RData'")