# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(purrr)
library(janitor)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)
source("funcs.R")


# census key
census_key <- readRDS("census_key.rds")

# population data
url <- "https://api.census.gov/data/2021/pep/population"
request <- request(url) |> req_url_query(get = "POP_2020,POP_2021,NAME",  
                                         `for` = "state:*",
                                         key = census_key
                                         )

response <-  request |> req_perform()

population <- response |> resp_body_json(simplifyVector = TRUE) 
population <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =NAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = str_remove(year, "POP_")) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )

# regions
url<-"https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions<-fromJSON(url,simplifyDataFrame=FALSE)
regions<-map_df(regions, function(x)
  data.frame(region= x$region,region_name= x$region_name,state_name= x$states)) |>
  mutate(region_name = ifelse(region ==2,"NJ,NY,PR,VI",region_name))
population <-left_join(population, regions, by = "state_name")





# Q4
# Checking structure for state abbreviation column and date columns
glimpse(cases_raw)
glimpse(hosp_raw)
glimpse(deaths_raw)
glimpse(vax_raw)

# Check unique values in date columns
unique_dates_sorted <- sort(unique(cases_raw$end_date))
head(unique_dates_sorted, 10)
unique_dates_sorted <- sort(unique(hosp_raw$collection_date))
head(unique_dates_sorted, 10)
unique_dates_sorted <- sort(unique(deaths_raw$week_ending_date))
head(unique_dates_sorted, 10)
unique_dates_sorted <- sort(unique(vax_raw$date))
head(unique_dates_sorted, 10)

# Get column names for easier inspection
names(cases_raw)   # end_date          state
names(hosp_raw)   # collection_date    jurisdiction
names(deaths_raw) # week_ending_date    state 
names(vax_raw)     # date               location



# Define the URLs for each dataset
cases_url <- "https://data.cdc.gov/resource/pwn4-m3yp.json"
hospitalizations_url <- "https://data.cdc.gov/resource/39z2-9zu6.json"
deaths_url <- "https://data.cdc.gov/resource/r8kw-7aab.json"
vaccinations_url <- "https://data.cdc.gov/resource/rh2h-3yt2.json"

# Download the datasets using get_cdc_data()
cases_raw <- get_cdc_data(cases_url)
hosp_raw <- get_cdc_data(hospitalizations_url)
deaths_raw <- get_cdc_data(deaths_url)
vax_raw <- get_cdc_data(vaccinations_url)



# Wrangle the cases data to prepare it for analysis
# cases
cases <- cases_raw |>
    # parse case number and date, change column names 
    mutate(       
    cases = parse_number(new_cases),
    date = as_date(ymd_hms(end_date))
  ) |>
  # only include states for we have info about population data
  filter(state %in% population$state) |>
  # change format of date, to epiweek/epiyear and store the variables.
  mutate(
    mmwr_week = epiweek(date),
    mmwr_year = epiyear(date)
  ) |>
  # Keep what we need and arrange
  select(state, mmwr_year, mmwr_week, cases) |>
  arrange(state, mmwr_year, mmwr_week)



# hosp
hosp <- hosp_raw |> filter(jurisdiction %in% population$state) |> # filter states
  filter(!is.na(new_covid_19_hospital) ) |>  # filter NA
  rename(hosp = new_covid_19_hospital, state = jurisdiction) |>   #  rename
  mutate(hosp = as.numeric(hosp), 
         date = as_date(ymd_hms (collection_date))) |>         # fix format, names
  mutate (mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |>
  select(mmwr_week, mmwr_year, hosp, state) |> 
  group_by(state, mmwr_week, mmwr_year) |>          # only keep entire full weeks
  summarize (hosp = sum(hosp), n= n(), .groups= "drop") |>
  filter(n == 7) |>
  arrange (state, mmwr_year, mmwr_week)

hosp |> ggplot( aes(mmwr_week, hosp, color = state))+
  geom_line( show.legend = FALSE) + facet_wrap(~mmwr_year)


# death

deaths <- deaths_raw |>
  # Filter rows to keep only states that exist in the population dataset
  filter(state %in% population$state_name) |>
#  filter(!is.na(covid_19_deaths) ) |>
  # Convert full state names in `state_name` to abbreviations using `state.abb` and `state.name`
  mutate(state = state.abb[match(state, state.name)]) |>
  # Handle special cases for District of Columbia (DC) and Puerto Rico (PR)
  mutate(
    state = case_when(
      state == "District of Columbia" ~ "DC",
      state == "Puerto Rico" ~ "PR",
      TRUE ~ state
    )
  ) |>
  # Convert `week_ending_date` to a Date format using `ymd_hms` from lubridate
  mutate(date = as_date(ymd_hms(week_ending_date))) |>
  # Create new columns for MMWR week and year using lubridate functions `epiweek()` and `epiyear()`
  mutate(
    mmwr_week = epiweek(date),
    mmwr_year = epiyear(date)
  ) |>
  # Select relevant columns: state, MMWR year, MMWR week, and deaths
  select(state, mmwr_year, mmwr_week, covid_19_deaths) |>
  rename(deaths = covid_19_deaths) |>
  # Arrange by state, MMWR year, and week for easier analysis
  arrange(state, mmwr_year, mmwr_week)
#na_count <- deaths |> summarize(na_state = sum(is.na(state)))

# vax
# Wrangle the vaccination data to keep state, MMWR year, MMWR week, and total weekly vaccinations
vax <- vax_raw |>
  # Filter rows to keep only states that have population estimates available
  filter(location %in% population$state) |>
  # Rename the `location` column to `state` for consistency
  rename(state = location) |>
  mutate(
    state = case_when(
      state == "District of Columbia" ~ "DC",
      state == "Puerto Rico" ~ "PR",
      TRUE ~ state
    )
  ) |>
  # Convert `date` from character to Date format using `ymd_hms` from lubridate
  mutate(date = as_date(ymd_hms(date))) |>
  # Remove duplicates
  distinct(state, date, .keep_all = TRUE) |>
  # Create new columns for MMWR week and year using lubridate functions `epiweek()` and `epiyear()`
  mutate(
    mmwr_week = epiweek(date),
    mmwr_year = epiyear(date)
  ) |>
  # Select relevant columns: state, MMWR year, MMWR week, series complete, and booster doses
  select(state, mmwr_year, mmwr_week, series_complete_cumulative, booster_cumulative) |>
  # Group by state, MMWR week, and MMWR year to calculate total vaccinations per week per state
  group_by(state, mmwr_week, mmwr_year) |>
  # Summarize to calculate the total number of vaccinations (`max(series_complete)` and `max(booster)`)
  summarize(
    series_complete = max(as.numeric(series_complete_cumulative), na.rm = TRUE),
    booster = max(as.numeric(booster_cumulative), na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  # Filter to keep only the weeks with complete data (7 days of reporting)
  #filter(n == 7) |>
  select(-n) |>
  # Arrange by state, MMWR year, and week for easier analysis
  arrange(state, mmwr_year, mmwr_week)




# join

# Make dates data frame: create a complete sequence of weeks for 2020 and 2021
all_dates <- data.frame(date = seq(make_date(2020, 1, 25), make_date(2021, 12, 31), by = "week")) |>
  # Ensure each date represents the end of the MMWR week (week ending on Saturday)
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  # Create MMWR year and MMWR week columns using lubridate functions
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date))
# Cross join the dates data frame with all unique states to create a full set of combinations
dates_and_pop <- crossing(all_dates, data.frame(state = unique(population$state))) |>
  # Join the population data by matching state and year
  left_join(population, by = c("state", "mmwr_year" = "year"))

# Join the datasets (`cases`, `hosp`, `deaths`, and `vax`) to the complete weeks and states
full_data <- dates_and_pop |>
  left_join(cases, by = c("state", "mmwr_week", "mmwr_year")) |>
  left_join(hosp, by = c("state", "mmwr_week", "mmwr_year")) |>
  left_join(deaths, by = c("state", "mmwr_week", "mmwr_year")) |>
  left_join(vax, by = c("state", "mmwr_week", "mmwr_year")) |>
  select(-n)

# Print the first few rows to inspect the joined data frame
print(head(full_data))
view(full_data)


# Save the joined dataset to an RDS file in the data directory
saveRDS(full_data, "../data/dat.rds")
