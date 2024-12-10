# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(purrr)
library(janitor)
library(readxl)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)
source("funcs.R")
# census key
census_key <- readRDS("census_key.rds")



# Provisional COVID-19 Death Counts by Week Ending Date and State, from week of 2019-12-29 to 2024-11-30
url <- "https://data.cdc.gov/resource/r8kw-7aab.json"
dat_raw <- get_cdc_data(url)
dat <- dat_raw |>
  filter(group == "By Week") |> 
  rename(state_name = state)|>
  select(state_name,year,mmwr_week, week_ending_date, total_deaths,covid_19_deaths) |>
  filter(state_name != "United States") |>
  mutate(week_ending_date = ymd(as_date(week_ending_date))) |>
  mutate(
    week_ending_date = ymd(week_ending_date),
    year = as.character(year),
    year = if_else(
      year == "2019/2020",
      "2020",
      substr(year, 1, 4)
    ))


# Weekly Counts of Deaths by State and Select Causes, 2014-2019, from week of 2014-10-25 to 2019-12-28
url <- "https://data.cdc.gov/resource/3yf8-kanr.json"
dat_raw2 <- get_cdc_data(url)
dat2 <- dat_raw2 |>
  rename(state_name = jurisdiction_of_occurrence) |>
  rename(year = mmwryear) |>
  select(1:5)|>
  rename(total_deaths = allcause ) |>
  filter(state_name != "United States") |>
  mutate(week_ending_date = ymd(as_date(weekendingdate))) |>
  select(-weekendingdate)|>
  rename(mmwr_week = mmwrweek)
  
dat_full <- dat2 |> mutate(covid_19_deaths = NA) |>
  bind_rows(dat)|>
  arrange(year, mmwr_week, state_name)

  

# Population data from 2020 to 2023, there is no API for this
file_path <- "../data/NST-EST2023-POP.xlsx"
pop_data <- read_excel(file_path, sheet = "NST-EST2023-POP", skip = 3)
colnames(pop_data)[1] <- "state"
pop_2023 <- pop_data |>
  filter(str_starts(state, "\\.")) %>%
  mutate(state = str_remove(state, "^\\."))|>
  select(-2) |>
  rename(state_name = state)|>
  pivot_longer(-state_name, names_to="year", values_to = "population")|>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )



# population for 2019
url <- "https://api.census.gov/data/2019/pep/population"
request <- request(url) |> req_url_query(get = "POP,NAME",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2019 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2019 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =NAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2019) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )



# population for 2018
url <- "https://api.census.gov/data/2018/pep/population"
request <- request(url) |> req_url_query(get = "POP,GEONAME",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2018 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2018 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =GEONAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2018) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )


# population for 2017
url <- "https://api.census.gov/data/2017/pep/population"
request <- request(url) |> req_url_query(get = "POP,GEONAME",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2017 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2017 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =GEONAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2017) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )

# population for 2016
url <- "https://api.census.gov/data/2016/pep/population"
request <- request(url) |> req_url_query(get = "POP,GEONAME",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2016 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2016 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =GEONAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2016) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )


# population for 2015
url <- "https://api.census.gov/data/2015/pep/population"
request <- request(url) |> req_url_query(get = "POP,GEONAME",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2015 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2015 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  select(-state) |>
  rename(state_name =GEONAME)|>
  mutate(state_name = str_extract(state_name, "^[^,]+"))|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2015) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )

# population for 2014
url <- "https://api.census.gov/data/2014/pep/natstprc"
request <- request(url) |> req_url_query(get = "POP,STNAME,DATE_",  
                                         `for` = "state:*",
                                         key = census_key
)

response <-  request |> req_perform()

pop_2014 <- response |> resp_body_json(simplifyVector = TRUE) 
pop_2014 <- response |> resp_body_json(simplifyVector = TRUE) |>
  row_to_names(1) |>
  as_tibble() |>
  filter(DATE_ == "1") |>
  select(-state,-DATE_) |>
  rename(state_name =STNAME)|>
  pivot_longer(-state_name, names_to = "year", values_to="population") |>
  mutate(year = 2014) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]  ) |>
  mutate(
    state= case_when (state_name == "Puerto Rico"~"PR",
                      state_name == "District of Columbia" ~ "DC",
                      TRUE ~ state)
  )

population_combined <- bind_rows(
  pop_2014, 
  pop_2015, 
  pop_2016, 
  pop_2017, 
  pop_2018, 
  pop_2019, 
  pop_2023
)
population_combined <- population_combined %>%
  arrange(year, state_name)



dat_full <- dat_full %>%
  mutate(year = as.numeric(year))|>
  left_join(
    population_combined %>% select(year, state_name, population, state),
    by = c("year", "state_name")
  )|>
  relocate(week_ending_date, .before = total_deaths)





# regions
url<-"https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions<-fromJSON(url,simplifyDataFrame=FALSE)
regions<-map_df(regions, function(x)
  data.frame(region= x$region,region_name= x$region_name,state_name= x$states)) |>
  mutate(region_name = ifelse(region ==2,"NJ,NY,PR,VI",region_name))
population_combined <-left_join(population_combined, regions, by = "state_name")








# Save the joined dataset to an RDS file in the data directory
saveRDS(dat_full, "../data/dat.rds")
