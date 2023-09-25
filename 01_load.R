# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Purpose:  Script loads data and uses various criteria to filter out flow recording stations in BC.


library(tidyverse)
library(lubridate)
library(tidyhydat)
library(stringr)

# Set up folder structure, for intermediate data
if(!dir.exists('data')) dir.create('data')

# Set up /www folder - used for shiny app
if(!dir.exists('app/www')) dir.create('app/www')

##### First pass to filter for stations with complete data

# Complete step for first time users or if data is out-of-date
tidyhydat::download_hydat()

## Filter stations for last n years of data, minimum number of years
year_filt <- year(Sys.Date())-5 # to select stations that are active within the last 5 years
n_years_filt <- 10 # stations must have a minimum of 10 yrs for trending

## Get all BC stations with "flow"
stations_all_bc_list <- unique(hy_annual_stats(prov_terr_state_loc = "BC") %>%
                                 filter(Parameter == "Flow") %>%
                                 pull(STATION_NUMBER))

## Pulls HYDAT daily flow data for all BC stations that measure flow
#  Step takes a few minutes to complete

hydat_daily_all <- hy_daily_flows(station_number = stations_all_bc_list)

## Filter stations for n complete years
daily_station_data <- hydat_daily_all %>%
  mutate(Year = year(Date)) %>%
  group_by(STATION_NUMBER, Year) %>%
  summarise(na = sum(is.na(Value)),
            Ann_Mean = mean(Value, na.rm = TRUE),
            perc_daily_missing = na / 365 * 100)

stations_filt <- daily_station_data |>
  group_by(STATION_NUMBER) %>%
  summarise(n_years = n(),
            incomplete_years = sum(na > 0),
            year_min = min(Year),
            year_max = max(Year)) %>%
  filter(year_max >= year_filt,
         n_years >= n_years_filt) %>%
  left_join(hy_stations(), by = "STATION_NUMBER") %>%
  left_join(hy_stn_regulation(), by = "STATION_NUMBER")

stations_filt_list <- unique(stations_filt$STATION_NUMBER)

saveRDS(hydat_daily_all, file = 'data/hydat_daily_all.rds')
saveRDS(daily_station_data, file = 'data/daily_station_data.rds')
saveRDS(stations_filt_list, file = 'data/stations_filt_list.rds')
saveRDS(stations_filt, file = 'data/stations_filt.rds')
