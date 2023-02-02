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

### Load in data that was accessed in the '01_load.R' script.

if(!exists("number_daily_records_per_station")){load('./tmp/station_data.Rdata')}

### Filter out some more stations
# 1. Must have end year of data within past 5 years (likely all active stations).
# 2. Exclude any stations with fewer than 10 years of data.

# i. (NOT CURRENTLY IMPLEMENTED) Remove stations that are upstream of another station without significant inputs (duplication of results);
# ii. 80% decadal data completeness (i.e. for each 10-year chunk of data, it should
#     have at least 80% of years with data?);

stations_to_exclude = number_daily_records_per_station %>%
  group_by(STATION_NUMBER) %>%
  # Get most recent years of data for each station
  arrange(desc(Year)) %>%
  slice(1) %>%
  # Get the station IDs of stations whose most recent data is 2017 or older (we'll drop these stations)
  filter(Year <= 2017) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER) %>%
  distinct() %>%
  pull(STATION_NUMBER)
#1280 stations to exclude (their most recent year of data is 2017 or earlier)

# Stations with fewer than 10 years of data.
stats_fewer_10_years = number_daily_records_per_station %>%
  count(STATION_NUMBER) %>%
  filter(n < 10) %>%
  pull(STATION_NUMBER)

stations_to_exclude = unique(c(stations_to_exclude, stats_fewer_10_years))

# # Stations with data gaps of 20% or more within decades of data
# # (note: the start year of each 10-year chunk depends on the station,
# #  rather than it just being fixed, e.g. 1980-1990, 1990-2000, etc.)
# number_daily_records_per_station %>%
#   mutate(decade = 10*(Year-1900) %/% 10) %>%
#   group_by(STATION_NUMBER,decade) %>%
#   mutate(prop_with_data = sum(DaysWithData)/())

## UNFINISHED ABOVE ##

stations_to_keep = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_exclude) %>%
  dplyr::select(STATION_NUMBER) %>%
  distinct() %>%
  pull(STATION_NUMBER)

# Apply station filter to data.
number_daily_records_per_station = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_exclude) %>%
  as_tibble()

# # restrict the data to 1967 onward
# number_daily_records_per_station = number_daily_records_per_station %>%
#   filter(Year >= 1967)

save(stations_to_keep, stations_to_exclude, number_daily_records_per_station, file = './tmp/station_data_cleaned.Rdata')
