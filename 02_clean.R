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
rm(list = ls())
if(!exists("mean_annual_flow_per_station")){load('./tmp/station_data.Rdata')}

### Filter out some stations
# 1. (NOT CURRENTLY IMPLEMENTED) Remove stations that do not have significant alteration (include some “regulated” stations);
# 2. (NOT CURRENTLY IMPLEMENTED) Remove stations that are upstream of another station without significant inputs (duplication of results);
# 3. (NOT CURRENTLY IMPLEMENTED) 80% decadal data completeness (so large missing gaps);
# 4. must have end year of data within past 5 years (likely all active stations).

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
#1914 stations to exclude (their most recent year of data is 2017 or earlier)

stations_to_keep = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% all_of(stations_to_exclude)) %>%
  dplyr::select(STATION_NUMBER) %>%
  distinct() %>%
  pull(STATION_NUMBER)

# Apply station filter to data.
mean_annual_flow_per_station = mean_annual_flow_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_exclude) %>%
  as_tibble()
number_daily_records_per_station = number_daily_records_per_station %>%
  filter(!STATION_NUMBER %in% stations_to_exclude) %>%
  as_tibble()

# restrict the data to 1967 onward
mean_annual_flow_per_station = mean_annual_flow_per_station %>%
  filter(Year >= 1967)
number_daily_records_per_station = number_daily_records_per_station %>%
  filter(Year >= 1967)

save(stations_to_keep, stations_to_exclude, mean_annual_flow_per_station, number_daily_records_per_station, file = './tmp/station_data_cleaned.Rdata')
