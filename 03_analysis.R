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

# Work plan:
# 1. Do the autocorrelation-correcting MK test on stations for 2 time periods: 25-30 years, and 50+ years.
# 2. Calculate metrics:

# USGS has:
# lowQ one day
# lowQ three days
# lowQ seven days
# scaled deficit
# Q2 ndays
# last Julian date <= Q2 threshold
# first Julian date <= Q2 threshold
# JD Difference Q2
# ZeroQ nDays
# Instantaneous Annual Peak Flows

# Carl's App currently has:

#    a. annual average flows (change in volume over years)
#    b. summer 7-day low flow (change in lowest summer drought flows)
#    c. spring freshet timing start and/or timing of half flows (shift in timing of spring melt)
#    d. spring freshet 3-day peak for freshet stations;
#    e. winter peak flows for PPT-driven stations (changes in peak flows)

# If I just use the fasstr app (which is what Carl did), I get access immediately
# to a large number of variables.

# library(StreamFlowTrend)
# library(fasstr)
library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)

# Use the list of stations that match our filtering requirements to access variables of interest.
# 1. Timing of 50% annual flow.
# 2. Timing of low flow (7-day period)
# 3. Total annual volume.

if(!exists("number_daily_records_per_station")){load('./tmp/station_data_cleaned.Rdata')}

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('./www')) dir.create('./www')

# The below code calculates the following flow variables:
# 1. Mean/median flow per year,
# 2. Day of year by which 50% of flow has passed,
# 3. 7-day flow minimum / day of year of 7-day flow minimum
# 4. Total annual flow (in m^3).

flow_dat = tidyhydat::hy_daily_flows(stations_to_keep) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Month = lubridate::month(Date))

# Annual Values #
# Calculate annual mean flow, Date of 50% annual flow, 7-day low flow date, and total volume in cubic meters.
# annual_mean_dat = calc_annual_stats(station_number = stations_to_keep) %>%
#   filter(!is.na(Mean)) %>%
#   dplyr::select(Year,STATION_NUMBER,Mean,Median)
annual_mean_dat = flow_dat %>%
  group_by(Year,STATION_NUMBER) %>%
  summarise(Mean = mean(Value),
            Median = median(Value)) %>%
  ungroup()

# flow_timing_dat = calc_annual_flow_timing(station_number = stations_to_keep, percent_total = 50) %>%
#   filter(!is.na(Date_50pct_TotalQ))
flow_timing_dat = flow_dat %>%
  group_by(STATION_NUMBER,Year) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = lubridate::yday(Date)) %>%
  rename('Date_50pct_TotalQ' = Date) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,Year,DoY_50pct_TotalQ,Date_50pct_TotalQ)

# lowflow_dat = calc_annual_lowflows(station_number = stations_to_keep, roll_days = 7) %>%
#   filter(!is.na(Min_7_Day_Date))

# 7-day rolling averages for Low flow
stations_list = as.list(stations_to_keep)

lowflow_dat = stations_list %>% map( ~ {
  daily_flows = hy_daily_flows(station_number = c(.x)) %>%
    filter(!is.na(Value)) %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(STATION_NUMBER,Year) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

  as_tibble(daily_flows_dt) %>%
    group_by(STATION_NUMBER,Year) %>%
    slice_min(Min_7_Day) %>%
    group_by(STATION_NUMBER,Year,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)
}) %>%
  bind_rows()

# Total volume

# totalvolume_dat = calc_annual_cumulative_stats(station_number = stations_to_keep) %>%
#   filter(!is.na(Total_Volume_m3))
totalvolume_dat = flow_dat %>%
  # The flow parameter here is a flow rate, i.e. m^3/second.
  # Multiply by number of seconds in a day to get volume.
  mutate(Volume = Value*86400) %>%
  group_by(STATION_NUMBER,Year) %>%
  summarise(Total_Volume_m3 = sum(Volume))

annual_flow_dat = annual_mean_dat %>%
  left_join(flow_timing_dat) %>%
  left_join(lowflow_dat) %>%
  left_join(totalvolume_dat)

write.csv(annual_flow_dat, './www/flow_dat.csv', row.names = F)

# Do the same but by month!

# Calculate monthly mean flow, Date of 50% monthly flow, 7-day low flow date,
# and total monthly volume in cubic meters.

monthly_mean_dat = flow_dat %>%
  group_by(Year,Month,STATION_NUMBER) %>%
  summarise(Mean = mean(Value),
            Median = median(Value)) %>%
  ungroup()

# 7-day rolling averages for Low flow
stations_list = as.list(stations_to_keep)

monthly_lowflow_dat = stations_list %>% map( ~ {
  daily_flows = flow_dat %>%
    filter(STATION_NUMBER == .x) %>%
    group_by(STATION_NUMBER,Year) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

  as_tibble(daily_flows_dt) %>%
    group_by(STATION_NUMBER,Year,Month) %>%
    slice_min(Min_7_Day) %>%
    group_by(STATION_NUMBER,Year,Month,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)
}) %>%
  bind_rows()

# Total volume

# totalvolume_dat = calc_annual_cumulative_stats(station_number = stations_to_keep) %>%
#   filter(!is.na(Total_Volume_m3))
monthly_totalvolume_dat = flow_dat %>%
  # The flow parameter here is a flow rate, i.e. m^3/second.
  # Multiply by number of seconds in a day to get volume.
  mutate(Volume = Value*86400) %>%
  group_by(STATION_NUMBER,Year,Month) %>%
  summarise(Total_Volume_m3 = sum(Volume))

monthly_flow_dat = monthly_mean_dat %>%
  left_join(monthly_lowflow_dat) %>%
  left_join(monthly_totalvolume_dat)

write.csv(monthly_flow_dat, './www/monthly_flow_dat.csv', row.names = F)

# =====================================
# Combine annual and monthly data, let's see if that works better.
# Split table by data type: numeric or date. Question: do we need the date vars??

dat_combo = annual_flow_dat %>%
  mutate(Month = 'All') %>%
  bind_rows(monthly_flow_dat %>%
              mutate(Month =  month.abb[Month]))

# Remove Date variables. Should we keep them...? Uncertain.
dat_combo_num = dat_combo %>%
  dplyr::select(-contains('Date'))

# Experimental: transform Day-of-Year variables such that
# day 182 (half-way through the year) is the largest possible value,
# while days 1 and 364 have 182 subtracted from them and the
# absolute value is taken of the results, such that these 2 dates are
# very close to each other.
dat_combo_num = dat_combo_num %>%
  mutate(DoY_50pct_TotalQ = abs(DoY_50pct_TotalQ - 182),
         Min_7_Day_DoY = abs(Min_7_Day_DoY - 182))

dat_combo_num = dat_combo_num %>%
  pivot_wider(names_from = 'name', values_from = 'value')

# Write out dataset at this point - data wide, unsummarised.
write.csv(dat_combo_num,'app/www/all_dat.csv',row.names = F)

# Daily Values ------------------------------------------------

flow_dat = tidyhydat::hy_daily_flows(stations_to_keep) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value))

# Daily Values #
# Calculate Daily mean flow, median flow, and total volume in cubic meters.
daily_mean_dat = flow_dat %>%
  group_by(STATION_NUMBER,Date) %>%
  summarise(Mean = mean(Value),
            Median = median(Value))

# flow_timing_dat = calc_Daily_flow_timing(station_number = stations_to_keep, percent_total = 50) %>%
#   filter(!is.na(Date_50pct_TotalQ))
flow_timing_dat = flow_dat %>%
  group_by(STATION_NUMBER,Year) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = lubridate::yday(Date)) %>%
  rename('Date_50pct_TotalQ' = Date) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,Year,DoY_50pct_TotalQ,Date_50pct_TotalQ)

# lowflow_dat = calc_Daily_lowflows(station_number = stations_to_keep, roll_days = 7) %>%
#   filter(!is.na(Min_7_Day_Date))

# 7-day rolling averages for Low flow
stations_list = as.list(stations_to_keep)

lowflow_dat = stations_list %>% map( ~ {
  daily_flows = hy_daily_flows(station_number = c(.x)) %>%
    filter(!is.na(Value)) %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(STATION_NUMBER,Year) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

  as_tibble(daily_flows_dt) %>%
    group_by(STATION_NUMBER,Year) %>%
    slice_min(Min_7_Day) %>%
    group_by(STATION_NUMBER,Year,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)
}) %>%
  bind_rows()

# Total volume

# totalvolume_dat = calc_Daily_cumulative_stats(station_number = stations_to_keep) %>%
#   filter(!is.na(Total_Volume_m3))
totalvolume_dat = flow_dat %>%
  # The flow parameter here is a flow rate, i.e. m^3/second.
  # Multiply by number of seconds in a day to get volume.
  mutate(Volume = Value*86400) %>%
  group_by(STATION_NUMBER,Year) %>%
  summarise(Total_Volume_m3 = sum(Volume))

Daily_flow_dat = Daily_mean_dat %>%
  left_join(flow_timing_dat) %>%
  left_join(lowflow_dat) %>%
  left_join(totalvolume_dat)

write.csv(daily_flow_dat, './www/daily_flow_dat.csv', row.names = F)


# Get station locations
stations_sf = tidyhydat::hy_stations(station_number = unique(dat_combo$STATION_NUMBER)) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)
write_sf(stations_sf, 'app/www/stations.gpkg')
