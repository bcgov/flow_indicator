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

if(!exists("final_stations_summary")){final_stations_summary = read.csv('data/finalstns.csv')}

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Pull out the stations to keep from the loading script.
stations_to_keep = final_stations_summary$STATION_NUMBER

# The below code calculates the following flow variables:
# 1. Mean/median flow per year,
# 2. Day of year by which 50% of flow has passed,
# 3. 7-day flow minimum / day of year of 7-day flow minimum
# 4. 30-day flow minimum / day of year of 30-day flow minimum
# 4. Total annual flow (in m^3).

flow_dat = tidyhydat::hy_daily_flows(stations_to_keep) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  mutate(Month = lubridate::month(Date))

# Annual Values =========================================================
annual_mean_dat = flow_dat %>%
  group_by(Year,STATION_NUMBER) %>%
  summarise(Mean = mean(Value),
            Median = median(Value)) %>%
  ungroup()

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
  dplyr::select(STATION_NUMBER,
                Year,DoY_50pct_TotalQ#,
                #Date_50pct_TotalQ
  )

# 7-day and 30-day rolling averages for Low flow (flow value, day of year, Date)
stations_list = as.list(stations_to_keep)

lowflow_dat = stations_list %>% map( ~ {

  print(paste0('Working on station ',.x,'!'))

  daily_flows = hy_daily_flows(station_number = c(.x)) %>%
    filter(!is.na(Value)) %>%
    mutate(Year = lubridate::year(Date)) %>%
    group_by(STATION_NUMBER,Year) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

  daily_flows_dt$Min_30_Day = frollmean(daily_flows_dt[, Value], 30, align = 'right')

  min_7_day_dat = daily_flows_dt %>%
    group_by(STATION_NUMBER,Year) %>%
    slice_min(Min_7_Day) %>%
    group_by(STATION_NUMBER,Year,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Min_30_Day, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)

  min_30_day_dat = daily_flows_dt %>%
    group_by(STATION_NUMBER,Year) %>%
    slice_min(Min_30_Day) %>%
    group_by(STATION_NUMBER,Year,Min_30_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Min_7_Day, Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

  min_7_day_dat %>%
    left_join(min_30_day_dat,
              by = join_by(STATION_NUMBER, Year)
    )
}) %>%
  bind_rows()

# Total volume

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

# write.csv(annual_flow_dat, './app/www/annual_flow_dat.csv', row.names = F)

# Do the same but by month!

monthly_mean_dat = flow_dat %>%
  group_by(Year,Month,STATION_NUMBER) %>%
  summarise(Mean = mean(Value),
            Median = median(Value)) %>%
  ungroup()

# 7-day and 30-day rolling averages for Low flow
monthly_lowflow_dat = stations_list %>% map( ~ {

  print(paste0('Working on station ',.x,'!'))

  daily_flows = flow_dat %>%
    filter(STATION_NUMBER == .x) %>%
    group_by(STATION_NUMBER,Year) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

  daily_flows_dt$Min_30_Day = frollmean(daily_flows_dt[, Value], 30, align = 'right')

  min_7_day_dat = daily_flows_dt %>%
    group_by(STATION_NUMBER,Year,Month) %>%
    slice_min(Min_7_Day) %>%
    group_by(STATION_NUMBER,Year,Month,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Min_30_Day, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)

  min_30_day_dat = daily_flows_dt %>%
    group_by(STATION_NUMBER,Year,Month) %>%
    slice_min(Min_30_Day) %>%
    group_by(STATION_NUMBER,Year,Month,Min_30_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Min_7_Day, Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

  min_7_day_dat %>%
    left_join(min_30_day_dat,
              by = join_by(STATION_NUMBER, Year, Month)
    )
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

# write.csv(monthly_flow_dat, './app/www/monthly_flow_dat.csv', row.names = F)

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
  mutate(DoY_50pct_TotalQ_halfyear_max = abs(DoY_50pct_TotalQ - 182),
         Min_7_Day_DoY_halfyear_max = abs(Min_7_Day_DoY - 182))

# Write out dataset at this point - data wide, unsummarised.
write.csv(dat_combo_num,'app/www/combined_flow_dat.csv',row.names = F)

# Get station locations
stations_sf = tidyhydat::hy_stations(station_number = unique(dat_combo$STATION_NUMBER)) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)

write_sf(stations_sf, 'app/www/stations.gpkg')
