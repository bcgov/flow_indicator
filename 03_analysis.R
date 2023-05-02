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

# The below code calculates the following flow variables:
# 1. Mean/median flow per year,
# 2. Day of year by which 50% of flow has passed,
# 3. 7-day flow minimum / day of year of 7-day flow minimum
# 4. 30-day flow minimum / day of year of 30-day flow minimum
# 5. Total annual flow (in m^3).

library(EnvStats)
library(tidyverse)

# Annual Values =========================================================
annual_mean_dat = flow_dat %>%
  group_by(Year,STATION_NUMBER) %>%
  summarise(Average = median(Value)) %>%
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
# Note: this is SUMMER low flow (i.e. May - October)
stations_list = as.list(stations_to_keep)

low_high_flow_dat = stations_list %>% map( ~ {

  print(paste0('Working on station ',.x,'!'))

  daily_flows = flow_dat %>%
    filter(STATION_NUMBER == .x) |>
    group_by(STATION_NUMBER,Year) |>
    mutate(my_row = row_number())  |>
    ungroup()

  # Summer low flows (7-day average low flow and 30-day average low flow)
  low_daily_flows = daily_flows  |>
    filter(Month %in% c(5:10))

  low_daily_flows_dt = data.table::data.table(low_daily_flows, key = c('STATION_NUMBER','Year'))

  low_daily_flows_dt$Min_7_Day = frollmean(low_daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  low_daily_flows_dt$Min_30_Day = frollmean(low_daily_flows_dt[, Value], 30, align = 'right', na.rm=T)

  min_7_day_dat = low_daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(Min_7_Day != lead(Min_7_Day)) %>%
    group_by(Year) %>%
    slice_min(Min_7_Day) %>%
    group_by(Year,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, -Min_30_Day, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)

  min_30_day_dat = low_daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(Min_30_Day != lead(Min_30_Day)) %>%
    group_by(Year) %>%
    slice_min(Min_30_Day) %>%
    group_by(Year,Min_30_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, -Min_7_Day, Min_30_Day_DoY = my_row, Min_30_Day_Date = Date)

  summer_low_flows = min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year, everything()) %>%
    left_join(min_30_day_dat,
              by = join_by(STATION_NUMBER, Year)
    )

  # Peak flows (7-day average flows, could be any time of year)

  high_daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  high_daily_flows_dt$Max_7_Day = frollmean(high_daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  max_7_day_dat = high_daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(Max_7_Day != lead(Max_7_Day)) %>%
    group_by(Year) %>%
    slice_max(Max_7_Day) %>%
    group_by(Year,Max_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Max_7_Day_DoY = my_row, Max_7_Day_Date = Date)

  summer_low_flows %>%
    left_join(max_7_day_dat,
              by = join_by(STATION_NUMBER,Year))
}) %>%
  bind_rows()

annual_flow_dat = annual_mean_dat %>%
  left_join(flow_timing_dat) %>%
  left_join(low_high_flow_dat)

# Do the same but by month!

## Monthly median flow.
monthly_mean_dat = flow_dat %>%
  mutate(Month = lubridate::month(Month, abbr = T, label = T)) %>%
  group_by(Year,Month,STATION_NUMBER) %>%
  summarise(Average = median(Value)) %>%
  ungroup() %>%
  pivot_wider(names_from = Month, values_from = Average, names_prefix = 'Average_')

## Monthly 7-day average low flow
# 7-day and 30-day rolling averages for Low flow (flow value, day of year, Date)
# Note: this is SUMMER low flow (i.e. May - October)
low_flow_monthly_dat = stations_list %>% map( ~ {

  print(paste0('Working on station ',.x,'!'))

  low_daily_flows =  flow_dat %>%
    filter(STATION_NUMBER == .x) %>%
    group_by(STATION_NUMBER,Year,Month) %>%
    mutate(my_row = row_number()) %>%
    ungroup()

  low_daily_flows_dt = data.table::data.table(low_daily_flows, key = c('STATION_NUMBER','Year','Month'))

  low_daily_flows_dt$Min_7_Day = frollmean(low_daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  min_7_day_dat = low_daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(Min_7_Day != lead(Min_7_Day)) %>%
    group_by(Year,Month) %>%
    slice_min(Min_7_Day) %>%
    group_by(Year,Month,Min_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -my_row,
                  Min_7_Day_Monthly = Min_7_Day, -Date)

  min_7_day_dat
}) %>%
  bind_rows()

low_flow_monthly_dat = low_flow_monthly_dat |>
  dplyr::select(-Min_7_Day_Date_Monthly) |>
  mutate(Month = paste0('LowFlow7_',month.abb[Month])) |>
  pivot_wider(names_from = Month, values_from = Min_7_Day_Monthly)
# =====================================
# Combine annual and monthly data, let's see if that works better.
# Split table by data type: numeric or date. Question: do we need the date vars??

dat_combo = annual_flow_dat %>%
  # mutate(Month = 'All') %>%
  left_join(monthly_mean_dat) |>
  left_join(low_flow_monthly_dat)

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
         Min_7_Day_DoY_halfyear_max = abs(Min_7_Day_DoY - 182),
         Max_7_Day_DoY_halfyear_max = abs(Min_7_Day_DoY - 182))

# Write out dataset at this point - data wide, unsummarised.
# Very strange...  I can't overwrite the original file, not sure how to
# detach the R session from 'combined_flow_dat.csv'. For now, make a 2nd one
# and manually delete the first one, after closing the R session... not ideal.
write_csv(dat_combo_num, 'app/www/combined_flow_dat2.csv')

# Get station locations
stations_sf = tidyhydat::hy_stations(station_number = unique(dat_combo$STATION_NUMBER)) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)

write_sf(stations_sf, 'app/www/stations.gpkg')
