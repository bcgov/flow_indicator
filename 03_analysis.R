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

### Load in data that was produced in the '02_clean.R' script.

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)

if(!exists("final_station_summary")){final_station_summary = read_csv('data/finalstns.csv')}
if(!exists("filtered_station_year")){filtered_station_year = read_csv('data/finalstnyr.csv')}


# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Pull out the stations to keep from the loading script.
stations_to_keep = final_station_summary %>%
  # filter(keep == TRUE) %>%
  pull(STATION_NUMBER)

# The below code calculates the following flow variables:
# 1. Average (median, not mean) flow per year,
# 2. Timing of freshet (i.e. day of year by which 50% of flow has passed),
# 3. Summer (May to late October) 7-day flow minimum / day of year of 7-day flow minimum
# 4. Peak annual flow (7-day flow maximum)

# Read in ALL daily flow info from the database for our stations to keep.
# Note: this includes 'gappy' data we identified in script 1.
# ~5 million rows, 7 columns.
flow_dat = tidyhydat::hy_daily_flows(stations_to_keep) %>%
  mutate(wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) + 1),
         lfYear = case_when(month(Date) >=4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1),
         Year = year(Date)) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value)) %>%
  mutate(Month = lubridate::month(Date))

#Restrict by station_year list (this will remove years with missing data)
flow_dat_filtered_filtered = flow_dat_filtered %>%
  filter(paste0(STATION_NUMBER, wYear) %in% paste0(filtered_station_year$STATION_NUMBER,filtered_station_year$Year)) %>%
  mutate(wDoY = case_when(month(Date) >= 10 ~ yday(Date) - yday(paste0(year(Date),"-09-30")),
                          month(Date) < 10 ~ yday(Date) + (365 - yday(paste0(year(Date),"-09-30")))),
         lfDoY = case_when(month(Date) >= 4 ~ yday(Date) - yday(paste0(year(Date),"-03-31")),
                           month(Date) < 4 ~ yday(Date) + (365 - yday(paste0(year(Date),"-03-31")))))

# # Take out big data gaps. This map function cycles through our station numbers,
# # identifying the new start year, if any (e.g. a station might have one year of
# # data in 1956, but then lack many years of data until 1973; this function would trim
# # away all data up to 1973).
# # Produces dataset of ~ 4.7 million rows
#
# flow_dat_filtered = unique(flow_dat_filtered$STATION_NUMBER) %>%
#   map( ~ {
#     year_for_trimming = unique(final_stations_summary[final_stations_summary$STATION_NUMBER == .x,]$Min_Year)
#
#     station_dat = flow_dat_filtered %>%
#       filter(STATION_NUMBER == .x)
#
#     filtered_dat = station_dat %>%
#       filter(wYear >= year_for_trimming)
#
#     filtered_dat
#   }) %>%
#   bind_rows() %>%


# Annual Values =========================================================
annual_mean_dat = flow_dat_filtered_filtered %>%
  group_by(wYear,STATION_NUMBER) %>%
  summarise(Average = mean(Value)) %>%
  ungroup() %>%
  rename(Year = wYear)

# Check data gaps are gone
ggplot(annual_mean_dat, aes(Year,STATION_NUMBER, colour = Average))+
  geom_point()


# Timing of freshet (i.e. date by which 50% of total annual flow has passed)
flow_timing_dat = flow_dat_filtered_filtered %>%
  group_by(STATION_NUMBER,wYear) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = wDoY,
         Date = Date) %>%
  # rename('Date_50pct_TotalQ' = Date) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year = wYear,DoY_50pct_TotalQ,
                Date
  )

# Create change column and then calculate cumulative change


# 7-day Summer Low flow (flow value, day of year, Date)
# Note: this is SUMMER low flow (i.e. May - October)

low_high_flow_dat_filtered_7day = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered |>
    filter(STATION_NUMBER ==.x)
    # group_by(STATION_NUMBER,Year) |#>
    # mutate(my_row = row_number()) |>
    # ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  # Filter for just summer months...
  # daily_flows_dt_summer = daily_flows_dt %>%
  #  filter(Month %in% c(5:9))

  min_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day) & between(Month, 3, 10)) %>%
    group_by(lfYear) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_7_Day_summer_DoY = lfDoY,
                  Min_7_Day_summer_Date = Date, Min_7_summer_Day = flow_7_Day, -wYear, -Year)%>%
    dplyr::select(-wDoY)

  summer_low_flows = min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything())

  min_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day)) %>%
    group_by(lfYear) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_7_Day_DoY = lfDoY,
                  Min_7_Day_Date = Date, Min_7_Day = flow_7_Day, -wYear, -Year)%>%
    dplyr::select(-wDoY)

  low_flows = min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything()) %>%
    left_join(summer_low_flows, by = join_by(STATION_NUMBER, Year))

  # Find Peak flows (7-day average flows, could be any time of year)
  max_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    group_by(wYear) %>%
    slice_max(flow_7_Day) %>%
    group_by(wYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month,
                  Max_7_Day_DoY = wDoY, Max_7_Day_Date = Date,
                  Max_7_Day = flow_7_Day, -lfYear, -Year)%>%
    dplyr::select(- lfDoY, Year = wYear)

  low_flows %>%
    left_join(max_7_day_dat,
              by = join_by(STATION_NUMBER, Year))
}) %>%
  bind_rows()

low_high_flow_dat_filtered_3day = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered |>
    filter(STATION_NUMBER ==.x)
  # group_by(STATION_NUMBER,Year) |#>
  # mutate(my_row = row_number()) |>
  # ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_3_Day = frollmean(daily_flows_dt[, Value], 3, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  # Filter for just summer months...
  # daily_flows_dt_summer = daily_flows_dt %>%
  #  filter(Month %in% c(5:9))

  min_3_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_3_Day != lead(flow_3_Day) & between(Month, 3, 10)) %>%
    group_by(lfYear) %>%
    slice_min(flow_3_Day) %>%
    group_by(lfYear,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_3_summer_Day_DoY = lfDoY,
                  Min_3_Day_summer_Date = Date, Min_3_summer_Day = flow_3_Day, -wYear, -Year)%>%
    dplyr::select(-wDoY)

  summer_low_flows = min_3_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything())

  min_3_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_3_Day != lead(flow_3_Day)) %>%
    group_by(lfYear) %>%
    slice_min(flow_3_Day) %>%
    group_by(lfYear,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_3_Day_DoY = lfDoY,
                  Min_3_Day_Date = Date, Min_3_Day = flow_3_Day, -wYear, -Year)%>%
    dplyr::select(-wDoY)

  low_flows = min_3_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything()) %>%
    left_join(summer_low_flows, by = join_by(STATION_NUMBER, Year))

  # Find Peak flows (7-day average flows, could be any time of year)
  max_3_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    group_by(wYear) %>%
    slice_max(flow_3_Day) %>%
    group_by(wYear,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month,
                  Max_3_Day_DoY = wDoY, Max_3_Day_Date = Date,
                  Max_3_Day = flow_3_Day, -lfYear, -Year)%>%
    dplyr::select(- lfDoY, Year = wYear)

  low_flows %>%
    left_join(max_3_day_dat,
              by = join_by(STATION_NUMBER, Year))
}) %>%
  bind_rows()

annual_flow_dat_filtered = annual_mean_dat  |>
  left_join(flow_timing_dat) |>
  left_join(low_high_flow_dat_filtered_7day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year")) |>
  left_join(low_high_flow_dat_filtered_3day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year")) |>
  dplyr::select(-ends_with("_Date"))

#Recalibrate values that are just beyond April 1 (set 1 month buffer)
buffer = 60

annual_flow_dat_filtered = annual_flow_dat_filtered %>%
  mutate(Min_7_Day_DoY = case_when(Min_7_Day_DoY < buffer ~ 365 + Min_7_Day_DoY,
                                   .default = Min_7_Day_DoY),
         Min_3_Day_DoY = case_when(Min_3_Day_DoY < buffer ~ 365 + Min_3_Day_DoY,
                                   .default = Min_3_Day_DoY),
         Max_3_Day_DoY = case_when(Max_3_Day_DoY < buffer ~ 365 + Max_3_Day_DoY,
                                   .default = Max_3_Day_DoY),
         Max_7_Day_DoY = case_when(Max_7_Day_DoY < buffer ~ 365 + Max_7_Day_DoY,
                                   .default = Max_7_Day_DoY))


# Monthly Values =========================================================

## Here we are only calculating three variables by month.
## i. Average (median) flow
## ii. 7-day Average Low Flow
## iii. values needed for hydrograph:
###     a. 50% quantiles (percentiles?) of normal flow
###     b. 90% quantiles (percentiles?) of normal flow

monthly_mean_dat = flow_dat_filtered |>
  group_by(wYear,Month,STATION_NUMBER) |>
  reframe(median_flow = median(Value,na.rm=T)) %>%
  rename(Year = wYear)

monthly_quantiles_dat = flow_dat_filtered %>%
  group_by(Month,STATION_NUMBER) %>%
  reframe(percentiles = list(quantile(Value, probs = c(0.05,0.25,0.50,0.75,0.95)))) %>%
  unnest_wider(percentiles) |>
  dplyr::rename(five_perc = `5%`, twentyfive_perc = `25%`,
                median_flow = `50%`,
                seventyfive_perc = `75%`, ninetyfive_perc = `95%`)

# Find the 7-day low flows per month.
monthly_7day_lowflow_dat_filtered = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered |>
    filter(STATION_NUMBER == .x) |>
    group_by(STATION_NUMBER,wYear,Month) |>
    ungroup() %>%
    select(-lfDoY)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year','Month'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  min_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day)) %>%
    group_by(wYear,Month) %>%
    slice_min(flow_7_Day) %>%
    group_by(wYear,Month,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Min_7_Day_DoY = wDoY, Min_7_Day_Date = Date,
                  Min_7_Day = flow_7_Day, -Year, -lfYear)

  min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = wYear, Month, everything())
}) %>%
  bind_rows()

monthly_3day_lowflow_dat_filtered = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered |>
    filter(STATION_NUMBER == .x) |>
    group_by(STATION_NUMBER,wYear,Month) |>
    ungroup() %>%
    select(-lfDoY)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year','Month'))

  # Calculate the rolling average with a 'window' of 3 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_3_Day = frollmean(daily_flows_dt[, Value], 3, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  min_3_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_3_Day != lead(flow_3_Day)) %>%
    group_by(wYear,Month) %>%
    slice_min(flow_3_Day) %>%
    group_by(wYear,Month,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Min_3_Day_DoY = wDoY, Min_3_Day_Date = Date,
                  Min_3_Day = flow_3_Day, -Year, -lfYear)

  min_3_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = wYear, Month, everything())
}) %>%
  bind_rows()

# Combine these monthly datasets into 2 files:
## 1. Average flow + low flow (metrics for the app).
## 2. Average flow + quantiles (needed for hydrograph)

monthly_flow_dat_filtered = monthly_mean_dat |>
  left_join(monthly_7day_lowflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month))|>
              dplyr::select(-Min_7_Day_DoY) |>
  left_join(monthly_3day_lowflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month)) |>
              dplyr::select(-Min_3_Day_DoY) |>
  mutate(Month = month.abb[Month]) |>
  dplyr::rename('Average' = median_flow) |>
  dplyr::select(-ends_with("_Date"))

hydrograph_dat = monthly_quantiles_dat |>
  mutate(Month = month.abb[Month])

saveRDS(annual_flow_dat_filtered, 'app/www/annual_flow_dat.rds')
saveRDS(hydrograph_dat, 'app/www/hydrograph_dat.rds')
saveRDS(monthly_flow_dat_filtered,'app/www/monthly_flow_dat.rds')

# # =====================================
# # Combine annual and monthly data, let's see if that works better.
# # Split table by data type: numeric or date. Question: do we need the date vars??
#
# dat_combo = annual_flow_dat_filtered %>%
#   # mutate(Month = 'All') %>%
#   left_join(monthly_mean_dat)
#
# # Remove Date variables. Should we keep them...? Uncertain.
# dat_combo_num = dat_combo %>%
#   dplyr::select(-contains('Date'))
#
# # Experimental: transform Day-of-Year variables such that
# # day 182 (half-way through the year) is the largest possible value,
# # while days 1 and 364 have 182 subtracted from them and the
# # absolute value is taken of the results, such that these 2 dates are
# # very close to each other.
# dat_combo_num = dat_combo_num %>%
#   mutate(DoY_50pct_TotalQ_halfyear_max = abs(DoY_50pct_TotalQ - 182),
#          Min_7_Day_DoY_halfyear_max = abs(Min_7_Day_DoY - 182),
#          Max_7_Day_DoY_halfyear_max = abs(Min_7_Day_DoY - 182))
#
# # Write out dataset at this point - data wide, unsummarised.
# write_csv(dat_combo_num,'app/www/combined_flow_dat_filtered2.csv')

# Get station locations
stations_sf = tidyhydat::hy_stations(station_number = unique(annual_flow_dat_filtered$STATION_NUMBER)) %>%

  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS),
         keep = case_when(is.na(keep) ~ FALSE,
                          .default = TRUE)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS) %>%
  left_join(final_station_summary)

#Hydrologic Zones
hydrozones = read_sf('data/HYDZ_HYDROLOGICZONE_SP/HYD_BC_H_Z_polygon.shp') %>%
  st_transform(crs = st_crs(stations_sf)) %>%
  mutate(HYDZN_NAME = str_to_title(HYDZN_NAME))

write_sf(hydrozones, 'app/www/hydrozones.gpkg')

#spatial join with hydrozones
stations_sf = stations_sf %>%
  st_join(hydrozones)

write_sf(stations_sf, 'app/www/stations.gpkg')


