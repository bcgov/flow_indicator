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
library(fasstr)
library(rmapshaper)

if(!exists("final_station_summary_wYear")){final_station_summary_wYear = read_csv('data/finalstns_wYear.csv')}
if(!exists("final_station_summary_lfYear")){final_station_summary_lfYear = read_csv('data/finalstns_lfYear.csv')}
if(!exists("final_station_summary_cYear")){final_station_summary_cYear = read_csv('data/finalstns_cYear.csv')}
if(!exists("filtered_station_year_wYear")){filtered_station_year_wYear = read_csv('data/finalstnyr_wYear.csv')}
if(!exists("filtered_station_year_lfYear")){filtered_station_year_lfYear = read_csv('data/finalstnyr_lfYear.csv')}
if(!exists("filtered_station_year_cYear")){filtered_station_year_cYear = read_csv('data/finalstnyr_cYear.csv')}

# Load regime group table
regime_groups = read.csv("data/river_groups.csv")

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Pull out the stations to keep from the loading script
stations_to_keep = final_station_summary_wYear %>%
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
  mutate(Year = year(Date),
         wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1)) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  left_join(regime_groups)

#Restrict by station_year list (this will remove years with missing data) - water year
flow_dat_filtered_wYear = flow_dat %>%
  filter(paste0(STATION_NUMBER, wYear) %in% paste0(filtered_station_year_wYear$STATION_NUMBER,filtered_station_year_wYear$Year)) %>%
  mutate(DoY = case_when(month(Date) >= 10 ~ yday(Date) - yday(paste0(year(Date),"-09-30")),
                          month(Date) < 10 ~ yday(Date) + (365 - yday(paste0(year(Date),"-09-30"))))) %>%
  dplyr::select(-lfYear, -Year)

# Low flow year
flow_dat_filtered_lfYear = flow_dat %>%
  filter(paste0(STATION_NUMBER, lfYear) %in% paste0(filtered_station_year_lfYear$STATION_NUMBER,filtered_station_year_lfYear$Year)) %>%
  mutate(DoY = case_when(month(Date) >= 4 ~ yday(Date) - yday(paste0(year(Date),"-03-31")),
                         month(Date) < 4 ~ yday(Date) + (365 - yday(paste0(year(Date),"-03-31"))))) %>%
  dplyr::select(-wYear, -Year)

#Calendar year
flow_dat_filtered_cYear = flow_dat %>%
  mutate(Year = year(Date)) %>%
  filter(paste0(STATION_NUMBER, Year) %in% paste0(filtered_station_year_cYear$STATION_NUMBER,filtered_station_year_cYear$Year)) %>%
  mutate(DoY = yday(Date)) %>%
  dplyr::select(-wYear, -lfYear)


# Annual Values =========================================================
# Use water year (October - October)
annual_mean_dat = flow_dat_filtered_wYear %>%
  group_by(wYear,STATION_NUMBER) %>%
  summarise(Average = mean(Value)) %>%
  ungroup()%>%
  filter(wYear > 1915) %>%# remove some random early year that looks to not have been removed by gap code (no idea why!)
  rename(Year = wYear)

# Check data gaps are gone
ggplot(annual_mean_dat, aes(Year,STATION_NUMBER, colour = Average)) +
  geom_point()

# # Calculate "annual" mean for mixed regime stations (March - September)
# annual_mean_dat_summer = flow_dat_filtered_cYear %>%
#   filter(between(Month, 3, 9)) %>%
#   group_by(Year, STATION_NUMBER) %>%
#   summarise(Average = mean(Value)) %>%
#
# # Calculate annual mean using calendar year for Late peak stations
# annual_mean_dat_cYear = flow_dat_filtered_cYear %>%
#   group_by(Year, STATION_NUMBER) %>%
#   summarise(Average = mean(Value)) %>%
#   ungroup()%>%
#   filter(wYear > 1915) # remove some random early year that looks to not have been removed by gap code (no idea why!)


# Timing of Freshet =========================================================
# Use water year (October - October)
flow_timing_earlypeak = flow_dat_filtered_wYear %>%
  filter(Regime == "Snow-Dominated - Early Peak") %>%
  group_by(STATION_NUMBER, wYear) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-10-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year = wYear,
                DoY_50pct_TotalQ,
                Date)

flow_timing_latepeak = flow_dat_filtered_cYear %>%
  filter(Regime == "Snow-Dominated - Late Peak") %>%
  group_by(STATION_NUMBER, Year) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-01-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year,
                DoY_50pct_TotalQ,
                Date)

flow_timing_mixed = flow_dat_filtered_cYear %>%
  filter(Regime == "Mixed Regimes" & between(Month, 3, 9)) %>%
  group_by(STATION_NUMBER,Year)%>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-01-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year,
                DoY_50pct_TotalQ,
                Date)

flow_timing_dat = bind_rows(flow_timing_earlypeak,
                            flow_timing_latepeak,
                            flow_timing_mixed)

# Return to 50% Mean Annual Discharge =========================================================

# This is a form of low flow so will use the low flow years
#
#
# # Lets look at a few stations-years to see where mean annual discharge ends up
# # flow_dat_filtered_lfYear %>%
# #   group_by(STATION_NUMBER) %>%
# #   mutate(mad = mean(Value)) %>%
# #   filter(STATION_NUMBER == "07EC002" & lfYear == 1980) %>%
# #   ggplot() +
# #   geom_point(aes(x = DoY, y = Value)) +
# #   scale_x_continuous(breaks = seq(0,365,40), labels = seq(0,365,40)) +
# #   geom_hline(aes(yintercept = mad)) +
# #   geom_hline(aes(yintercept = mad * mad_threshold))

MAD_station = calc_longterm_mean(flow_dat_filtered_lfYear,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 percent_MAD = 50)

MAD_station_rain = calc_longterm_mean(flow_dat_filtered_lfYear,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 percent_MAD = 20)

rtn_2_mad_perc_rain = flow_dat_filtered_lfYear %>%
  filter(Regime == "Rain-Dominated") %>%
  left_join(MAD_station) %>%
  group_by(STATION_NUMBER) %>%
  mutate(below_mad_perc = case_when (Value <= '20%MAD' ~ 1,
                                     .default = 0)) %>%
  group_by(STATION_NUMBER, lfYear) %>%
  mutate(peak_date = DoY[which.max(Value)]) %>%
  filter(below_mad_perc == 1 & DoY>peak_date) %>%
  arrange(Date) %>%
  slice(1) %>%
  dplyr::select(STATION_NUMBER,
                Year = lfYear,
                R2MAD_DoY = DoY)

rtn_2_mad_perc_rest = flow_dat_filtered_lfYear %>%
  filter(Regime != "Rain-Dominated") %>%
  left_join(MAD_station) %>%
  group_by(STATION_NUMBER) %>%
  mutate(below_mad_perc = case_when (Value <= '50%MAD' ~ 1,
                                     .default = 0)) %>%
  group_by(STATION_NUMBER, lfYear) %>%
  mutate(peak_date = DoY[which.max(Value)]) %>%
  filter(below_mad_perc == 1 & DoY>peak_date) %>%
  arrange(Date) %>%
  slice(1) %>%
  dplyr::select(STATION_NUMBER,
                Year = lfYear,
                R2MAD_DoY = DoY)

rtn_2_mad_perc = bind_rows(rtn_2_mad_perc_rain,
                           rtn_2_mad_perc_rest)


#Filter out NAs (i.e. never returned to 50% MAD within that year)
nas_MADs = annual_mean_dat %>%
  left_join(rtn_2_mad_perc) %>%
  filter(is.na(R2MAD_DoY))

#Apply above, but ignore requirement for it to happen after freshet
#Alternatively, apply it to water year instead of low flow year
# rtn_2_mad_perc2 = flow_dat_filtered_wYear %>%
#   filter(paste0(STATION_NUMBER,wYear) %in% paste0(nas_MADs$STATION_NUMBER,nas_MADs$Year)) %>%
#   group_by(STATION_NUMBER) %>%
#   mutate(mad = mean(Value)) %>%
#   mutate(mad_perc = mad * mad_threshold) %>%
#   mutate(below_mad_perc = case_when (Value <= mad_perc ~ 1,
#                                      .default = 0)) %>%
#   group_by(STATION_NUMBER, wYear) %>%
#   mutate(peak_date = DoY[which.max(Value)]) %>%
#   filter(below_mad_perc == 1& DoY>peak_date) %>%
#   arrange(Date) %>%
#   slice(1) %>%
#   dplyr::select(STATION_NUMBER,
#                 Year = wYear,
#                 R2MAD_DoY_50 = DoY
#   )
#
# # Doeasnt really work for a bunch but better than nothing
# rtn_2_mad_perc = bind_rows(rtn_2_mad_perc, rtn_2_mad_perc2)

# rtn_2_mad_perc = rtn_2_mad_perc %>%
#   left_join(rtn_2_mad_perc2)

dir.create(file.path("./pngs"), showWarnings = FALSE)

# unique(flow_timing_dat$STATION_NUMBER) %>%
#   map ( ~ {
#     dir.create(file.path(paste0("./pngs/",.x)), showWarnings = FALSE)
#     rtn_2_mad_perc %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Low_flow_Date = as.Date(R2MAD_DoY, origin = "2000-04-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Low_flow_Date)) +
#       scale_y_date(date_labels = "%b-%d", breaks = "2 weeks")
#
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_mad50perc.png"))
#   })

# Try using same approach as with freshet but a higher percentage of flow (may not work due to the huge variability between years of speed of snowmelt)
# low_perc = 0.8
#
# low_flow_timing_dat = flow_dat_filtered_lfYear %>%
#   filter(paste0(STATION_NUMBER, lfYear) %in% paste0(filtered_station_year_lfYear$STATION_NUMBER,filtered_station_year_lfYear$Year)) %>%
#   group_by(STATION_NUMBER, lfYear) %>%
#   mutate(RowNumber = row_number(),
#          TotalFlow = sum(Value),
#          FlowToDate = cumsum(Value)) %>%
#   filter(FlowToDate > TotalFlow * low_perc) %>%
#   slice(1) %>%
#   mutate(DoY_90pct_TotalQ = DoY,
#          Date = Date) %>%
#   ungroup() %>%
#   dplyr::select(STATION_NUMBER,
#                 Year = lfYear,
#                 DoY_90pct_TotalQ
#   )

#plot freshet results

# unique(flow_timing_dat$STATION_NUMBER) %>%
#   map ( ~ {
#     dir.create(file.path(paste0("./pngs/",.x)), showWarnings = FALSE)
#     flow_timing_dat %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Freshet_Date = as.Date(DoY_50pct_TotalQ, origin = "2000-10-01")) %>%
#     ggplot() +
#       geom_point(aes(x = Year, y = Freshet_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#       ggsave(filename = paste0("./pngs/",.x,"/",.x,"freshet.png"))
#   })

# unique(low_flow_timing_dat$STATION_NUMBER) %>%
#   map ( ~ {
#     dir.create(file.path(paste0("./pngs/",.x)), showWarnings = FALSE)
#     low_flow_timing_dat %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Low_flow_Date = as.Date(DoY_90pct_TotalQ, origin = "2000-04-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Low_flow_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"low_flow.png"))
#   })

# 7-day Summer Low flow (flow value, day of year, Date)
# Note: this is SUMMER low flow (i.e. May - October)

low_flow_dat_filtered_7day = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered_lfYear |>
    filter(STATION_NUMBER ==.x)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','lfYear'))

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
    filter(flow_7_Day != lead(flow_7_Day) & between(Month, 6, 10)) %>%
    group_by(lfYear) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_7_Day_summer_DoY = DoY,
                  Min_7_Day_summer_Date = Date, Min_7_Day_summer = flow_7_Day)

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
    dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_7_Day_DoY = DoY,
                  Min_7_Day_Date = Date, Min_7_Day = flow_7_Day)

  low_flows = min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything()) %>%
    left_join(summer_low_flows, by = join_by(STATION_NUMBER, Year))

  # # Find Peak flows (7-day average flows, could be any time of year)
  # max_7_day_dat = daily_flows_dt %>%
  #   as_tibble() %>%
  #   # Missing data can produce identical minimum flow values.
  #   # Keep only the latest record for each such duplication.
  #   group_by(wYear) %>%
  #   slice_max(flow_7_Day) %>%
  #   group_by(wYear,flow_7_Day) %>%
  #   slice(1) %>%
  #   ungroup() %>%
  #   dplyr::select(-Parameter,-Value,-Symbol, -Month,
  #                 Max_7_Day_DoY = wDoY, Max_7_Day_Date = Date,
  #                 Max_7_Day = flow_7_Day, -lfYear, -Year)%>%
  #   dplyr::select(- lfDoY, Year = wYear)
  #
  # low_flows %>%
  #   left_join(max_7_day_dat,
  #             by = join_by(STATION_NUMBER, Year))
}) %>%
  bind_rows()

high_flow_dat_filtered_3day = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered_wYear |>
    filter(STATION_NUMBER ==.x)
  # group_by(STATION_NUMBER,Year) |#>
  # mutate(my_row = row_number()) |>
  # ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','wYear'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_3_Day = frollmean(daily_flows_dt[, Value], 3, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  # Filter for just summer months...
  # daily_flows_dt_summer = daily_flows_dt %>%
  #  filter(Month %in% c(5:9))

  # min_3_day_dat = daily_flows_dt %>%
  #   as_tibble() %>%
  #   # Missing data can produce identical minimum flow values.
  #   # Keep only the latest record for each such duplication.
  #   filter(flow_3_Day != lead(flow_3_Day) & between(Month, 4, 10)) %>%
  #   group_by(lfYear) %>%
  #   slice_min(flow_3_Day) %>%
  #   group_by(lfYear,flow_3_Day) %>%
  #   slice(1) %>%
  #   ungroup() %>%
  #   dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_3_summer_Day_DoY = lfDoY,
  #                 Min_3_Day_summer_Date = Date, Min_3_summer_Day = flow_3_Day, -wYear, -Year)%>%
  #   dplyr::select(-wDoY)
  #
  # summer_low_flows = min_3_day_dat %>%
  #   dplyr::select(STATION_NUMBER, Year = lfYear, everything())
  #
  # min_3_day_dat = daily_flows_dt %>%
  #   as_tibble() %>%
  #   # Missing data can produce identical minimum flow values.
  #   # Keep only the latest record for each such duplication.
  #   filter(flow_3_Day != lead(flow_3_Day)) %>%
  #   group_by(lfYear) %>%
  #   slice_min(flow_3_Day) %>%
  #   group_by(lfYear,flow_3_Day) %>%
  #   slice(1) %>%
  #   ungroup() %>%
  #   dplyr::select(-Parameter,-Value,-Symbol, -Month, Min_3_Day_DoY = lfDoY,
  #                 Min_3_Day_Date = Date, Min_3_Day = flow_3_Day, -wYear, -Year)%>%
  #   dplyr::select(-wDoY)
  #
  # low_flows = min_3_day_dat %>%
  #   dplyr::select(STATION_NUMBER, Year = lfYear, everything()) %>%
  #   left_join(summer_low_flows, by = join_by(STATION_NUMBER, Year))

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
                  Max_3_Day_DoY = DoY, Max_3_Day_Date = Date,
                  Max_3_Day = flow_3_Day, Year = wYear)

  # low_flows %>%
  #   left_join(max_3_day_dat,
  #             by = join_by(STATION_NUMBER, Year))
}) %>%
  bind_rows()

# Do same for low and high flow as for freshet
# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     low_flow_dat_filtered_7day %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Low_flow_Date = as.Date(Min_7_Day_DoY, origin = "2000-10-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Low_flow_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_7day.png"))
#   })

# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     low_flow_dat_filtered_7day %>%
#       filter(STATION_NUMBER == .x) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Min_7_Day_DoY))+
#       scale_y_continuous(breaks = c(min(Min_7_Day_DoY), max(Min_7_Day_DoY)),#,
#                          # # labels = c("Earlier", "Later"))
#                          labels = c(as.Date(min(Min_7_Day_DoY), origin = "2000-10-01"),
#                                     as.Date(max(Min_7_Day_DoY), origin = "2000-10-01")))
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_7day.png"))
#   })

# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     low_flow_dat_filtered_7day %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Summer_Low_flow_Date = as.Date(Min_7_Day_summer_DoY, origin = "2000-10-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Summer_Low_flow_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_7day_july.png"))
#   })

# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     low_flow_dat_filtered_7day %>%
#       filter(STATION_NUMBER == .x) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Min_7_Day_summer_DoY))+
#       scale_y_continuous(breaks = c(min(Min_7_Day_summer_DoY), max(Min_7_Day_summer_DoY)),#,
#                          # # labels = c("Earlier", "Later"))
#                          labels = c(as.Date(min(Min_7_Day_summer_DoY), origin = "2000-10-01"),
#                                     as.Date(max(Min_7_Day_summer_DoY), origin = "2000-10-01")))
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_7day_july.png"))
#   })

# unique(high_flow_dat_filtered_3day$STATION_NUMBER) %>%
#   map ( ~ {
#     high_flow_dat_filtered_3day %>%
#       filter(STATION_NUMBER == .x) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Max_3_Day_DoY))
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_hf_3day.png"))
#   })

annual_flow_dat_filtered = annual_mean_dat  |>
  left_join(flow_timing_dat) |>
  # left_join(low_flow_timing_dat) %>%
  left_join(rtn_2_mad_perc) %>%
  left_join(low_flow_dat_filtered_7day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year")) |>
  left_join(high_flow_dat_filtered_3day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year")) |>
  dplyr::select(-ends_with("_Date"))

#Recalibrate values that are just beyond April 1 (set buffer)
# buffer = 30
#
# annual_flow_dat_filtered = annual_flow_dat_filtered %>%
#   mutate(Min_7_Day_summer_DoY = case_when(Min_7_Day_summer_DoY < buffer ~ 365 + Min_7_Day_summer_DoY,
#                                    .default = Min_7_Day_summer_DoY),
#          Max_3_Day_DoY = case_when(Max_3_Day_DoY < buffer ~ 365 + Max_3_Day_DoY,
#                                    .default = Max_3_Day_DoY))
#
# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     annual_flow_dat_filtered %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Low_flow_Date = as.Date(Min_7_Day_DoY, origin = "2000-10-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Low_flow_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_",buffer,".png"))
#   })

# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     annual_flow_dat_filtered %>%
#       filter(STATION_NUMBER == .x) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Min_7_Day_DoY))+
#       scale_y_continuous(breaks = c(min(Min_7_Day_DoY), max(Min_7_Day_DoY)),#,
#                          # # labels = c("Earlier", "Later"))
#                          labels = c(as.Date(min(Min_7_Day_DoY), origin = "2000-10-01"),
#                                     as.Date(max(Min_7_Dayr_DoY), origin = "2000-10-01")))
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_",buffer,".png"))
#   })

# unique(low_flow_dat_filtered_7day$STATION_NUMBER) %>%
#   map ( ~ {
#     annual_flow_dat_filtered %>%
#       filter(STATION_NUMBER == .x) %>%
#       mutate(Summer_Low_flow_Date = as.Date(Min_7_Day_summer_DoY, origin = "2000-10-01")) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Summer_Low_flow_Date))+
#       scale_y_date(date_labels = "%b-%d", date_breaks = "2 weeks")
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_lf_",buffer,"_july.png"))
#   })

# unique(high_flow_dat_filtered_3day$STATION_NUMBER) %>%
#   map ( ~ {
#     annual_flow_dat_filtered %>%
#       filter(STATION_NUMBER == .x) %>%
#       ggplot() +
#       geom_point(aes(x = Year, y = Max_3_Day_DoY))
#     ggsave(filename = paste0("./pngs/",.x,"/",.x,"_hf_",buffer, ".png"))
#   })

# Monthly Values =========================================================

## Here we are only calculating three variables by month.
## i. Average (median) flow
## ii. 7-day Average Low Flow
## iii. values needed for hydrograph:
###     a. 50% quantiles (percentiles?) of normal flow
###     b. 90% quantiles (percentiles?) of normal flow

monthly_mean_dat_filtered = flow_dat_filtered_wYear %>%
  group_by(wYear,Month,STATION_NUMBER) %>%
  mutate(perc_na = ((days_in_month(Month)-n())/days_in_month(Month))*100) %>%
  filter(perc_na<10) %>%
reframe(median_flow = median(Value,na.rm=T),
        mean_flow = mean(Value, na.rm = T)) %>%
rename(Year = wYear)

monthly_quantiles_dat = flow_dat_filtered_wYear %>%
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
  daily_flows = flow_dat_filtered_lfYear |>
    filter(STATION_NUMBER == .x) |>
    group_by(STATION_NUMBER,lfYear,Month) |>
    ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','lfYear','Month'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  min_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day)) %>%
    group_by(lfYear,Month) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,Month,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Min_7_Day_DoY = DoY, Min_7_Day_Date = Date,
                  Min_7_Day = flow_7_Day)

  min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Month, Year = lfYear, everything())
}) %>%
  bind_rows()

monthly_3day_highflow_dat_filtered = stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows = flow_dat_filtered_wYear |>
    filter(STATION_NUMBER == .x) |>
    group_by(STATION_NUMBER, wYear, Month) |>
    ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','wYear','Month'))

  # Calculate the rolling average with a 'window' of 3 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  daily_flows_dt$flow_3_Day = frollmean(daily_flows_dt[, Value], 3, align = 'right', na.rm=T)

  # Convert back from data.table object to a dataframe, and clean it up.

  max_3_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_3_Day != lead(flow_3_Day)) %>%
    group_by(wYear,Month) %>%
    slice_max(flow_3_Day) %>%
    group_by(wYear,Month,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Max_3_Day_DoY = DoY, Max_3_Day_Date = Date,
                  Max_3_Day = flow_3_Day)

  max_3_day_dat %>%
    dplyr::select(STATION_NUMBER, Month, Year = wYear, everything())
}) %>%
  bind_rows()

# Combine these monthly datasets into 2 files:
## 1. Average flow + low flow (metrics for the app).
## 2. Average flow + quantiles (needed for hydrograph)

monthly_flow_dat_filtered = monthly_mean_dat_filtered |>
  left_join(monthly_7day_lowflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month))|>
              dplyr::select(-Min_7_Day_DoY) |>
  left_join(monthly_3day_highflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month)) |>
              dplyr::select(-Max_3_Day_DoY) |>
  mutate(Month = month.abb[Month]) |>
  dplyr::rename('Average' = mean_flow) |>
  dplyr::select(-ends_with("_Date"))

hydrograph_dat = monthly_quantiles_dat |>
  mutate(Month = month.abb[Month])

#calculate Mean Annual Discharge and add to hydrograph data
hydrograph_dat = flow_dat_filtered_wYear %>%
  group_by(STATION_NUMBER) %>%
  mutate(MAD = mean(Value)) %>%
  dplyr::select(STATION_NUMBER, MAD) %>%
  distinct() %>%
  left_join(hydrograph_dat)

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
  left_join(final_station_summary_wYear)

# BC Boundaries
bound <- bcmaps::bc_bound_hres()%>%
  st_as_sf(crs = 4326)

# WSC Basins and clip to BC (remove Alberta and territory basins)
wsc_basins <- bcmaps::wsc_drainages()%>%
  filter(MAJOR_DRAINAGE_AREA_CD != "05",
         !SUB_SUB_DRAINAGE_AREA_CD %in% c("07AA","07AB",
                                          "10ED","10FA")) %>%
  st_as_sf(crs = 4326) %>%
  st_intersection(bound) %>%
  st_transform(crs = 4326)

# View the map
mapview::mapview(wsc_basins, zcol = "SUB_SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "MAJOR_DRAINAGE_AREA_CD")


# Group subbasins to create major and sub-basin groups
trending_basins <- wsc_basins  %>%
  mutate(Major_Basin = case_when(SUB_DRAINAGE_AREA_CD == "08N" ~ "Columbia",
                                 SUB_SUB_DRAINAGE_AREA_CD %in% c("08MH") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("08M","08L","08K","08J") ~ "Fraser",
                                 SUB_DRAINAGE_AREA_CD %in% c("08H","08G","08F","08O") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("07F","07E") ~ "Peace",
                                 SUB_DRAINAGE_AREA_CD %in% c("10A","10B","10C","10D","10E","10F") ~ "Liard",
                                 SUB_DRAINAGE_AREA_CD %in% c("08A","08B","08C","08D","08E","09A") ~ "Northwest"),
         Sub_Basin = case_when(SUB_SUB_DRAINAGE_AREA_CD == "08NL" ~ "Similkameen",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NM" ~ "Okanagan",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NN" ~ "Kettle",
                               #SUB_SUB_DRAINAGE_AREA_CD %in% c("08NL","08NM","08NN") ~ "Similkameen-Okanagan-Kettle",
                               SUB_DRAINAGE_AREA_CD == "08H" ~ "Vancouver Island",
                               SUB_DRAINAGE_AREA_CD == "08O" ~ "Haida Gwaii",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GD","08GC","08GB","08GA","08MH") ~ "South Coast",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GF","08GE") ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08F" ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08D" ~ "Nass",
                               SUB_DRAINAGE_AREA_CD == "08J" ~ "Nechako",
                               SUB_DRAINAGE_AREA_CD == "08E" ~ "Skeena",
                               SUB_DRAINAGE_AREA_CD == "08C" ~ "Stikine",
                               SUB_DRAINAGE_AREA_CD == "09A" ~ "Yukon",
                               SUB_DRAINAGE_AREA_CD == "08L" ~ "Thompson",
                               SUB_DRAINAGE_AREA_CD == "07F" ~ "Upper Peace",
                               SUB_DRAINAGE_AREA_CD == "07E" ~ "Williston Lake",
                               SUB_DRAINAGE_AREA_CD == "10C" ~ "Fort Nelson",
                               SUB_DRAINAGE_AREA_CD %in% c("10A","10B") ~ "Upper and Central Liard",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08KA","08KB","08KD","08KC","08KE","08KG","08KF") ~ "Upper Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MD","08ME","08MG","08MF","08MC") ~ "Lower Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NC","08NB","08NA","08ND","08NE") ~ "Columbia",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MB","08MA") ~ "Chilcotin",
                               SUB_SUB_DRAINAGE_AREA_CD == "08KH" ~ "Quesnel",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NG","08NF","08NK","08NP",
                                                               "08NH", "08NJ") ~ "Kootenay"))
# make simple table for viewing data
trending_basins_table <- trending_basins %>%
  st_drop_geometry()

# view the basin groupings by all basins
mapview::mapview(trending_basins, zcol = "Major_Basin")
mapview::mapview(trending_basins, zcol = "Sub_Basin")

# Merge major basins and view features
major_basins <- trending_basins %>%
  group_by(Major_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()%>%
  ms_simplify()
mapview::mapview(list(major_basins), zcol = c("Major_Basin"))

# Merge sub basins and view features
sub_basins <- trending_basins %>%
  group_by(Sub_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  filter(!is.na(Sub_Basin)) %>%
  ungroup() %>%
  ms_simplify()
mapview::mapview(list(sub_basins), zcol = c("Sub_Basin"))

#Merge major basins and sub basins
basins = sub_basins %>%
  st_intersection(major_basins) %>%
  ms_simplify()
mapview::mapview(list(basins), zcol = c("Sub_Basin"))

#spatial join with basins
stations_sf = stations_sf %>%
  st_intersection(basins) %>%
  left_join(regime_groups)

write_sf(stations_sf, 'app/www/stations.gpkg')
write_sf(basins, 'app/www/basins.gpkg')
write_sf(sub_basins, 'app/www/sub_basins.gpkg')
write_sf(major_basins, 'app/www/major_basins.gpkg')

