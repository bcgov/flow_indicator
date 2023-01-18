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

library(StreamFlowTrend)
library(fasstr)
library(EnvStats)
library(tidyverse)

# Use the list of stations that match our filtering requirements to access variables of interest.
# 1. Timing of 50% annual flow.
# 2. Timing of low flow (7-day period)
# 3. Total annual volume.
rm(list = ls())
if(!exists("mean_annual_flow_per_station")){load('./tmp/station_data_cleaned.Rdata')}

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('./www')) dir.create('./www')

if(!file.exists('./www/flow_dat.csv')){
# Calculate annual mean flow, Date of 50% annual flow, 7-day low flow date, and total volume in cubic meters.
annual_mean_dat = calc_annual_stats(station_number = stations_to_keep) %>%
  filter(!is.na(Mean)) %>%
  dplyr::select(Year,STATION_NUMBER,Mean,Median)
gc()

flow_timing_dat = calc_annual_flow_timing(station_number = stations_to_keep, percent_total = 50) %>%
  filter(!is.na(Date_50pct_TotalQ))
gc() #Clear out temporary files - the above operation takes a lot of RAM!

lowflow_dat = calc_annual_lowflows(station_number = stations_to_keep, roll_days = 7) %>%
  filter(!is.na(Min_7_Day_Date))
gc()

totalvolume_dat = calc_annual_cumulative_stats(station_number = stations_to_keep) %>%
  filter(!is.na(Total_Volume_m3))
gc()

flow_dat = annual_mean_dat %>%
  left_join(flow_timing_dat) %>%
  left_join(lowflow_dat) %>%
  left_join(totalvolume_dat)

write.csv(flow_dat, './www/flow_dat.csv', row.names = F)
} else if(file.exists('www/flow_dat.csv')) {flow_dat = read.csv('www/flow_dat.csv')}

# To run MK test, stations must have at least 3 values.
flow_dat_too_little_data = flow_dat %>%
  count(STATION_NUMBER) %>%
  filter(n <= 3) %>%
  pull(STATION_NUMBER)

flow_dat %>%
  filter(!STATION_NUMBER %in% flow_dat_too_little_data) %>%
  summarise(STATION_NUMBER,Year,values = Mean) %>%
  group_by(STATION_NUMBER) %>%
  group_split() %>%
  map(STATION_NUMBER, ~ {
  modifiedmk::mmky1lag(.[.$STATION_NUMBER == .x,]$values)
    })

  ungroup() %>%
  as.data.frame(.) %>%
  data.frame(parameter = row.names(.), .) %>%
  as_tibble() %>%
  pivot_wider(names_from = parameter, values_from = values)
#
#
# gstation_dat = tibble(station_id = stations_to_keep,
#                      half_flow_dat = list(NA),
#                      min7day_flow_dat = list(NA),
#                      totalvolume_dat = list(NA),
#                      results_shortperiod = list(NA),
#                      results_longperiod = list(NA))
#
# for(i in length(stations_to_keep)){
#
#   station_id = stations_to_keep[i]
#
#   print(paste0('working on station ',i,' of ',length(stations_to_keep)))
#
#   for(time_period in c('1990 - present','1967 - present')){
#     flow_timing_dat = calc_annual_flow_timing(station_number = station_id, percent_total = 50) %>%
#       filter(!is.na(Date_50pct_TotalQ))
#     if(time_period == '1990 - present'){flow_timing_dat = flow_timing_dat %>% filter(Year >= 1990)}
#     mmky.test <- mmky1lag(flow_timing_dat$DoY_50pct_TotalQ)
#     HalfFlowMK = data.frame(statistic = row.names(as.data.frame(mmky.test)),
#                             value = as.numeric(mmky.test)) %>%
#       mutate(station_id = station_id,
#              parameter = 'Date_50pct_TotalQ',
#              period = time_period)
#
#     lowflow_dat = calc_annual_lowflows(station_number = station_id, roll_days = 7) %>%
#       filter(!is.na(Min_7_Day_Date))
#     if(time_period == '1990 - present'){lowflow_dat = lowflow_dat %>% filter(Year >= 1990)}
#     mmky.test <- mmky1lag(lowflow_dat$Min_7_Day_DoY)
#     Min7DayMK = data.frame(statistic = row.names(as.data.frame(mmky.test)),
#                            value = as.numeric(mmky.test)) %>%
#       mutate(station_id = station_id,
#              parameter = 'Min_7_Day_DoY',
#              period = time_period)
#
#     totalvolume_dat = calc_annual_cumulative_stats(station_number = station_id) %>%
#       filter(!is.na(Total_Volume_m3))
#     if(time_period == '1990 - present'){totalvolume_dat = totalvolume_dat %>% filter(Year >= 1990)}
#     mmky.test <- mmky1lag(totalvolume_dat$Total_Volume_m3)
#     TotalVolMK = data.frame(statistic = row.names(as.data.frame(mmky.test)),
#                             value = as.numeric(mmky.test)) %>%
#       mutate(station_id = station_id,
#              parameter = 'Total_Volume_m3',
#              period = time_period)
#
#     if(time_period == '1967 - present') {
#       station_dat[i,]$results_longperiod = list(bind_rows(HalfFlowMK, Min7DayMK, TotalVolMK))
#     }
#     if(time_period == '1990 - present') station_dat[i,]$results_shortperiod = list(bind_rows(HalfFlowMK, Min7DayMK, TotalVolMK))
#   }
# }

# Above code 9:19 AM ->
# # Least Squares trend line.
# lm.fit <- lm(Value ~ Year, data=station.mean)
# summary(lm.fit)$coefficients
#
# # Mann-Kendall trend test.
# mk.test <- kendallTrendTest(Value~Year, data=station.mean)
# mk.test
# cat("Confidence interval for the slope \n")
# mk.test$interval$limits
#
# # Tau is the MK correlation between the year and value variables.
# cor.test(station.mean$Value, station.mean$Year, method="kendall")
#
# # "Linear" trend line of Senn slope.
# station.mean$mk.pred <- mk.test$estimate["intercept"]+
#   mk.test$estimate["slope"]*station.mean$Year
#
# plot1 %>% add_trace(data=station.mean, x=~Year, y=~mk.pred, mode="lines")
#
# # Note: we may prefer to use a version of the MK test that accounts for
# # serial autocorrelation. This function shows the old and new p values and
# # other parameters of the model fit.
# mmky.test <- modifiedmk::mmky1lag(station.mean$Value)
# mmky.test
#
# # We could also use a 'robust' linear regression model.
# lmrob.fit <- robustbase::lmrob(Value ~ Year, data=station.mean)
# summary(lmrob.fit)$coefficients
#
# station.mean$lmrob.pred <- predict(lmrob.fit, newdata=station.mean)
#
# plot1 %>% add_trace(data=station.mean, x=~Year, y=~lmrob.pred, mode="lines")
