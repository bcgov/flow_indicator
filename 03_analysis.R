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
if(!exists("number_daily_records_per_station")){load('./tmp/station_data_cleaned.Rdata')}

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('./www')) dir.create('./www')

# The below code chunk looks for a flow_dat.csv summary file in the /www folder.
# If this file cannot be found, the {fasstr} functions are used to generate
# fields for each station and year of variables like mean/median flow per year,
# day of year by which 50% of flow has passed, 7-day flow minimum, day of year
# of 7-day flow minimum, and total annual flow. This code chunk takes about 12:00 to
# to run.

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
}
