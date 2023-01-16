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

library(StreamFlowTrend)
# library(broom)
# library(broom.mixed)
# library(nlme)
library(tidyverse)
library(tidyhydat)
library(magrittr)
# library(plotly)

# The following code is from Carl's StreamFlowTrend work (script='v01-individual-station-analysis')

trend.stat.names <- c("estimate","se","p.value","rho","se.adj","p.value.adj")

# Picking single station to develop data access pipeline
station_number <- '08NM116'
station.info <- hy_stations( station_number)

# get the bc_stations data
bc_stations <- hy_stations( prov_terr_state_loc="BC")

# What are available statistics?
stfl_get_avail_stat()

# Note: avoid the maximum/minimum statistics

hydata <- tidyhydat::hy_daily_flows(station_number = station_number)
hydata$Year <- lubridate::year(hydata$Date)
temp <- as.data.frame(xtabs(~Year, data=hydata, exclude=NULL, na.action=na.pass))
temp$Year <- as.numeric(as.character(temp$Year)) # convert from factor
temp$Decade <- floor(temp$Year/10)*10
temp$Year.in.Decade = temp$Year %% 10
xtabs(Freq ~ Decade+Year.in.Decade, data=temp)

# We see that for this station, complete (or nearly complete) daily data is only
# available starting around 1967. Caution will be needed to interpret any
# long-term trends that include data from 1949 to 1965 that excludes about
# 1/2 of the year.

# get the mean daily flow
station.mean <- stfl_get_annual_stat(station_number, Parameter="Flow", Statistic=c("MEAN"), ignore_missing=TRUE)
head(station.mean)
tail(station.mean)

