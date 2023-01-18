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
library(lubridate)
library(tidyhydat)
library(magrittr)
# library(plotly)

# If this is the first time this script is being run, create a 'tmp' folder to place
# intermediate results and data.
if(!dir.exists('tmp')){dir.create('tmp')}

# If you have not downloaded the hydrological database using the {tidyhydat} package,
# do this now. It is ~260 MB large.
tidyhydat::download_hydat()



# The following code is from Carl's StreamFlowTrend work (script='v01-individual-station-analysis')

# Select which statistics to view.
trend.stat.names <- c("estimate","se","p.value","rho","se.adj","p.value.adj")

# get the bc_stations data
bc_stations <- hy_stations( prov_terr_state_loc="BC")

# Create station list. It is possible to exclude stations at this point.
station_list = bc_stations

# For each of the stations in the station_list object, attempt to read in
# daily flow recordings from the hydat database (need to have this locally installed)
# and summarise the number of daily readings per year for each station.

number_daily_records_per_station = station_list %>%
  summarise(
    map_dfr(STATION_NUMBER, ~ {
      tryCatch(
        expr = hy_daily_flows(station_number = .x) %>%
          mutate(Year = year(Date)) %>%
          group_by(Year) %>%
          summarise(DaysWithData = n()) %>%
          mutate(STATION_NUMBER = .x) %>%
          dplyr::select(STATION_NUMBER,everything()),
        error = function(e) {
          print(paste0("error - no data available for station ", .x))
          data.frame(STATION_NUMBER = .x, Year = 1960, DaysWithData = 0)
        }
      )
    })
  )

mean_annual_flow_per_station = stfl_get_annual_stat(station_list$STATION_NUMBER, Parameter = 'Flow', Statistic = c("MEAN"), ignore_missing = TRUE)
# Takes 30+ minutes

# mean_annual_flow_per_station = station_list %>%
#   summarise(
#     map_dfr(STATION_NUMBER, ~ {
#       tryCatch(
#         expr = stfl_get_annual_stat(.x, Parameter="Flow", Statistic=c("MEAN"), ignore_missing=TRUE) %>%
#           mutate(STATION_NUMBER = .x) %>%
#           dplyr::select(STATION_NUMBER,everything()),
#         error = function(e) {
#           print(paste0("error - no data available for station ", .x))
#           data.frame(STATION_NUMBER = .x)
#         }
#       )
#     })
#   )
## Takes about 3:55 PM ->

save(station_list, number_daily_records_per_station, mean_annual_flow_per_station, file = './tmp/station_data.Rdata')
