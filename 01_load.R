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

# library(StreamFlowTrend)
library(tidyverse)
library(lubridate)
library(tidyhydat)
library(magrittr)

# If this is the first time this script is being run, create a 'tmp' folder to place
# intermediate results and data.
if(!dir.exists('out')){dir.create('out')}

# If you have not downloaded the hydrological database using the {tidyhydat} package,
# do this now. It is ~260 MB large.
tidyhydat::download_hydat()

# The following code is from Carl's StreamFlowTrend work (script='v01-individual-station-analysis')

# Get list of BC stations
bc_stations <- hy_stations( prov_terr_state_loc="BC")

# Remove stations that are 'regulated', i.e., rivers that are highly
# modified and thus their flow readings don't make sense in this analysis.
bc_stations = bc_stations %>%
  filter(!STATION_NUMBER %in% (hy_stn_regulation(
    bc_stations$STATION_NUMBER) %>%
      filter(REGULATED == T) %>%
      pull(STATION_NUMBER)
  )
  )

# Potential filtering step: only include stations that are within X KM (e.g. 50)
# from base stations.
# Create station list. It is possible to exclude stations at this point.
station_list = bc_stations

# For each of the stations in the station_list object, attempt to read in
# daily flow recordings from the hydat database (need to have this locally installed)
# and summarise the number of daily readings per year for each station.

# This step takes about ten minutes (depending on computer and internet connection)
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

save(station_list, number_daily_records_per_station, file = './out/station_data.Rdata')
