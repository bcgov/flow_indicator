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



# Preliminary plot
library(plotly)
plot1 <- station.mean %>% plot_ly(type="scatter", x=~Year, y=~Value, showlegend=FALSE) %>%
  add_lines( showlegend=FALSE) %>%
  layout( title=paste0("Station ", station_number),
          yaxis=list(title="Mean daily flow"))
plot1


# restrict the data to 1967 onward
station.mean.restrict <- station.mean[ station.mean$Year >= 1967,]

# the revised plot
plot2 <- station.mean.restrict %>% plot_ly(type="scatter", x=~Year, y=~Value, showlegend=FALSE) %>%
  add_lines( showlegend=FALSE) %>%
  layout( title=paste0("Station ", station_number, " excluding early years with missing data"),
          yaxis=list(title="Mean daily flow "))
plot2

