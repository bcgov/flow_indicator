# Copyright 2024 Province of British Columbia
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

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)
library(cowplot)
library(envreportutils)
library(webshot)
library(htmlwidgets)
library(ggpubr)
library(leaflet)

#Load data
if(!exists("hydrozones")){hydrozones = st_read('app/www/hydrozones.gpkg')}
if(!exists("basins")){basins = st_read('app/www/basins.gpkg')}
if(!exists("sub_basins")){sub_basins = st_read('app/www/sub_basins.gpkg')}
if(!exists("major_basins")){major_basins = st_read('app/www/major_basins.gpkg')}

if(!exists("stations")){stations = st_read('app/www/stations.gpkg')}

if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}
if(!exists("monthly_flow_dat")){monthly_flow_dat = readRDS('app/www/monthly_flow_dat.rds')}

if(!exists("regime_groups")){regime_groups = read.csv('app/www/river_groups.csv')}

# #transform basins CRS
# basins = st_set_crs(basins, st_crs(hydrozones))

# Remove upstream stations (n = 182)
stations_filt = stations %>%
  filter(keep == 1)

# Merge with regime info
stations_filt = stations_filt %>%
  left_join(regime_groups) %>%
  filter(!is.na(Regime))

annual_flow_dat = annual_flow_dat %>%
  filter(STATION_NUMBER %in% stations_filt$STATION_NUMBER)

#Color scheme
mypal = colorFactor(palette = c("#2171b5", "#bdd7e7", "#784B84", "#ff0000"),
                    domain = stations_filt,
                    levels = c("Snow-Dominated - Early Peak",
                               "Snow-Dominated - Late Peak",
                               "Mixed Regimes",
                               "Rain-Dominated"),
                    ordered = T)

## FIGURE 1 - Map of stations and regime type
leaflet(options =
          leafletOptions(zoomControl = FALSE)) %>%
  setView(lat = 55, lng = -125, zoom = 5) %>%
  # fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4]) %>%
  addTiles(group = "Streets") %>%
  addPolygons(data = sub_basins,
              color = "black",
              fillColor = "white",
              weight = 1,
              fillOpacity = 0.2,
              label = ~Sub_Basin) %>%
  addPolygons(data = major_basins,
              color = "black",
              fillColor = "white",
              weight = 2,
              fillOpacity = 0.2) %>%
  addCircleMarkers(data = stations_filt,
                   radius = 3,
                   color = "black",
                   weight = 0.5,
                   fillOpacity = 1,
                   fillColor = ~mypal(Regime)) %>%
  addLegend(pal = mypal,
            values = ~Regime,
            title = "River Flow Stations",
            data =stations_filt,
            #className = "info legend solid circle", #Css from original leaflet script
            opacity = 1,
            layerId = 'legend',
            position = 'topright') %>%
  ## save html to png
  saveWidget("temp.html", selfcontained = FALSE)
webshot("temp.html", file = "print_ver/out/figs/static_leaflet.png",
        cliprect = "viewport", zoom = 2)

calculate_MK_results = function(data,chosen_variable){

  yeardat = data %>%
    group_by(STATION_NUMBER) %>%
    summarise(minYear =min(Year),
              maxYear = max(Year),
              range = max(Year) - min(Year))

 data %>%
     filter(!is.na(chosen_variable)) %>%
      dplyr::select(STATION_NUMBER,
                    Year,
                    values = !!sym(chosen_variable)) %>%
   filter(!is.na(values)) %>%
      add_count(STATION_NUMBER, name = 'number_records') |>
      filter(number_records > 3) |>
      group_by(STATION_NUMBER) %>%
      reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
      unnest(MK_results) %>%
      unnest_longer(col = MK_results) %>%
      group_by(STATION_NUMBER) %>%
      mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
      pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
      mutate(direction = case_when(Slope <0 ~ "negative",
                                   .default = "positive")) %>%
    left_join(yeardat) %>%
    group_by(direction) %>%
    mutate(begin_flow = (Intercept + (Slope * minYear)+0.00000001),
           end_flow = (Intercept + (Slope * maxYear)+0.00000001)) %>%
    mutate(per_change = (((end_flow - begin_flow)/begin_flow))/range*100,
           change_timing =  (end_flow - begin_flow)/range) %>%
    ungroup() %>%
    mutate(
      trend_sig = fcase(
        abs(Tau) <= 0.05 , "No Trend",
        Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Earlier",
        Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Earlier",
        Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Later",
        Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Later",
        Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Down",
        Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Down",
        Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Up",
        Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Up"
      ),
      magnitude_fixed = fcase(
        change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 2 days earlier",
        between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "1 - 2 days earlier",
        between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "< 1 days change",
        between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "1 - 2 days later",
        change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 2 days later",
        per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 5% decrease",
        between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "1 - 5% decrease",
        between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "< 1% change",
        between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "1 - 5% increase",
        per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 5% increase"
      ),
      significant = case_when(P_value <=0.05~ 1,
                              .default = 0.1),
      color = fcase(
        change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'),"#CA0020",
        between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F4A582",
        between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F7F7F7",
        between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#92C5DE",
        change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#0571B0",
        per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#CA0020",
        between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F4A582",
        between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F7F7F7",
        between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#92C5DE",
        per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#0571B0")
    )
}

# Number of stations in each category for each metric
mk_annual = calculate_MK_results(annual_flow_dat, chosen_variable = "Average")  %>%
  left_join(stations, by = "STATION_NUMBER") %>%
  mutate(metric = "Average Annual Flow")

mk_peak = calculate_MK_results(annual_flow_dat, chosen_variable = "Max_3_Day")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(metric = "Peak Flow")

mk_low = calculate_MK_results(annual_flow_dat, chosen_variable = "Min_7_Day_summer")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(metric = "Low Summer Flow")

mk_freshet = calculate_MK_results(annual_flow_dat, chosen_variable = "DoY_50pct_TotalQ")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(metric = "Date of Freshet")

mk_date_low = calculate_MK_results(annual_flow_dat, chosen_variable = "R2MAD_DoY") %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(metric = "Start of Low Flow Period")

#combine above results
mk_results_tbl = bind_rows(mk_annual,
                           mk_peak,
                           mk_low,
                           mk_date_low,
                           mk_freshet)

## Plot settings - magnitude of flow
colour.scale <- c("> 5% increase"="#2171b5",
                  "1 - 5% increase"="#bdd7e7",
                  "No significant trend" = "grey",
                  "< 1% change" = "white",
                  "1 - 5% decrease"="#ff7b7b",
                  "> 5% decrease"="#ff0000")

## Plot settings - Timing of flow
colour.scale.date <- c("> 2 days later"="#2171b5",
                       "1 - 2 days later"="#bdd7e7",
                       "No significant trend" = "grey",
                       "< 1 days change" = "white",
                       "1 - 2 days earlier"="#ff7b7b",
                       "> 2 days earlier"="#ff0000")

# combine above into single doc
mk_results_all = bind_rows(mk_annual,
                           mk_low,
                           mk_peak,
                           mk_freshet,
                           mk_date_low) %>%
  dplyr::select(metric, STATION_NUMBER, magnitude_fixed, significant)

mk_magnitude = mk_results_all %>%
  filter(metric %in% c("Average Annual Flow",
                       "Low Summer Flow",
                       "Peak Flow"))  %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  labs(x = "", y = "") +
  coord_flip()+
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("Volume")

mk_timing = mk_results_all %>%
  filter(!metric %in% c("Average Annual Flow",
                       "Low Summer Flow",
                       "Peak Flow"))  %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 2 days later",
                                                         "1 - 2 days later",
                                                         "< 1 days change",
                                                         "1 - 2 days earlier",
                                                         "> 2 days earlier",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  labs(x = "", y = "Number of Stations") +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("Timing")

mk_all = plot_grid(mk_magnitude, mk_timing, nrow = 2, rel_heights = c(1,1))
mk_all

svg_px("./print_ver/out/figs/mk_all.svg", width = 600, height = 600)
plot(mk_all)
dev.off()

# Split by regime
annual_regime = mk_annual %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Mean Annual Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0,-2,0), "lines"))

peak_regime = mk_peak %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Peak Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))

#Get legend for below plots
legend = get_legend(
  mk_magnitude +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.justification="right")
)

low_regime = mk_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Summer Low Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))

# Timing
freshet_regime = mk_freshet %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Timing of Freshet") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0,-2,0), "lines"))

date_low_regime = mk_date_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Start of Low Flow Period") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))

volume_regime = ggarrange(annual_regime, peak_regime, low_regime,
                          ncol = 1,
                          common.legend = TRUE,
                          legend = "bottom",
                          # align = "v",
                          align = "hv")

svg_px("./print_ver/out/figs/volume_regime.svg", width = 600, height = 600)
plot(volume_regime)
dev.off()

timing_regime = ggarrange(freshet_regime, date_low_regime,
                          ncol = 1,
                          common.legend = TRUE,
                          legend = "bottom",
                          # align = "v",
                          align = "hv")

svg_px("./print_ver/out/figs/timing_regime.svg", width = 600, height = 600)
plot(timing_regime)
dev.off()

# Metrics - Magnitude of River Flow
# - Average Annual Flow

annual_bar = mk_annual %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = annual_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

 p1 = coast  %>%
   ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p1

fraser = annual_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p2

Columbia = annual_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p3

Liard = annual_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p4


Northwest = annual_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p5

Peace = annual_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p6



annual_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

annual_bar_plot

svg_px("./print_ver/out/figs/annual_bar.svg", width = 600, height = 600)
plot(annual_bar_plot)
dev.off()

# - Peak Flow
peak_bar = mk_peak %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = peak_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p1

fraser = peak_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p2

Columbia = peak_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p3

Liard = peak_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p4


Northwest = peak_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p5

Peace = peak_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p6



peak_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

peak_bar_plot

svg_px("./print_ver/out/figs/peak_bar.svg", width = 600, height = 600)
plot(peak_bar_plot)
dev.off()

# - Low Flow
low_bar = mk_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = low_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p1

fraser = low_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p2

Columbia = low_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p3

Liard = low_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p4


Northwest = low_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p5

Peace = low_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p6



low_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

low_bar_plot

svg_px("./print_ver/out/figs/low_bar.svg", width = 600, height = 600)
plot(low_bar_plot)
dev.off()

  #Metrics - Timing of Flow
legend_timing = get_legend(
  mk_timing +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.justification="right")
)

  # Date of Freshet
freshet_bar = mk_freshet %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = freshet_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p1

fraser = freshet_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p2

Columbia = freshet_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p3

Liard = freshet_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p4


Northwest = freshet_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p5

Peace = freshet_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p6



freshet_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

freshet_bar_plot

svg_px("./print_ver/out/figs/freshet_bar.svg", width = 600, height = 600)
plot(freshet_bar_plot)
dev.off()

  # Start of Low Flow Period
date_low_bar = mk_date_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = date_low_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p1

fraser = date_low_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p2

Columbia = date_low_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p3

Liard = date_low_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p4


Northwest = date_low_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p5

Peace = date_low_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p6



date_low_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

date_low_bar_plot

svg_px("./print_ver/out/figs/date_low_bar.svg", width = 600, height = 600)
plot(date_low_bar_plot)
dev.off()

## MONTHLY BREAKDOWN ------------------------------------------------------------------------------------------------------

monthly_flow_dat = monthly_flow_dat %>%
  filter(STATION_NUMBER %in% stations_filt$STATION_NUMBER)

mk_average_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Average") %>%
      mutate(Month = .x,
             metric = "Average")

      }) %>%
  bind_rows()

monthly_average_bar_plot = mk_average_monthly %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed),
                                   levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name)) %>%
  mutate(Month = fct_relevel(Month, "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March",
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Median River Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(name = "Change per decade",
                    values = c("#2171b5",
                               "#bdd7e7",
                               "white",
                               "#ff7b7b",
                               "#ff0000",
                               "grey"),
                    labels = c("> 5% increase",
                               "1 - 5% increase",
                               "< 1% change",
                               "1 - 5% decrease",
                               "> 5% decrease",
                               "No significant trend"),
                    drop = FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1))



svg_px("./print_ver/out/figs/monthly_average_bar_plot.svg", width = 800, height = 600)
plot(monthly_average_bar_plot)
dev.off()

mk_low_flow_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Min_7_Day") %>%
      mutate(Month = .x,
             metric = "Low Flow")

  }) %>%
  bind_rows()

monthly_low_flow_bar_plot = mk_low_flow_monthly %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name)) %>%
  mutate(Month = fct_relevel(Month,
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September",
                             "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Low Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


svg_px("./print_ver/out/figs/monthly_low_flow_bar_plot.svg", width = 800, height = 600)
plot(monthly_low_flow_bar_plot)
dev.off()

# Monthly peak flow
mk_peak_flow_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Max_3_Day") %>%
      mutate(Month = .x,
             metric = "Peak Flow")

  }) %>%
  bind_rows()

monthly_peak_flow_bar_plot = mk_peak_flow_monthly %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name)) %>%
  mutate(Month = fct_relevel(Month, "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March",
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Low Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop= FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


svg_px("./print_ver/out/figs/monthly_peak_flow_bar_plot.svg", width = 800, height = 600)
plot(monthly_peak_flow_bar_plot)
dev.off()

# Create table of MK results

mk_results_monthly_tbl = bind_rows(mk_average_monthly,
                                   mk_low_flow_monthly,
                                   mk_peak_flow_monthly)
  # Write ecoprovinces -
#to do: do we need ecoprovs?? KARLY

# ecoprovs = bcmaps::ecoprovinces() |> st_transform(crs = 4326) |>
#   st_simplify(dTolerance = 1000)
# sf::write_sf(ecoprovs, 'app/www/ecoprovinces.gpkg')

save(mk_all, annual_bar_plot, low_bar_plot, peak_bar_plot, freshet_bar_plot, date_low_bar_plot, monthly_average_bar_plot, monthly_low_flow_bar_plot, monthly_peak_flow_bar_plot, timing_regime, volume_regime, file = "print_ver/out/figures.RData")

saveRDS(mk_results_tbl, 'print_ver/out/mk_results_tbl.rds')

saveRDS(mk_results_monthly_tbl, 'print_ver/out/mk_results_monthly_tbl.rds')
