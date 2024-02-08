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

#Load data
if(!exists("hydrozones")){hydrozones = st_read('app/www/hydrozones.gpkg')}
if(!exists("stations")){stations = st_read('app/www/stations.gpkg')}

if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}
if(!exists("monthly_flow_dat")){monthly_flow_dat = readRDS('app/www/monthly_flow_dat.rds')}


# Remove upstream stations (n = 182)
stations_filt = stations %>%
  filter(keep == 1)

annual_flow_dat = annual_flow_dat %>%
  filter(STATION_NUMBER %in% stations_filt$STATION_NUMBER)

## Create images of regions to add to plots
ne.poly = hydrozones %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = hydrozones %>% filter(region == "North-East"), fill = "yellow")

se.poly = hydrozones %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = hydrozones %>% filter(region == "South-East"), fill = "yellow")

sw.poly = hydrozones %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = hydrozones %>% filter(region == "South-West"), fill = "yellow")

nw.poly = hydrozones %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = hydrozones %>% filter(region == "North-West"), fill = "yellow")

island.poly = hydrozones %>%
  ggplot() +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = hydrozones %>% filter(region == "Island"), fill = "yellow")

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
        Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Earlier",
        Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Earlier",
        Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Later",
        Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Later",
        Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Down",
        Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Down",
        Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Up",
        Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Up"
      ),
      magnitude_fixed = fcase(
        change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days earlier",
        between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days earlier",
        between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "< 0.1 days change",
        between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days later",
        change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days later",
        per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% decrease",
        between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% decrease",
        between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "< 0.1% change",
        between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% increase",
        per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% increase"
      ),
      significant = case_when(P_value <=0.05~ 1,
                              .default = 0.1),
      color = fcase(
        change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'),"#CA0020",
        between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F4A582",
        between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F7F7F7",
        between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#92C5DE",
        change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#0571B0",
        per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#CA0020",
        between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F4A582",
        between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F7F7F7",
        between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#92C5DE",
        per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY_50', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#0571B0")
    )
}

# Number of stations in each category for each metric
mk_annual = calculate_MK_results(annual_flow_dat, chosen_variable = "Average")  %>%
  left_join(stations, by = "STATION_NUMBER") %>%
  mutate(Var = "Average")

mk_peak = calculate_MK_results(annual_flow_dat, chosen_variable = "Max_3_Day")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(Var = "Peak Flow")

mk_low = calculate_MK_results(annual_flow_dat, chosen_variable = "Min_7_Day_summer")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(Var = "Low Summer Flow")

mk_freshet = calculate_MK_results(annual_flow_dat, chosen_variable = "DoY_50pct_TotalQ")  %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(Var = "Date of Freshet")

mk_date_low = calculate_MK_results(annual_flow_dat, chosen_variable = "R2MAD_DoY_50") %>%
  left_join(stations, by = "STATION_NUMBER")%>%
  mutate(Var = "Date of Low Summer Flow")

#combine above results
mk_results_tbl = bind_rows(mk_annual,
                           mk_peak,
                           mk_low,
                           mk_date_low,
                           mk_freshet)

## Plot settings - magnitude of flow
colour.scale <- c("> 0.5% increase"="#2171b5",
                  "0.1 - 0.5% increase"="#bdd7e7",
                  "No significant trend" = "grey",
                  "< 0.1% change" = "white",
                  "0.1 - 0.5% decrease"="#ff7b7b",
                  "> 0.5% decrease"="#ff0000")

## Plot settings - Timing of flow
colour.scale.date <- c("> 0.2 days later"="#2171b5",
                       "0.1 - 0.2 days later"="#bdd7e7",
                       "< 0.1 days change" = "white",
                       "0.1 - 0.2 days earlier"="#ff7b7b",
                       "> 0.2 days earlier"="#ff0000",
                       "No significant trend" = "grey")

# combine above into single doc
mk_results_all = bind_rows(mk_annual,
                           mk_low,
                           mk_peak,
                           mk_freshet,
                           mk_date_low) %>%
  mutate(metric = rep(c("Average Annual flow",
                        "Low flow",
                        "Peak flow",
                        "Date of Freshet",
                        "Date of low flow"),
                      each = length(unique(annual_flow_dat$STATION_NUMBER)))) %>%
  dplyr::select(metric, STATION_NUMBER, magnitude_fixed, significant)

mk_magnitude = mk_results_all %>%
  filter(metric %in% c("Average Annual flow",
                       "Low flow",
                       "Peak flow"))  %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 0.5% increase",
                                                         "0.1 - 0.5% increase",
                                                         "< 0.1% change",
                                                         "0.1 - 0.5% decrease",
                                                         "> 0.5% decrease",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(values = colour.scale) +
  labs(x = "", y = "") +
  coord_flip()+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Magnitude")

mk_timing = mk_results_all %>%
  filter(!metric %in% c("Average Annual flow",
                       "Low flow",
                       "Peak flow"))  %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 0.2 days later",
                                                         "0.1 - 0.2 days later",
                                                         "< 0.1 days change",
                                                         "0.1 - 0.2 days earlier",
                                                         "> 0.2 days earlier",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  labs(x = "", y = "Number of Stations") +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black") +
  scale_fill_manual(values = colour.scale.date) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Timing")

mk_all = plot_grid(mk_magnitude, mk_timing, nrow = 2, rel_heights = c(1,1))
mk_all

svg_px("./print_ver/out/figs/mk_all.svg", width = 600, height = 600)
plot(mk_all)
dev.off()

#Get legend for below plots
legend = get_legend(
  mk_magnitude +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.justification="right",
          legend.title = element_blank())
)

# Metrics - Magnitude of River Flow
# - Average Annual Flow

annual_bar = mk_annual %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
                                                          "0.1 - 0.5% increase",
                                                          "< 0.1% change",
                                                          "0.1 - 0.5% decrease",
                                                          "> 0.5% decrease",
                                                          "No significant trend"))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(region)) %>%
  mutate(percent = n/n_stations)


# Try plotting each region "separately and combining in facet grid
ne = annual_bar %>%
  filter(region == "North-East")

 p1 = ne  %>%
   ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(ne.poly),ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p1

se = annual_bar %>%
  filter(region == "South-East")

p2 = se %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(se.poly),ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)


p2

sw = annual_bar %>%
  filter(region == "South-West")

p3 = sw %>%
  filter(region == "South-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(sw.poly),ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)


p3

nw = annual_bar %>%
  filter(region == "North-West")

p4 = nw %>%
  filter(region == "North-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(nw.poly),ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)


p4

Island = annual_bar %>%
  filter(region == "Island")

p5 = Island %>%
  filter(region == "Island") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  # theme(axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(island.poly),ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

annual_bar_plot = plot_grid(p1,p2,p3,p4,p5, ncol = 1,
                            align = "v")

annual_bar_plot = plot_grid(annual_bar_plot,
                            legend,
                            ncol = 1,
                            rel_heights = c(1,0.1))

annual_bar_plot

svg_px("./print_ver/out/figs/annual_bar.svg", width = 600, height = 600)
plot(annual_bar_plot)
dev.off()

# - Peak Flow

peak_bar = mk_peak %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
                                                          "0.1 - 0.5% increase",
                                                          "< 0.1% change",
                                                          "0.1 - 0.5% decrease",
                                                          "> 0.5% decrease",
                                                          "No significant trend"))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(region)) %>%
  mutate(percent = n/n_stations)


# Try plotting each region "separately and combining in facet grid
ne = peak_bar %>%
  filter(region == "North-East")

p1 = ne  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(ne.poly),ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p1

se = peak_bar %>%
  filter(region == "South-East")

p2 = se %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(se.poly),ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)


p2

sw = peak_bar %>%
  filter(region == "South-West")

p3 = sw %>%
  filter(region == "South-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(sw.poly),ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)


p3

nw = peak_bar %>%
  filter(region == "North-West")

p4 = nw %>%
  filter(region == "North-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(nw.poly),ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)


p4

Island = peak_bar %>%
  filter(region == "Island")

p5 = Island %>%
  filter(region == "Island") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  # theme(axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(island.poly),ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

peak_bar_plot = plot_grid(p1,p2,p3,p4,p5, ncol = 1,
                            align = "v")

peak_bar_plot = plot_grid(peak_bar_plot,
                            legend,
                            ncol = 1,
                            rel_heights = c(1,0.1))

peak_bar_plot

svg_px("./print_ver/out/figs/peak_bar.svg", width = 600, height = 600)
plot(peak_bar_plot)
dev.off()

# - Low Flow

low_bar = mk_low %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
                                                          "0.1 - 0.5% increase",
                                                          "< 0.1% change",
                                                          "0.1 - 0.5% decrease",
                                                          "> 0.5% decrease",
                                                          "No significant trend"))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(region)) %>%
  mutate(percent = n/n_stations)


# Try plotting each region "separately and combining in facet grid
ne = low_bar %>%
  filter(region == "North-East")

p1 = ne  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(ne.poly),ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p1

se = low_bar %>%
  filter(region == "South-East")

p2 = se %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(se.poly),ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)


p2

sw = low_bar %>%
  filter(region == "South-West")

p3 = sw %>%
  filter(region == "South-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(sw.poly),ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)


p3

nw = low_bar %>%
  filter(region == "North-West")

p4 = nw %>%
  filter(region == "North-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(nw.poly),ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)


p4

Island = low_bar %>%
  filter(region == "Island")

p5 = Island %>%
  filter(region == "Island") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  # theme(axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(island.poly),ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

low_bar_plot = plot_grid(p1,p2,p3,p4,p5, ncol = 1,
                            align = "v")

low_bar_plot = plot_grid(low_bar_plot,
                            legend,
                            ncol = 1,
                            rel_heights = c(1,0.1))

low_bar_plot

svg_px("./print_ver/out/figs/low_bar.svg", width = 600, height = 600)
plot(low_bar_plot)
dev.off()

  #Metrics - Timing of Flow
legend_timing = get_legend(
  mk_timing +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.justification="right",
          legend.title = element_blank())
)

  # Date of Freshet

freshet_bar = mk_freshet %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.2 days earlier",
                                                          "0.1 - 0.2 days earlier",
                                                          "< 0.1 days change",
                                                          "0.1 - 0.2 days later",
                                                          "> 0.2 days later",
                                                          "No significant trend"))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(region)) %>%
  mutate(percent = n/n_stations)


# Try plotting each region "separately and combining in facet grid
ne = freshet_bar %>%
  filter(region == "North-East")

p1 = ne  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(ne.poly),ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p1

se = freshet_bar %>%
  filter(region == "South-East")

p2 = se %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(se.poly),ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)


p2

sw = freshet_bar %>%
  filter(region == "South-West")

p3 = sw %>%
  filter(region == "South-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(sw.poly),ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)


p3

nw = freshet_bar %>%
  filter(region == "North-West")

p4 = nw %>%
  filter(region == "North-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(nw.poly),ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)


p4

Island = freshet_bar %>%
  filter(region == "Island")

p5 = Island %>%
  filter(region == "Island") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  # theme(axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(island.poly),ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

freshet_bar_plot = plot_grid(p1,p2,p3,p4,p5, ncol = 1,
                            align = "v")

freshet_bar_plot = plot_grid(freshet_bar_plot,
                            legend_timing,
                            ncol = 1,
                            rel_heights = c(1,0.1))

freshet_bar_plot

svg_px("./print_ver/out/figs/freshet_bar.svg", width = 600, height = 600)
plot(freshet_bar_plot)
dev.off()

  # Date of Low Flow

date_low_bar = mk_date_low %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.2 days earlier",
                                                          "0.1 - 0.2 days earlier",
                                                          "< 0.1 days change",
                                                          "0.1 - 0.2 days later",
                                                          "> 0.2 days later",
                                                          "No significant trend"))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(region)) %>%
  mutate(percent = n/n_stations)


# Try plotting each region "separately and combining in facet grid
ne = date_low_bar %>%
  filter(region == "North-East")

p1 = ne  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(ne.poly),ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p1

se = date_low_bar %>%
  filter(region == "South-East")

p2 = se %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(se.poly),ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)


p2

sw = date_low_bar %>%
  filter(region == "South-West")

p3 = sw %>%
  filter(region == "South-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(sw.poly),ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)


p3

nw = date_low_bar %>%
  filter(region == "North-West")

p4 = nw %>%
  filter(region == "North-West") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(nw.poly),ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)


p4

Island = date_low_bar %>%
  filter(region == "Island")

p5 = Island %>%
  filter(region == "Island") %>%
  # mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 0.5% increase",
  #                                                         "0.1 - 0.5% increase",
  #                                                         "< 0.1% change",
  #                                                         "0.1 - 0.5% decrease",
  #                                                         "> 0.5% decrease",
  #                                                         "No significant trend"))) %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale.date) +
  xlab("") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  # theme(axis.line.x = element_blank(),
  #       axis.text.x = element_blank(),
  #       axis.title.x = element_blank(),
  #       axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations)+ 10) +
  annotation_custom(grob = ggplotGrob(island.poly),ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

date_low_bar_plot = plot_grid(p1,p2,p3,p4,p5, ncol = 1,
                             align = "v")

date_low_bar_plot = plot_grid(date_low_bar_plot,
                             legend_timing,
                             ncol = 1,
                             rel_heights = c(1,0.1))

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
      mutate(Month = .x)

      }) %>%
  bind_rows()

monthly_average_bar_plot = mk_average_monthly %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 0.5% increase",
                                                         "0.1 - 0.5% increase",
                                                         "< 0.1% change",
                                                         "0.1 - 0.5% decrease",
                                                         "> 0.5% decrease",
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
  scale_fill_manual(values = colour.scale) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


svg_px("./print_ver/out/figs/monthly_average_bar_plot.svg", width = 800, height = 600)
plot(monthly_average_bar_plot)
dev.off()

mk_low_flow_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Min_7_Day") %>%
      mutate(Month = .x)

  }) %>%
  bind_rows()

monthly_low_flow_bar_plot = mk_low_flow_monthly %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 0.5% increase",
                                                         "0.1 - 0.5% increase",
                                                         "< 0.1% change",
                                                         "0.1 - 0.5% decrease",
                                                         "> 0.5% decrease",
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
  scale_fill_manual(values = colour.scale) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


svg_px("./print_ver/out/figs/monthly_low_flow_bar_plot.svg", width = 800, height = 600)
plot(monthly_low_flow_bar_plot)
dev.off()
  # Write ecoprovinces -
#to do: do we need ecoprovs?? KARLY

# ecoprovs = bcmaps::ecoprovinces() |> st_transform(crs = 4326) |>
#   st_simplify(dTolerance = 1000)
# sf::write_sf(ecoprovs, 'app/www/ecoprovinces.gpkg')

save(mk_all, annual_bar_plot, low_bar_plot, peak_bar_plot, freshet_bar_plot, date_low_bar_plot, monthly_average_bar_plot, monthly_low_flow_bar_plot,  file = "print_ver/out/figures.RData")

saveRDS(mk_results_tbl, 'print_ver/out/mk_results_tbl.rds')
