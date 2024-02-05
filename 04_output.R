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

#Load data
if(!exists("hydrozones")){hydrozones = st_read('app/www/hydrozones.gpkg')}
if(!exists("stations")){stations = st_read('app/www/stations.gpkg')}

if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}
# if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}
# if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}

#function for calculated MK results
# calculate_MK_results = function(data, chosen_variable){
#   # library(santoku)
# data %>%
#    filter(!is.na(chosen_variable)) %>%
#     dplyr::select(STATION_NUMBER,
#                   Year,
#                   values = !!sym(chosen_variable)) %>%
#     add_count(STATION_NUMBER, name = 'number_records') |>
#     filter(number_records > 3) |>
#     group_by(STATION_NUMBER) %>%
#     reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
#     unnest(MK_results) %>%
#     unnest_longer(col = MK_results) %>%
#     group_by(STATION_NUMBER) %>%
#     mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
#     pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
#     mutate(direction = case_when(Slope <0 ~ "negative",
#                                  .default = "positive")) %>%
#     group_by(direction) %>%
#     # mutate(bins = chop_equally(Slope,
#     #                            groups = 3))%>%
#     ungroup() %>%
#     mutate(trend_sig = fcase(
#       abs(Tau) <= 0.05 , "No Trend",
#       Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Earlier",
#       Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Earlier",
#       Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Later",
#       Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Later",
#       Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Down",
#       Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Down",
#       Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Up",
#       Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Up"
#     ),
#     # magnitude = fcase(
#     #   bins == levels(bins)[1] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Strong Decrease",
#     #   bins == levels(bins)[2] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Decrease",
#     #   bins %in% c(levels(bins)[3],levels(bins)[4]) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Minimal Change",
#     #   bins == levels(bins)[5] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Increase",
#     #   bins == levels(bins)[6] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Strong Increase",
#     #   bins %in% c(levels(bins)[1],levels(bins)[2]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Earlier",
#     #   bins %in% c(levels(bins)[3],levels(bins)[4]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Minimal Change",
#     #   bins %in% c(levels(bins)[5],levels(bins)[6]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Later"
#     # ),
#     magnitude_fixed = fcase(
#       change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days earlier",
#       between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days earlier",
#       between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "< 0.1 days change",
#       between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days later",
#       change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days later",
#       per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% decrease",
#       between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% decrease",
#       between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "< 0.1% change",
#       between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% increase",
#       per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% increase"
#     ),
#     significant = case_when(P_value <=0.05~ 1,
#                             .default = 0.1))
# }

calculate_MK_results = function(data,chosen_variable){
  library(santoku)

  # data = annual_flow_dat_filtered %>%
  #   dplyr::select(STATION_NUMBER,Year,values = !!sym("DoY_50pct_TotalQ")) |>
  #   filter(!is.na(values))

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
    mutate(bins = chop_equally(Slope,
                               groups = 3))%>%
    mutate(begin_flow = (Intercept + (Slope * minYear)+0.00000001),
           end_flow = (Intercept + (Slope * maxYear)+0.00000001)) %>%
    mutate(per_change = (((end_flow - begin_flow)/begin_flow))/range*100,
           change_timing =  (end_flow - begin_flow)/range) %>%
    ungroup() %>%
    mutate(trend_sig = fcase(
      abs(Tau) <= 0.05 , "No Trend",
      Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Earlier",
      Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Earlier",
      Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-Significant Trend Later",
      Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Significant Trend Later",
      Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Down",
      Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Down",
      Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-Significant Trend Up",
      Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Significant Trend Up"
    ),
    magnitude = fcase(
      bins == levels(bins)[1] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Strong Decrease",
      bins == levels(bins)[2] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Decrease",
      bins %in% c(levels(bins)[3],levels(bins)[4]) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Minimal Change",
      bins == levels(bins)[5] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Increase",
      bins == levels(bins)[6] & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Strong Increase",
      bins %in% c(levels(bins)[1],levels(bins)[2]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Earlier",
      bins %in% c(levels(bins)[3],levels(bins)[4]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Minimal Change",
      bins %in% c(levels(bins)[5],levels(bins)[6]) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Later"
    ),
    magnitude_fixed = fcase(
      change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days earlier",
      between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days earlier",
      between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "< 0.1 days change",
      between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "0.1 - 0.2 days later",
      change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 0.2 days later",
      per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% decrease",
      between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% decrease",
      between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "< 0.1% change",
      between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "0.1 - 0.5% increase",
      per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 0.5% increase"
    ),
    significant = case_when(P_value <=0.05~ 1,
                            .default = 0.1),
    color = fcase(
      change_timing < -0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'),"#CA0020",
      between(change_timing, -0.2, -0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F4A582",
      between(change_timing, -0.1, 0.1) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#F7F7F7",
      between(change_timing, 0.1, 0.2) & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#92C5DE",
      change_timing > 0.2 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#0571B0",
      per_change < -0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#CA0020",
      between(per_change, -0.5, -0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F4A582",
      between(per_change, -0.1, 0.1) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#F7F7F7",
      between(per_change, 0.1, 0.5) & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#92C5DE",
      per_change > 0.5 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#0571B0")
    )
}

# Number of stations in each category for each metric
mk_annual = calculate_MK_results(annual_flow_dat, chosen_variable = "Average")  %>%
  left_join(stations, by = "STATION_NUMBER")

mk_peak = calculate_MK_results(annual_flow_dat, chosen_variable = "Max_3_Day")  %>%
  left_join(stations, by = "STATION_NUMBER")

mk_Low = calculate_MK_results(annual_flow_dat, chosen_variable = "Min_7_Day_summer")  %>%
  left_join(stations, by = "STATION_NUMBER")

mk_freshet = calculate_MK_results(annual_flow_dat, chosen_variable = "DoY_50pct_TotalQ")  %>%
  left_join(stations, by = "STATION_NUMBER")

mk_date_low = calculate_MK_results(annual_flow_dat, chosen_variable = "Min_7_Day_summer_DoY")  %>%
  left_join(stations, by = "STATION_NUMBER")

mk_date_high = calculate_MK_results(annual_flow_dat, chosen_variable = "Max_3_Day_DoY")  %>%
  left_join(stations, by = "STATION_NUMBER")

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
                           mk_Low,
                           mk_peak,
                           mk_freshet,
                           mk_date_low,
                           mk_date_high) %>%
  mutate(metric = rep(c("Average Annual flow",
                        "Low flow",
                        "Peak flow",
                        "Date of Freshet",
                        "Date of low flow",
                        "Date of high flow"), each = length(unique(annual_flow_dat$STATION_NUMBER)))) %>%
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
  theme_soe() +
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
  theme_soe() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Timing")

# legend = get_legend(
#   mk_magnitude +
#     guides(color = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom",
#           legend.title = NULL)
# )

mk_all = plot_grid(mk_magnitude, mk_timing, nrow = 2, rel_heights = c(1,1))
mk_all

# Metrics - Magnitude of River Flow
# - Average Annual Flow

annual_bar = mk_annual %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 10% increase",
                                                         "5 - 10% increase",
                                                         "< 5% change",
                                                         "5 - 10% decrease",
                                                         "> 10% decrease",
                                                         "No significant trend"
                                                         ))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
ggplot() +
  ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,n_stations) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("Hydrologic Zone") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_soe()

svg_px("./print_ver/out/figs/annual_bar.svg", width = 800, height = 600)
plot(annual_bar)
dev.off()

# - Peak Flow

peak_bar = mk_annual %>%
  mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed)) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 10% increase",
                                                         "5 - 10% increase",
                                                         "< 5% change",
                                                         "5 - 10% decrease",
                                                         "> 10% decrease",
                                                         "No significant trend"
  ))) %>%
  group_by(HYDZN_NAME) %>%
  mutate(n_stations = n()) %>%
  group_by(HYDZN_NAME, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Maximum Annual Flow (3 day window)") +
  geom_col(aes(x = fct_reorder(HYDZN_NAME,n_stations) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
  scale_fill_manual(values = colour.scale) +
  xlab("Hydrologic Zone") +
  ylab("Number of stations") +
  labs(fill = "Magnitude of change") +
  coord_flip() +
  theme_soe()

svg_px("./print_ver/out/figs/peak_bar.svg", width = 800, height = 600)
plot(peak_bar)
dev.off()

# - Low Flow

  low_bar = mk_annual %>%
    mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                       .default = magnitude_fixed)) %>%
    mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 10% increase",
                                                           "5 - 10% increase",
                                                           "< 5% change",
                                                           "5 - 10% decrease",
                                                           "> 10% decrease",
                                                           "No significant trend"
    ))) %>%
    group_by(HYDZN_NAME) %>%
    mutate(n_stations = n()) %>%
    group_by(HYDZN_NAME, magnitude_fixed) %>%
    summarise(n = n(),
              n_stations = unique(n_stations)) %>%
    mutate(percent = n/n_stations) %>%
    ggplot() +
    ggtitle("Minimum Annual Flow (7 day window)") +
    geom_col(aes(x = fct_reorder(HYDZN_NAME, n_stations) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
    scale_fill_manual(values = colour.scale) +
    xlab("Hydrologic Zone") +
    ylab("Number of stations") +
    labs(fill = "Magnitude of change") +
    coord_flip() +
    theme_soe()

  svg_px("./print_ver/out/figs/low_bar.svg", width = 800, height = 600)
  plot(low_bar)
  dev.off()

  #Metrics - Timing of Flow
  # Date of Freshet

freshet_bar = mk_freshet %>%
    mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                       .default = magnitude_fixed)) %>%
    mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 10% later",
                                                           "5 - 10% later",
                                                           "< 5% change",
                                                           "5 - 10% earlier",
                                                           "> 10% earlier",
                                                           "No significant trend"
    ))) %>%
    group_by(HYDZN_NAME) %>%
    mutate(n_stations = n()) %>%
    group_by(HYDZN_NAME, magnitude_fixed) %>%
    summarise(n = n(),
              n_stations = unique(n_stations)) %>%
    mutate(percent = n/n_stations) %>%
    ggplot() +
    ggtitle("Change in Date of Freshet") +
    geom_col(aes(x = fct_reorder(HYDZN_NAME,n_stations) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
    scale_fill_manual(values = colour.scale.date) +
    xlab("Hydrologic Zone") +
    ylab("Number of stations") +
    labs(fill = "Magnitude of change") +
    coord_flip() +
    theme_soe()

  svg_px("./print_ver/out/figs/freshet_bar.svg", width = 800, height = 600)
  plot(freshet_bar)
  dev.off()

  # Date of Low Flow

  date_low_bar = mk_date_low %>%
    mutate(magnitude_fixed = case_when(significant == 0.1 ~ "No significant trend",
                                       .default = magnitude_fixed)) %>%
    mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 10% later",
                                                            "5 - 10% later",
                                                            "< 5% change",
                                                            "5 - 10% earlier",
                                                            "> 10% earlier",
                                                            "No significant trend"
    ))) %>%
    group_by(HYDZN_NAME) %>%
    mutate(n_stations = n()) %>%
    group_by(HYDZN_NAME, magnitude_fixed) %>%
    summarise(n = n(),
              n_stations = unique(n_stations)) %>%
    mutate(percent = n/n_stations) %>%
    ggplot() +
    ggtitle("Change in Date of Low Flow") +
    geom_col(aes(x = fct_reorder(HYDZN_NAME,n_stations) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1) +
    scale_fill_manual(values = colour.scale.date) +
    xlab("Hydrologic Zone") +
    ylab("Number of stations") +
    labs(fill = "Magnitude of change") +
    coord_flip() +
    theme_soe()

  svg_px("./print_ver/out/figs/date_low_bar.svg", width = 800, height = 600)
  plot(date_low_bar)
  dev.off()

  # Write ecoprovinces -
#to do: do we need ecoprovs?? KARLY

# ecoprovs = bcmaps::ecoprovinces() |> st_transform(crs = 4326) |>
#   st_simplify(dTolerance = 1000)
# sf::write_sf(ecoprovs, 'app/www/ecoprovinces.gpkg')

save(mk_all, annual_bar, low_bar, peak_bar, freshet_bar, date_low_bar,  file = "print_ver/out/figures.RData")
