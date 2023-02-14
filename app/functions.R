
# Calculate Mann-Kendall trend test for data.
calculate_MK_results = function(data,chosen_variable){
data %>%
  group_by(STATION_NUMBER) %>%
  reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
  unnest(MK_results) %>%
  unnest_longer(col = MK_results) %>%
  group_by(STATION_NUMBER) %>%
  mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
  pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
  mutate(trend_sig = fcase(
    abs(Tau) <= 0.05 , "No Trend",
    Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Significant Trend Earlier",
    Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Non-Significant Trend Earlier",
    Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Non-Significant Trend Later",
    Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), "Significant Trend Later",
    Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Significant Trend Down",
    Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Non-Significant Trend Down",
    Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Non-Significant Trend Up",
    Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY')), "Significant Trend Up"
  ))
}


station_flow_plot = function(data,variable_choice,clicked_station,stations_shapefile,slopes){

  label.frame = data.frame(varname = c('Mean','Median',
                                       'DoY_50pct_TotalQ','Min_7_Day',
                                       'Min_7_Day_DoY','Total_Volume_m3'),
                           labels = c('Mean Flow','Median Flow',
                                      'Date of 50% Annual Flow',
                                      'Minimum Flow (7day)',
                                      'Date of Minimum Flow (7day)',
                                      'Total Flow'))

  if(clicked_station == 'no_selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
        ggthemes::theme_map()
    } else {

      plot_units = fcase(
        variable_choice %in% c('Mean','Median','Total_Volume_m3','Min_7_Day') , '(m<sup>3</sup>/second)',
        variable_choice %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY'), " "
      )

      data %>%
        filter(STATION_NUMBER == clicked_station) %>%
        left_join(stations_shapefile %>% st_drop_geometry() %>% dplyr::select(STATION_NUMBER,STATION_NAME)) %>%
        #Start of ggplot code block. This allows us to assign a variable
        # to the plot title.
        {ggplot(.) +
            geom_point(aes(y = values, x = Year)) +
            geom_line(aes(y = values, x = Year)) +
            geom_line(aes(y = SlopePreds, x = Year),
                      colour = 'darkblue',
                      linetype = 1,
                      linewidth = 2,
                      alpha = 0.75,
                      slopes) +
            labs(title = paste0(unique(.$STATION_NAME)," (",unique(.$STATION_NUMBER),")"),
                 subtitle = paste0(unique(slopes$trend_sig),
                                   " (Sen slope:",round(slopes$Slope,3),
                                   ", p-value ~ ",round(unique(slopes$P_value),2),")")) +
            labs(y = paste(label.frame[label.frame$varname == variable_choice,]$labels,plot_units,sep = " ")) +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            theme_minimal() +
            theme(axis.title.y = element_markdown(size = 14),
                  axis.title.x = element_text(size = 14),
                  axis.text = element_text(size = 11))
        } #End of ggplot code block.
    }
}
