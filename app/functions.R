
# Calculate Mann-Kendall trend test for data.
calculate_MK_results = function(data,chosen_variable){
  data %>%
    add_count(STATION_NUMBER, name = 'number_records') |>
    filter(number_records >= 3) |>
    group_by(STATION_NUMBER) %>%
    reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
    unnest(MK_results) %>%
    unnest_longer(col = MK_results) %>%
    group_by(STATION_NUMBER) %>%
    mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
    pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
    mutate(trend_sig = fcase(
      abs(Tau) <= 0.05 , "No Trend",
      Tau < -0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY'), "Significant Trend Earlier",
      Tau < -0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY'), "Non-Significant Trend Earlier",
      Tau > 0.05 & P_value >= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY'), "Non-Significant Trend Later",
      Tau > 0.05 & P_value < 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY'), "Significant Trend Later",
      Tau < -0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY')), "Significant Trend Down",
      Tau < -0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY')), "Non-Significant Trend Down",
      Tau > 0.05 & P_value >= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY')), "Non-Significant Trend Up",
      Tau > 0.05 & P_value < 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Max_7_Day_DoY')), "Significant Trend Up"
    ))
}


station_flow_plot = function(data,variable_choice,clicked_station,stations_shapefile,slopes,caption_label){

  label.frame = data.frame(varname = c('Average',
                                       'DoY_50pct_TotalQ','Min_7_Day',
                                       'Min_7_Day_DoY','Min_30_Day',
                                       'Min_30_Day_DoY',
                                       'Max_7_Day','Max_7_Day_DoY'),
                           labels = c('Average Flow',
                                      'Date of 50% Annual Flow',
                                      'Minimum Flow (7day)',
                                      'Date of Minimum Flow (7day)',
                                      'Minimum Flow (30day)',
                                      'Date of Minimum Flow (30day)',
                                      'Maximum Flow (7day)',
                                      'Date of Maximum Flow (7day)'))

  if(clicked_station == 'no_selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
        ggthemes::theme_map()
    } else {

      plot_units = fcase(
        variable_choice %in% c('Average','Total_Volume_m3','Min_7_Day','Min_30_Day') , '(m<sup>3</sup>/second)',
        variable_choice %in% c('DoY_50pct_TotalQ','Min_7_Day_DoY','Min_30_Day_DoY','Max_7_Day_DoY'), " "
      )

      station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER == clicked_station,]$STATION_NAME)

      data %>%
        ungroup() %>%
        filter(STATION_NUMBER == clicked_station) %>%
        left_join(stations_shapefile %>%
                    st_drop_geometry() %>%
                    dplyr::select(STATION_NUMBER,STATION_NAME)) %>%
        ggplot() +
            geom_point(aes(y = values, x = Year)) +
            geom_line(aes(y = SlopePreds, x = Year),
                      colour = 'darkblue',
                      linetype = 1,
                      linewidth = 2,
                      alpha = 0.75,
                      data = slopes) +
            labs(title = paste0(station_name," (",unique(clicked_station),")"),
                 subtitle = paste0(unique(slopes$trend_sig),
                                   " (Sen slope:",round(slopes$Slope,3),
                                   ", p-value ~ ",round(unique(slopes$P_value),2),")"),
                 caption = caption_label) +
            labs(y = paste(label.frame[label.frame$varname == variable_choice,]$labels,plot_units,sep = " ")) +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            theme_minimal() +
            theme(axis.title.y = element_markdown(size = 14),
                  axis.title.x = element_text(size = 14),
                  axis.text = element_text(size = 11))
    }
}

station_hydrograph_plot = function(dat,clicked_station,stations_shapefile){

  if(clicked_station == 'no_selection'){

    ggplot() +
      geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
      ggthemes::theme_map()

  } else {
    # Get station name for labelling the plot.
    station_name = unique(stations_shapefile[stations_shapefile$STATION_NUMBER == clicked_station,]$STATION_NAME)

    plotting_df = dat %>%
      ungroup() |>
      filter(STATION_NUMBER == clicked_station) %>%
      # Convert from calendar year to 'water year'
      mutate(month_label = factor(Month, levels = c(month.abb[10:12],month.abb[1:9]))) %>%
      arrange(month_label) |>
      # Add labels for the ribbons we'll add to the figure.
      mutate(median_line_label = 'Median Flow') %>%
      mutate(fifty_pct_label = '"Normal" range (50%) of flow') %>%
      mutate(ninety_pct_label = 'Range of 90% of flow')

    plotting_df %>%
      ggplot() +
      geom_ribbon(aes(x = as.numeric(month_label), ymin = five_perc, ymax = ninetyfive_perc, fill = ninety_pct_label)) +
      geom_ribbon(aes(x = as.numeric(month_label), ymin = twentyfive_perc, ymax = seventyfive_perc, fill = fifty_pct_label)) +
      geom_line(aes(x = as.numeric(month_label), y = median_flow, colour = median_line_label),
                linewidth = 1) +
      scale_colour_manual(values = c("Median Flow" = "#2d7ca1")) +
      scale_fill_manual(values = c("Range of 90% of flow" = "#ceeaed",
                                   '"Normal" range (50%) of flow' = 'lightblue')) +
      scale_x_continuous(breaks = c(1:12),
                         labels = plotting_df$month_label[c(1:12)]) +
      labs(y = 'Average Discharge (m<sup>3</sup>/s)',
           x = '',
           title = '*Daily Stream or River Discharge*',
           subtitle = station_name,
           col = '',
           fill = '') +
      theme(axis.title.y = element_markdown(size = 15),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.position = 'top',
            plot.title = element_markdown(hjust = 0.5),
            panel.background = element_rect(fill = 'transparent'),
            panel.grid.major = element_line(colour = 'grey'))
  }
}
