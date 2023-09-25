
# Load in data
# flow_dat_all = vroom::vroom('www/combined_flow_dat.csv')
annual_flow_dat = readRDS('www/annual_flow_dat.rds')
monthly_flow_dat = readRDS('www/monthly_flow_dat.rds')
hydrograph_dat = readRDS('www/hydrograph_dat.rds')
stations_sf = read_sf('www/stations.gpkg')
ecoprovs = read_sf('www/ecoprovinces.gpkg')

# Drop variables that do not pertain to the selected time scale.
flow_dat = reactive({
  if(input$time_scale == 'Annual'){
    dat = annual_flow_dat
  }
  if(input$time_scale == 'Monthly'){
    req(input$month_selector)
    dat = monthly_flow_dat %>%
      filter(Month == input$month_selector)

    lowflows_monthly = flow_dat_all %>%
      dplyr::select(Year,STATION_NUMBER,contains('LowFlow7_')) %>%
      pivot_longer(-c(Year,STATION_NUMBER),names_to='Month',values_to='Min_7_Day') %>%
      mutate(Month = str_remove(Month,'LowFlow7_')) %>%
      filter(Month == input$month_selector) %>%
      filter(!is.na(Min_7_Day))

    dat = average_flows_monthly |>
      full_join(lowflows_monthly)
  }
  # If the user chooses to restrict the years included in the analysis, implement here.
  if(input$user_period_choice == '2010+'){
    return(dat %>% filter(Year >= 2010))
  }
  if(input$user_period_choice == '1990+'){
    return(dat %>% filter(Year >= 1990))
  }
  if(input$user_period_choice == 'all'){
    return(dat)
  }
})

flow_dat_chosen_var = reactive({
  if(input$time_scale == 'Monthly')req(input$month_selector)
  flow_dat() %>%
    dplyr::select(STATION_NUMBER,Year,values = !!sym(input$user_var_choice)) |>
    filter(!is.na(values))
})

hydrograph_data_station = reactive({
  req(click_station() != 'no_selection')
  hydrograph_dat |>
    filter(STATION_NUMBER == click_station())
})

