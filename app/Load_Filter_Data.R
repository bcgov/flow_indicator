# Workflow of filtering data as follows:

# 1. Read in data => 'flow_dat_inclusive'
# 2. Filter: annual or monthly => 'flow_dat'
# 3. Filter: variable of choice => 'flow_dat_focused'
# 4. Filter: recent, medium-term, or all data => 'flow_dat_filtered'


# Load in data
flow_dat_all = vroom::vroom('www/combined_flow_dat.csv')
stations_sf = read_sf('www/stations.gpkg')

# Drop variables that do not pertain to the selected time scale.
flow_dat = reactive({
  if(input$time_scale == 'Annual'){
    dat = flow_dat_all %>%
      dplyr::select(-contains('Average_'),-contains('LowFlow7_'))
  }
  if(input$time_scale == 'Monthly'){
    req(input$month_selector)

    average_flows_monthly = flow_dat_all %>%
      dplyr::select(Year,STATION_NUMBER,contains('Average_')) %>%
      pivot_longer(-c(Year,STATION_NUMBER),names_to='Month',values_to='Average') %>%
      mutate(Month = str_remove(Month,'Average_')) %>%
      filter(Month == input$month_selector) %>%
      filter(!is.na(Average))

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
    dplyr::select(STATION_NUMBER,Year,values = !!sym(input$user_var_choice))
})


