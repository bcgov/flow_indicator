# Workflow of filtering data as follows:

# 1. Read in data => 'flow_dat_inclusive'
# 2. Filter: annual or monthly => 'flow_dat'
# 3. Filter: variable of choice => 'flow_dat_focused'
# 4. Filter: recent, medium-term, or all data => 'flow_dat_filtered'


# Load in data
flow_dat_all = vroom::vroom('www/combined_flow_dat.csv')
stations_sf = read_sf('www/stations.gpkg')

flow_dat = reactive({
  if(input$time_scale == 'Annual'){
    dat = flow_dat_all %>% filter(Month == 'All')
  }
  if(input$time_scale == 'Monthly'){
    req(input$month_selector)
    dat = flow_dat_all %>%
             filter(Month == input$month_selector)
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

# mk_results_f = reactive({
#   mk_results %>%
#     filter(name == input$user_var_choice) %>%
#     dplyr::select(-name)
# })

flow_dat_chosen_var = reactive({
  flow_dat() %>%
    dplyr::select(STATION_NUMBER,Year,Month, values = !!sym(input$user_var_choice))
})

# # Filter the full dataset so that it only lists the parameter the user has chosen with the dropdown.
# flow_dat_focused = reactive({
#   if(input$user_var_choice == 'Mean Flow'){
#     return(flow_dat() %>% dplyr::select(STATION_NUMBER,Year,Month,values = Mean))
#   }
#   if(input$user_var_choice == 'Median Flow'){
#     return(flow_dat() %>% dplyr::select(STATION_NUMBER,Year,Month,values = Median))
#   }
#   if(input$user_var_choice == "Date of 50% Annual Flow"){
#     return(flow_dat() %>% dplyr::select(STATION_NUMBER,Year,Month,values = DoY_50pct_TotalQ))
#   }
#   if(input$user_var_choice == 'Minimum Flow (7day)'){
#     return(flow_dat() %>% dplyr::select(STATION_NUMBER,Year,Month,values = Min_7_Day_DoY))
#   }
#   if(input$user_var_choice == 'Total Flow'){
#     return(flow_dat() %>% dplyr::select(STATION_NUMBER,Year,Month,values = Total_Volume_m3))
#   }
# })


