# Workflow of filtering data as follows:

# 1. Read in data => 'flow_dat_inclusive'
# 2. Filter: annual or monthly => 'flow_dat'
# 3. Filter: variable of choice => 'flow_dat_focused'
# 4. Filter: recent, medium-term, or all data => 'flow_dat_filtered'


# Load in data ----------------------------------------------------
flow_dat_monthly = vroom::vroom('www/all_dat.csv')
stations_sf = read_sf('www/stations.gpkg')
flow_dat_daily = read_feather('www/all_flow_dat.feather') %>%
  mutate(Month = month(Date),
         Year = year(Date))

# If time selector is set to 'annual' or a month, use summarised data.
# If custom date range is toggled, return that massive dataset.
dat = reactive({
  # req(input$time_selector)
  # if(input$custom_daterange)
  flow_dat_daily
  # flow_dat_monthly
})

# First filtering cut: time periods -------------------------------
dat_filtered = reactive({
  if(input$user_period_choice == '2010+'){
    return(dat() %>% filter(Year >= 2010))
  }
  if(input$user_period_choice == '1990+'){
    return(dat() %>% filter(Year >= 1990))
  }
  if(input$user_period_choice == 'all'){
    return(dat())
  }
})

# Second filter cut: annual, monthly, or other time range ----------
dat_filteredTwo = reactive({
  dat_filtered() %>%
    filter(Month == input$time_selector) %>%
    dplyr::select(STATION_NUMBER,Year,Month,values = !!sym(input$user_var_choice))
})

bigdat_sum = reactive({
  # Only start calculating this reactive once we have all 4 inputs.
  # req(input$start_month, input$start_day, input$end_month, input$end_day)

  # Use {lubridate} to calculate the start and end periods. We use these to filter the data.
  start_period = (months(as.numeric(input$start_month)) + days(input$start_day))
  end_period = (months(as.numeric(input$end_month)) + days(input$end_day))

  # Perform check that end period is later than start period
  date_check = start_period < end_period
  # If it's not, give a warning.
  shinyFeedback::feedbackWarning("end_month", !date_check, "End date must be later than start date")
  # Date check must be TRUE to proceed.
  req(date_check)

  # Filter data.
  dat = dat_filtered() %>%
    mutate(Year = year(Date),
           Month = month(Date),
           Day = day(Date),
           this_period = c(months(Month) + days(Day))) %>%
    filter(this_period >= start_period,
           this_period <= end_period) %>%
    dplyr::select(-this_period,-Day)

  # Since this dataset has no metrics summarised yet, we'll need
  # to do that here...
  if(input$user_var_choice == 'Mean'){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      summarise(mean = mean(Value,na.rm=T))
    return(dat)
  }
  if(input$user_var_choice == 'Median'){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      summarise(median = median(Value,na.rm=T))
    return(dat)
  }
  if(input$user_var_choice == 'DoY_50pct_TotalQ'){
    dat = dat %>%
      group_by(STATION_NUMBER,Year) %>%
      mutate(RowNumber = row_number(),
             TotalFlow = sum(Value),
             FlowToDate = cumsum(Value)) %>%
      filter(FlowToDate > TotalFlow/2) %>%
      slice(1) %>%
      mutate(DoY_50pct_TotalQ = lubridate::yday(Date))
    return(dat)
  }
  if(input$user_var_choice %in% c('Min_7_Day','Min_7_Day_DoY')){
    dat = as.list(unique(dat$STATION_NUMBER)) %>%
      map( ~ {
        daily_flows = hy_daily_flows(station_number = c(.x)) %>%
          filter(!is.na(Value)) %>%
          mutate(Year = lubridate::year(Date)) %>%
          group_by(STATION_NUMBER,Year) %>%
          mutate(my_row = row_number()) %>%
          ungroup()

        daily_flows_dt = data.table::data.table(daily_flows, key = c('STATION_NUMBER','Year'))

        daily_flows_dt$Min_7_Day = frollmean(daily_flows_dt[, Value], 7, align = 'right')

        as_tibble(daily_flows_dt) %>%
          group_by(STATION_NUMBER,Year) %>%
          slice_min(Min_7_Day) %>%
          group_by(STATION_NUMBER,Year,Min_7_Day) %>%
          slice(1) %>%
          ungroup() %>%
          dplyr::select(-Parameter,-Value,-Symbol, Min_7_Day_DoY = my_row, Min_7_Day_Date = Date)
      }) %>%
      bind_rows()
    return(dat)
  }
  if(input$user_var_choice == 'Total_Volume_m3'){
    dat = dat %>%
      # The flow parameter here is a flow rate, i.e. m^3/second.
      # Multiply by number of seconds in a day to get volume.
      mutate(Volume = Value*86400) %>%
      group_by(STATION_NUMBER,Year) %>%
      summarise(Total_Volume_m3 = sum(Volume))
    return(dat)
  }
  return(dat %>%
           dplyr::select(STATION_NUMBER,Year,Month, values = !!sym(input$user_var_choice))
  )
})

flow_dat_focused = eventReactive(input$custom_daterange, {
  browser()
  if(input$custom_daterange) return(bigdat_sum())
  dat_filteredTwo()
})


