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

rm(list = ls())

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.providers)
library(envreportutils)
library(sf)
library(EnvStats)
library(modifiedmk)
library(tidyverse)
# library(plotly)
library(ggtext)

# Trend selection options
trend_select_options_tab = wellPanel(
  selectizeInput(inputId = 'user_var_choice',
                 label = 'Trend to Display',
                 choices = c("Mean Annual Flow",
                             "Median Annual Flow",
                             "Date of 50% Annual Flow",
                             "Minimum Flow (7day)",
                             "Total Annual Flow"),
                 selected = 'Mean Annual Flow',
                 width = '100%'),
  radioButtons(inputId = 'user_period_choice',
               label = 'Date Cutoff',
               choices = c('1990+','1967+','1912+'),
               selected = '1912+',
               inline = F)
)

station_plot_tab = wellPanel(
  plotOutput('myplot',height=225)
)
# Absolute Panel with trend selection.
trend_select_abs_panel = absolutePanel(
  id = 'trend_selector',
  top = 240, left = 10, width = 600, height = 550,
  draggable = T,
  tabsetPanel(
    id = 'tabset',
    tabPanel('Trend Options',trend_select_options_tab),
    tabPanel('Station Plot',station_plot_tab),
    tabPanel('DT of test',DT::DTOutput('test'))
  )
)

# Absolute panel with map as background.
map_abs_panel = absolutePanel(
  top = 0, left = 0, right = 0,
  fixed = TRUE,
  div(
    style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
    fluidRow(
      leafletOutput('leafmap',
                    height = '550px')
    )
  )
)
#
# UI with moveable trend selector absolute panel
# on top of map background.
ui = shiny::fluidPage(
  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.5;}
         #trend_selector:hover{opacity:0.9;}'))),
  titlePanel("Flow Indicator"),
  map_abs_panel,
  card(trend_select_abs_panel)
)

## Option 2.

# ui = fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       trend_select_options,
#       # textOutput('selected_station'),
#       plotlyOutput('myplotly')
#     ),
#     mainPanel(
#       leafletOutput('leafmap',
#                     height = '500px')
#     )
#   )
# )

# ui = fluidPage(
#   fluidRow(
#     column(width = 7,
#            bslib::card(
#              card_body(
#                leafletOutput('leafmap',
#                              height = '500px')
#              )
#            )
#     ),
#     column(width = 5,
#            bslib::card(
#              trend_select_options
#              ),
#            bslib::card(
#              title = textOutput('selected_station'),
#              plotlyOutput('myplotly')
#            )
#     )
#   )
# )
# Define server logic required to draw a histogram
server <- function(input, output) {

  # Load in data
  # flow_dat = read.csv('./flow_dat.csv')
  flow_dat = read.csv('www/flow_dat.csv')

  # Filter the full dataset so that it only lists the parameter the user has chosen with the dropdown.
  flow_dat_focused = reactive({
    if(input$user_var_choice == 'Mean Annual Flow'){
      return(flow_dat %>% dplyr::select(STATION_NUMBER,Year,values = Mean))
    }
    if(input$user_var_choice == 'Median Annual Flow'){
      return(flow_dat %>% dplyr::select(STATION_NUMBER,Year,values = Median))
    }
    if(input$user_var_choice == "Date of 50% Annual Flow"){
      return(flow_dat %>% dplyr::select(STATION_NUMBER,Year,values = DoY_50pct_TotalQ))
    }
    if(input$user_var_choice == 'Minimum Flow (7day)'){
      return(flow_dat %>% dplyr::select(STATION_NUMBER,Year,values = Min_7_Day_DoY))
    }
    if(input$user_var_choice == 'Total Annual Flow'){
      return(flow_dat %>% dplyr::select(STATION_NUMBER,Year,values = Total_Volume_m3))
    }
  })

  # Make a reactive variable of whether the user has selected a parameter that
  # is essentially a numeric value, or has selected a date variable.
  chosen_var_type = reactive({
    if(input$user_var_choice %in% c('Date of 50% Annual Flow','Minimum Flow (7day)')){
      "Date"
    } else {"Value"}
  })

  # If the user chooses to restrict the years included in the analysis, implement here.
  flow_dat_filtered = reactive({
    if(input$user_period_choice == '1990+'){flow_dat_focused() %>% filter(Year >= 1990)}
    if(input$user_period_choice == '1967+'){flow_dat_focused() %>% filter(Year >= 1967)}
    else{flow_dat_focused()}
  })

  # Get list of (included) stations and their coordinates for map.
  stations_sf = tidyhydat::hy_stations(prov_terr_state_loc = 'BC') %>%
    as_tibble() %>%
    filter(STATION_NUMBER %in% unique(flow_dat$STATION_NUMBER)) %>%
    mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
           HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
    st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
    dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS)

  # Calculate trend significance for user's selected variable and join to stations_sf
  MK_vars = c('Corrected Zc','New p-value','N/N*',
              'Original Z','Original p-value','Tau',
              "Sen's Slope",'Original variance','New variance')

  # Get a list of which stations have too few data points for an MK test.
  flow_dat_too_little_data = flow_dat %>%
    dplyr::count(STATION_NUMBER) %>%
    filter(n <= 6) %>%
    pull(STATION_NUMBER)

  # MK_table = reactive({
  #   flow_dat_filtered() %>%
  #     # Remove stations with too few data points for MK test
  #     filter(!STATION_NUMBER %in% flow_dat_too_little_data) %>%
  #     group_by(STATION_NUMBER) %>%
  #     summarise(MK_results = list(mmky1lag(values))) %>%
  #     unnest(MK_results) %>%
  #     cbind(MK_vars) %>%
  #     as_tibble() %>%
  #     pivot_wider(names_from = MK_vars, values_from = MK_results) %>%
  #     bind_rows(
  #       data.frame(STATION_NUMBER = flow_dat_too_little_data)
  #     ) %>%
  #     mutate(trend_sig = case_when(
  #       abs(Tau) <= 0.05 ~ "No Trend",
  #       Tau < -0.05 & `New p-value` < 0.05 & chosen_var_type() == 'Date' ~ "Significant Trend Earlier",
  #       Tau < -0.05 & `New p-value` >= 0.05 & chosen_var_type() == 'Date' ~ "Non-Significant Trend Earlier",
  #       Tau > 0.05 & `New p-value` >= 0.05 & chosen_var_type() == 'Date' ~ "Non-Significant Trend Later",
  #       Tau > 0.05 & `New p-value` < 0.05 & chosen_var_type() == 'Date'  ~ "Significant Trend Later",
  #       Tau < -0.05 & `New p-value` < 0.05 & chosen_var_type() == 'Value' ~ "Significant Trend Down",
  #       Tau < -0.05 & `New p-value` >= 0.05 & chosen_var_type() == 'Value' ~ "Non-Significant Trend Down",
  #       Tau > 0.05 & `New p-value` >= 0.05 & chosen_var_type() == 'Value' ~ "Non-Significant Trend Up",
  #       Tau > 0.05 & `New p-value` < 0.05 & chosen_var_type() == 'Value' ~ "Significant Trend Up"
  #     ))
  # })

  MK_table = reactive({
    flow_dat_filtered() %>%
      # Remove stations with too few data points for MK test
      filter(!STATION_NUMBER %in% flow_dat_too_little_data) %>%
      group_by(STATION_NUMBER) %>%
      summarise(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
      unnest(MK_results) %>%
      unnest_longer(col = MK_results) %>%
      group_by(STATION_NUMBER) %>%
      mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
      pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
      #Add in the stations that had too few points to plot.
      bind_rows(
        data.frame(STATION_NUMBER = flow_dat_too_little_data)
      ) %>%
      mutate(trend_sig = case_when(
        abs(Tau) <= 0.05 ~ "No Trend",
        Tau < -0.05 & P_value < 0.05 & chosen_var_type() == 'Date' ~ "Significant Trend Earlier",
        Tau < -0.05 & P_value >= 0.05 & chosen_var_type() == 'Date' ~ "Non-Significant Trend Earlier",
        Tau > 0.05 & P_value >= 0.05 & chosen_var_type() == 'Date' ~ "Non-Significant Trend Later",
        Tau > 0.05 & P_value < 0.05 & chosen_var_type() == 'Date'  ~ "Significant Trend Later",
        Tau < -0.05 & P_value < 0.05 & chosen_var_type() == 'Value' ~ "Significant Trend Down",
        Tau < -0.05 & P_value >= 0.05 & chosen_var_type() == 'Value' ~ "Non-Significant Trend Down",
        Tau > 0.05 & P_value >= 0.05 & chosen_var_type() == 'Value' ~ "Non-Significant Trend Up",
        Tau > 0.05 & P_value < 0.05 & chosen_var_type() == 'Value' ~ "Significant Trend Up"
      ))
  })

  # Join the Mann-Kendall test result to the spatial table of stations.
  # If the user has chosen a variable based on day-of-year measurements,
  # use the terms 'later and earlier' rather than 'up and down'.
  stations_sf_with_trend = reactive({
    if(chosen_var_type() == "Date"){
      stations_sf %>%
        left_join(MK_table()) %>%
        mutate(trend_sig = replace_na(trend_sig, 'No Trend')) %>%
        mutate(trend_sig = factor(trend_sig, levels = c('Significant Trend Later','Non-Significant Trend Later',
                                                        'No Trend','Non-Significant Trend Earlier','Significant Trend Earlier')))
    } else {
      stations_sf %>%
        left_join(MK_table()) %>%
        mutate(trend_sig = replace_na(trend_sig, 'No Trend')) %>%
        mutate(trend_sig = factor(trend_sig, levels = c('Significant Trend Up','Non-Significant Trend Up',
                                                        'No Trend','Non-Significant Trend Down','Significant Trend Down')))
    }
  })

  # # Make a dataframe for whichever station the user has selected
  # # that we can use to add a Sen slope trend line (plus MK test p-value)
  # # to a ggplot figure.
  # senslope_dat = reactive({
  #   flow_dat_filtered() %>%
  #     filter(STATION_NUMBER == click_station()) %>%
  #     slice(1,nrow(.)) %>%
  #     mutate(start_year = .[1,]$Year,
  #            end_year = .[2,]$Year) %>%
  #     mutate(mid_year = start_year + (end_year-start_year)/2) %>%
  #     slice(1) %>%
  #     left_join(MK_table() %>%
  #                 st_drop_geometry()) %>%
  #     summarise(STATION_NUMBER,
  #               y = values,
  #               start_year,
  #               end_year,
  #               slope = `Sen's Slope`,
  #               p_value = `New p-value`,
  #               trend_sig) %>%
  #     mutate(yend = y + (slope*(end_year-start_year))) %>%
  #     mutate(ymid = y + (yend-y)/2)
  # })

  senslope_dat = reactive({
    flow_dat_filtered() %>%
      filter(STATION_NUMBER == click_station()) %>%
      mutate(start_year = first(Year),
             end_year = last(Year)) %>%
      slice(1) %>%
      left_join(MK_table() %>%
                  st_drop_geometry()) %>%
      summarise(STATION_NUMBER,
                Intercept,
                start_year,
                end_year,
                Slope,
                trend_sig,
                P_value) %>%
      mutate(y = Intercept,
             y_end = as.numeric(y) + (as.numeric(Slope)*as.numeric(end_year)))
      # summarise(STATION_NUMBER,
      #           y_mid = values,
      #           start_year,
      #           mid_year = Year,
      #           end_year,
      #           slope = `Sen's Slope`,
      #           p_value = `New p-value`,
      #           trend_sig) %>%
      # mutate(y = y_mid - slope*(end_year - mid_year),
      #        y_end = y_mid + slope*(end_year - mid_year))
  })

  output$test = DT::renderDT({senslope_dat()})

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Watch for a click on the leaflet map. Once clicked...

  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  output$myplot = renderPlot({
    if(click_station() == 'no_selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
        ggthemes::theme_map()
    } else {

      plot_units = case_when(
        input$user_var_choice %in% c('Mean Annual Flow','Median Annual Flow','Total Annual Flow','Minimum Flow (7day)') ~ '(m<sup>3</sup>/second)',
        input$user_var_choice == 'Date of 50% Annual Flow' ~ ""
      )

      flow_dat_filtered() %>%
        filter(STATION_NUMBER == click_station()) %>%
        left_join(stations_sf %>% st_drop_geometry() %>% dplyr::select(STATION_NUMBER,STATION_NAME)) %>%
        #Start of ggplot code block. This allows us to assign a variable
        # to the plot title.
        {ggplot(.) +
            geom_point(aes(y = values, x = Year)) +
            geom_line(aes(y = values, x = Year)) +
            geom_segment(colour = 'darkblue',
                         linetype = 1,
                         linewidth = 2,
                         alpha = 0.75,
                         aes(x = start_year, y = y,
                             xend = end_year, yend = y_end),
                         data = senslope_dat()) +
            labs(title = paste0(unique(.$STATION_NAME)," (",unique(.$STATION_NUMBER),")"),
                 subtitle = paste0(unique(senslope_dat()$trend_sig),
                                   " (Sen slope:",round(senslope_dat()$Slope,3),
                                   ", p-value ~ ",round(unique(senslope_dat()$P_value),2),")")) +
            labs(y = paste(input$user_var_choice,plot_units,sep = " ")) +
            scale_x_continuous(breaks = scales::pretty_breaks()) +
            theme_minimal() +
            theme(axis.title.y = element_markdown(size = 14),
                  axis.title.x = element_text(size = 14),
                  axis.text = element_text(size = 11))
        } #End of ggplot code block.
    }
  })

  mypal = reactive({
    if(chosen_var_type() == 'Date'){
      leaflet::colorFactor(palette = c('RdBu'),
                           levels = c('Significant Trend Later','Non-Significant Trend Later',
                                      'No Trend','Non-Significant Trend Earlier','Significant Trend Earlier'),
                           reverse = T,
                           ordered = TRUE)
    } else {
      leaflet::colorFactor(palette = c('RdBu'),
                           levels = c('Significant Trend Up','Non-Significant Trend Up',
                                      'No Trend','Non-Significant Trend Down','Significant Trend Down'),
                           reverse = T,
                           ordered = TRUE)
    }
  })

  output$leafmap <- renderLeaflet({

    m = leaflet(stations_sf_with_trend()) %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addProviderTiles("Esri.WorldImagery",group = "Sat") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("Streets","Terrain","CartoDB","Satellite"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

  observe({
    leafletProxy("leafmap") %>%
      addCircleMarkers(layerId = ~STATION_NUMBER,
                       color = ~mypal()(trend_sig),
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                       data = stations_sf_with_trend()) %>%
      addLegend(pal = mypal(), values = ~trend_sig,
                title = 'Mann-Kendall Trend Result',
                data = stations_sf_with_trend())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
