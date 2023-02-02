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
library(data.table)
library(tidyverse)
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
               choices = c('One decade (2010 - present)' = '2010+',
                           'Three decades (1990 - present)' = '1990+',
                           'All available data' = 'all'),
               selected = 'all',
               inline = F)
)

station_plot_tab = wellPanel(
  plotOutput('myplot',height=225)
)
# Absolute Panel with trend selection.
trend_select_abs_panel = absolutePanel(
  id = 'trend_selector',
  top = 240, left = 10, width = 450, height = 550,
  draggable = T,
  tabsetPanel(
    id = 'tabset',
    tabPanel('Trend Options',trend_select_options_tab),
    tabPanel('Station Plot',station_plot_tab)#,
    # tabPanel('Dat view',DT::DTOutput('test'))
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
                    height = '600px')
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

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Load in data
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
    if(input$user_period_choice == '2010+'){
      return(flow_dat_focused() %>% filter(Year >= 2010))
      }
    if(input$user_period_choice == '1990+'){
      return(flow_dat_focused() %>% filter(Year >= 1990))
      }
    if(input$user_period_choice == 'all'){
      return(flow_dat_focused())
      }
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

  MK_table = reactive({
    flow_dat_filtered() %>%
      # Remove stations with too few data points for MK test
      filter(!STATION_NUMBER %in% flow_dat_too_little_data) %>%
      group_by(STATION_NUMBER) %>%
      reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
      unnest(MK_results) %>%
      unnest_longer(col = MK_results) %>%
      group_by(STATION_NUMBER) %>%
      mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
      pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
      #Add in the stations that had too few points to plot.
      bind_rows(
        data.frame(STATION_NUMBER = flow_dat_too_little_data)
      ) %>%
      mutate(trend_sig = fcase(
        abs(Tau) <= 0.05 , "No Trend",
        Tau < -0.05 & P_value < 0.05 & chosen_var_type() == 'Date' , "Significant Trend Earlier",
        Tau < -0.05 & P_value >= 0.05 & chosen_var_type() == 'Date' , "Non-Significant Trend Earlier",
        Tau > 0.05 & P_value >= 0.05 & chosen_var_type() == 'Date' , "Non-Significant Trend Later",
        Tau > 0.05 & P_value < 0.05 & chosen_var_type() == 'Date'  , "Significant Trend Later",
        Tau < -0.05 & P_value < 0.05 & chosen_var_type() == 'Value' , "Significant Trend Down",
        Tau < -0.05 & P_value >= 0.05 & chosen_var_type() == 'Value' , "Non-Significant Trend Down",
        Tau > 0.05 & P_value >= 0.05 & chosen_var_type() == 'Value' , "Non-Significant Trend Up",
        Tau > 0.05 & P_value < 0.05 & chosen_var_type() == 'Value' , "Significant Trend Up"
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

  senslope_dat = reactive({
    flow_dat_filtered() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(MK_table() %>%
                  st_drop_geometry()) %>%
      mutate(SlopePreds = Intercept+Slope*Year) %>%
      dplyr::select(STATION_NUMBER,
                SlopePreds,
                Slope,
                trend_sig,
                P_value,
                Year)
  })

  output$test = DT::renderDT({flow_dat_filtered()})

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Watch for a click on the leaflet map. Once clicked...

  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Station Plot')
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  output$myplot = renderPlot({
    if(click_station() == 'no_selection'){
      ggplot() +
        geom_text(aes(x=1,y=1,label='Click a station on the map to see its plot.')) +
        ggthemes::theme_map()
    } else {

      plot_units = fcase(
        input$user_var_choice %in% c('Mean Annual Flow','Median Annual Flow','Total Annual Flow','Minimum Flow (7day)') , '(m<sup>3</sup>/second)',
        input$user_var_choice == 'Date of 50% Annual Flow' , ""
      )

      flow_dat_filtered() %>%
        filter(STATION_NUMBER == click_station()) %>%
        left_join(stations_sf %>% st_drop_geometry() %>% dplyr::select(STATION_NUMBER,STATION_NAME)) %>%
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
                      senslope_dat()) +
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

    m = leaflet() %>% #stations_sf_with_trend()) %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      addProviderTiles("Esri.WorldImagery",group = "Sat") %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("Streets","Terrain","Satellite",'CartoDB'),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

  observe({
    leafletProxy("leafmap") %>%
      addCircleMarkers(layerId = ~STATION_NUMBER,
                       color = ~mypal()(trend_sig),
                       radius = 3,
                       weight = 10,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                       data = stations_sf_with_trend()) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal(), values = ~trend_sig,
                title = 'Mann-Kendall Trend Result',
                data = stations_sf_with_trend(),
                layerId = 'legend')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
