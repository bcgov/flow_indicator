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

source('UI.R')
source('functions.R')

ui = shiny::fluidPage(
  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.5;}
         #trend_selector:hover{opacity:0.9;}'))),
  titlePanel("Flow Indicator"),
  map_abs_panel,
  trend_select_abs_panel
)

server <- function(input, output) {

  source(file.path('Load_Filter_Data.R'), local = T)$value
  source(file.path('Render_UI_elements.R'), local = T)$value

  date_vars = c("Min_7_Day_DoY","Max_7_Day_DoY","Min_3_Day_DoY","Max_3_Day_DoY","DoY_50pct_TotalQ")

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$time_scale, {
    if(input$time_scale == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Average',
                                       "Low Flow (7-day)" = "Min_7_Day",
                                       "Low Flow (3-day)" = "Min_3_Day")
      )
    }
    if(input$time_scale == 'Annual'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Average',
                                       'Date of Freshet' = 'DoY_50pct_TotalQ',
                                       'Low Flow (7-day)' = 'Min_7_Day',
                                       'Low Flow (3-day)' = 'Min_3_Day',
                                       'Date of Low Flow (7-day)' = 'Min_7_Day_DoY',
                                       'Date of Low Flow (3-day)' = 'Min_3_Day_DoY',
                                       'Peak Flow (7-day)' = 'Max_7_Day',
                                       'Peak Flow (3-day)' = 'Max_3_Day',
                                       'Date of Peak Flow (7-day)' = 'Max_7_Day_DoY',
                                       'Date of Peak Flow (3-day)' = 'Max_3_Day_DoY')
      )
    }
  })

  mk_results = reactive({
    calculate_MK_results(data = flow_dat_chosen_var(),
                         chosen_variable = input$user_var_choice)
  })

  flow_dat_with_mk = reactive({
    # flow_dat() %>%
    flow_dat_chosen_var() %>%
      left_join(mk_results(), by = join_by(STATION_NUMBER))
  })

  stations_sf_with_trend = reactive({
    dat = stations_sf %>%
      left_join(mk_results(), by = join_by(STATION_NUMBER))

    if(input$user_var_choice %in% date_vars){
      dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Earlier",
                                                        'Non-Significant Trend Earlier',
                                                        'No Trend',
                                                        'Non-Significant Trend Later',
                                                        'Significant Trend Later')),
               magnitude = factor(magnitude, levels = c("Earlier",
                                                        "Minimal Change",
                                                        "Later")),
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 10% earlier",
                                                                    "5 - 10% earlier",
                                                                    "< 5% change",
                                                                    "5 - 10% later",
                                                                    "> 10% later")))
    } else {
      dat %>%

        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Down",
                                                        'Non-Significant Trend Down',
                                                        'No Trend',
                                                        'Non-Significant Trend Up',
                                                        'Significant Trend Up')),
               magnitude = factor(magnitude, levels = c("Strong Decrease",
                                                        "Decrease",
                                                        "Minimal Change",
                                                        "Increase",
                                                        "Strong Increase")),
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 10% decrease",
                                                                    "5 - 10% decrease",
                                                                    "< 5% change",
                                                                    "5 - 10% increase",
                                                                    "> 10% increase")))
    }
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Flow Metric Plot')
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  date_choice_label = reactive({
    switch(input$time_scale,
           Annual = 'Based on Data from: Entire year',
           Monthly = paste0('Based on Data from: ',month.name[as.numeric(input$time_selector)])
    )
  })

  output$myplot = renderPlot({
    station_flow_plot(data = flow_dat_chosen_var(),
                      variable_choice = input$user_var_choice,
                      clicked_station = click_station(),
                      stations_shapefile = stations_sf,
                      slopes = senslope_dat(),
                      caption_label = date_choice_label())
  })

  output$myhydrograph = renderPlot({
    station_hydrograph_plot(
      dat = hydrograph_data_station(),
      clicked_station = click_station(),
      stations_shapefile = stations_sf)
  })

  mypal = reactive({
    if(input$user_var_choice %in% date_vars){
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Earlier",
                             'Non-Significant Trend Earlier',
                             'No Trend',
                             'Non-Significant Trend Later',
                             'Significant Trend Later'),
                  ordered = T)
    } else {
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$trend_sig,
                  levels = c("Significant Trend Down",
                             'Non-Significant Trend Down',
                             'No Trend',
                             'Non-Significant Trend Up',
                             'Significant Trend Up'),
                  ordered = T)
    }
  })

  #   mypal2 = reactive({
  #   if(input$user_var_choice %in% date_vars){
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("> 25% earlier",
  #                            "5 - 25% earlier",
  #                            "< 5% change",
  #                            "5 - 25% later",
  #                            "> 25% later"),
  #                 ordered = T)
  #   } else {
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("> 25% decrease",
  #                            "5 - 25% decrease",
  #                            "< 5% change",
  #                            "5 - 25% increase",
  #                            "> 25% increase"),
  #                 ordered = T)
  #   }
  # })

  # mypal2 = reactive({
  #   if(input$user_var_choice %in% date_vars){
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("Much Earlier",
  #                            "Earlier",
  #                            "Little Earlier",
  #                            "Little Later",
  #                            "Later",
  #                            "Much Later"),
  #                 ordered = T)
  #   } else {
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("Strong Negative",
  #                            "Negative",
  #                            "Weak Negative",
  #                            "Weak Positive",
  #                            "Positive",
  #                            "Strong Positive"),
  #                 ordered = T)
  #   }
  # })

  # mypal2 = reactive({
  #   if(input$user_var_choice %in% date_vars){
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("Earlier",
  #                            "Minimal Change",
  #                            "Later"),
  #                 ordered = T)
  #   } else {
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$magnitude,
  #                 levels = c("Strong Decrease",
  #                            "Decrease",
  #                            "Minimal Change",
  #                            "Increase",
  #                            "Strong Increase"),
  #                 ordered = T)
  #   }
  # })

  mypal3 = reactive({
    if(input$user_var_choice %in% date_vars){
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$magnitude_fixed,
                  levels = c("> 10% earlier",
                             "5 - 10% earlier",
                             "< 5% change",
                             "5 - 10% later",
                             "> 10% later"),
                  ordered = T)
    } else {
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$magnitude_fixed,
                  levels = c("> 10% decrease",
                             "5 - 10% decrease",
                             "< 5% change",
                             "5 - 10% increase",
                             "> 10% increase"),
                  ordered = T)
    }
  })

  output$leafmap <- renderLeaflet({

    leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')
  })

  observe({
    leafletProxy("leafmap") %>%
      clearMarkers() %>%
      addCircleMarkers(layerId = ~STATION_NUMBER,
                       color = 'black',
                       fillColor = ~mypal3()(magnitude_fixed),
                       radius = 5,
                       weight = 1,
                       fillOpacity = ~significant,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                       data = stations_sf_with_trend()) %>%
      removeControl("legend") %>%
      addLegend(pal = mypal3(),
                values = ~magnitude_fixed,
                title = 'Mann-Kendall Trend Result',
                data = stations_sf_with_trend(),
                layerId = 'legend')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
