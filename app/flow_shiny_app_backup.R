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
    HTML('#trend_selector {opacity:0.9;}
         #trend_selector:hover{opacity:1;}'))),
  # Enables us to do some fancy things in javascript...
  useShinyjs(),
  # Include our own styling sheet; defined class 'my_home_button'
  includeCSS('www/bc_button.css'),

  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.9;}
         #trend_selector:hover{opacity:0.9;}'))),
  titlePanel("Flow Indicator"),
  # Throw in our own action button, setting class to 'my_home_button'
  actionButton(
    'abs_button',
    '',
    class = 'my_bc_button'
  ),
  titlePanel("Flow Indicator"),
  map_abs_panel,
  trend_select_abs_panel
)

server <- function(input, output, session) {

  source(file.path('Load_Filter_Data.R'), local = T)$value
  source(file.path('Render_UI_elements.R'), local = T)$value

  date_vars = c("Min_7_Day_DoY","Max_7_Day_DoY","Min_3_Day_DoY","Max_3_Day_DoY","DoY_50pct_TotalQ")

  recent_stations = annual_flow_dat %>%
    group_by(STATION_NUMBER) %>%
    summarise(minYear = min(Year)) %>%
    filter(minYear>=1990) %>%
    pull(STATION_NUMBER)

  upstream_stations = stations_sf %>%
    filter(keep == FALSE) %>%
    pull(STATION_NUMBER)

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

  #New BC Button
  observeEvent(input$abs_button, {
    zoom = input$leafmap_zoom
    lat = input$leafmap_center[2]
    lon = input$leafmap_center[1]

    # Change region_rv() to 'All'
    if(hydro_rv() != 'All'| zoom != 5 | lat != 50 | lon != -130){
      hydro_rv('All')

      # Update map to BC zoom (customizable)
      leafletProxy('leafmap') |>
        set_bc_view()

      updateSelectInput(session = session,
                        'hydrozone_choice',
                        selected = 'All')
    }
  })

  #Some reactives

  hydro_rv = reactiveVal('All')

  # Hydrozone reactives
  hydro_drop_rv = reactive({
    input$hydrozone_choice
  })

  observeEvent(input$hydrozone_choice,{

    hydro_rv(hydro_drop_rv())

  })

  hydro_click_rv = reactive({
    input$leafmap_shape_click$id
  })

  observeEvent(input$leafmap_shape_click$id,{
    updateSelectInput(session = session,
                      'hydrozone_choice',
                      selected = input$leafmap_shape_click$id)
    hydro_rv(hydro_click_rv())
  })

  #Calculate trend results
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

    if(input$recent == FALSE){
      dat = dat %>%
        filter(!(STATION_NUMBER %in% recent_stations))
    }
    else{
      dat = dat
    }

    if(input$upstream == FALSE){
      dat = dat %>%
        filter(!(STATION_NUMBER %in% upstream_stations))
    }
    else{
      dat = dat
    }
    if(hydro_rv() == "All"){
      dat = dat
    }
    else {
      dat = dat %>%
        filter(HYDZN_NAME == hydro_rv())
    }

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
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 0.25 days earlier per year",
                                                                    "0.1 - 0.25 days earlier per year",
                                                                    "< 0.1 days change per year",
                                                                    "0.1 - 0.25 days later per year",
                                                                    "> 0.25 days later per year")))
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
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 0.5% decrease per year",
                                                                    "0.1 - 0.5% decrease per year",
                                                                    "< 0.1% change per year",
                                                                    "0.1 - 0.5% increase per year",
                                                                    "> 0.5% increase per year")))
    }
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year,
             per_change = (((Intercept + Slope * max(Year))-(Intercept + Slope*min(Year)))/(Intercept + Slope * min(Year)))/(max(Year)-min(Year))*100)
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

  # mypal = reactive({
  #   if(input$user_var_choice %in% date_vars){
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$trend_sig,
  #                 levels = c("Significant Trend Earlier",
  #                            'Non-Significant Trend Earlier',
  #                            'No Trend',
  #                            'Non-Significant Trend Later',
  #                            'Significant Trend Later'),
  #                 ordered = T)
  #   } else {
  #     colorFactor(palette = 'RdBu',
  #                 domain = mk_results()$trend_sig,
  #                 levels = c("Significant Trend Down",
  #                            'Non-Significant Trend Down',
  #                            'No Trend',
  #                            'Non-Significant Trend Up',
  #                            'Significant Trend Up'),
  #                 ordered = T)
  #   }
  # })

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
                  levels = c("> 0.25 days earlier per year",
                             "0.1 - 0.25 days earlier per year",
                             "< 0.1 days change per year",
                             "0.1 - 0.25 days later per year",
                             "> 0.25 days later per year"),
                  ordered = T)
    } else {
      colorFactor(palette = 'RdBu',
                  domain = mk_results()$magnitude_fixed,
                  levels = c("> 0.5% decrease per year",
                             "0.1 - 0.5% decrease per year",
                             "< 0.1% change per year",
                             "0.1 - 0.5% increase per year",
                             "> 0.5% increase per year"),
                  ordered = T)
    }
  })

  # zoom
  boundary_data = reactive({
    if(hydro_rv()== "All") {
      hydrozones
    }
    else{
      stations_sf %>%
        filter(HYDZN_NAME == hydro_rv()) %>%
        st_bbox()
    }
  })

  # polygon
  hydro_data = reactive({
    if(hydro_rv()== "All") {
      hydrozones
    }
    else{
      hydrozones %>%
        filter(HYDZN_NAME == hydro_rv())
    }
  })

  output$leafmap <- renderLeaflet({

    map = leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
      # add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')

    if(hydro_rv() == "All"){
      map %>%
        set_bc_view()
    }
    else{
      map %>%
        fitBounds(as.numeric(boundary_data()$xmin)-1, as.numeric(boundary_data()$ymin)-1, as.numeric(boundary_data()$xmax)+1, as.numeric(boundary_data()$ymax)+1)
    }
  })

  observe({
    leafletProxy("leafmap") %>%
      clearMarkers() %>%
      addPolygons(layerId = ~HYDZN_NAME,
                  data = hydrozones,
                  label = ~paste0(HYDZN_NAME), color = "grey", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "#979B9D", weight = 2,
                                                      bringToFront = FALSE)
      ) %>%
      addPolygons(layerId = ~HYDZN_NAME,
                  data = hydro_data(),
                  label = ~paste0(HYDZN_NAME), color = "black", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE)
      ) %>%
      addMarkers(icon = makeIcon(
        iconUrl = "http://leafletjs.com/docs/images/leaf-green.png",
        iconWidth = 1, iconHeight = 1
      ),
      data = stations_sf_with_trend(),
      group = 'STATION_NAME') %>%
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
                layerId = 'legend')%>%
      addSearchFeatures(
        targetGroups = 'STATION_NAME',
        options = searchFeaturesOptions(
          zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
          autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ,
          position = "bottomright"))

  })
}

# Run the application
shinyApp(ui = ui, server = server)
