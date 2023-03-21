library(shiny)
library(bslib)
library(plotly)
library(leaflet)
library(leaflet.providers)
library(envreportutils)
library(sf)
library(EnvStats)
library(data.table)
library(tidyverse)
library(ggtext)

oldest_data_filter_ui = radioButtons(
  inputId = 'user_period_choice',
  label = 'Oldest data to include',
  choices = c('2010' = '2010+',
              '1990' = '1990+',
              'All' = 'all'),
  selected = 'all',
  inline = F)

timescale_ui = fluidRow(
  radioButtons(
    inputId = 'time_scale',
    label = 'Yearly or Monthly Data',
    choices = c('Annual','Monthly'),
    inline = F,
    selected = 'Annual'),
  uiOutput('month_selector_UI')
)

varchoice_ui = selectizeInput(
  inputId = 'user_var_choice',
  label = 'Trend to Display',
  choices = c('Average Flow' = 'Average',
              'Date of 50% Flow' = 'DoY_50pct_TotalQ',
              'Minimum Flow (7-day)' = 'Min_7_Day',
              'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
              'Minimum Flow (30-day)' = 'Min_30_Day',
              'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
              'Maximum Flow (7-day)' = 'Max_7_Day',
              'Date of Maximum Flow (7-day)' = 'Max_7_Day_DoY'),
  selected = 'Mean',
  width = '100%')

number_stations_vb = value_box(
  "Stations Included",
  span(
    textOutput('num_stations_on_plot')
  ),
  showcase = bsicons::bs_icon("moisture", size = "300%"),
  class = "bg-secondary"
)

number_stations_declining = value_box(
  "Flow Metric Shift Down/Earlier",
  span(
    textOutput('num_stations_dec')
  ),
  showcase = bsicons::bs_icon("droplet-half", size = "300%"),
  class = 'bg-danger'
)

number_stations_increasing = value_box(
  "Flow Metric Shift Up/Later",
  span(
    textOutput('num_stations_inc')
  ),
  showcase = bsicons::bs_icon("droplet-fill", size = "300%"),
  class = "bg-primary"
)

summary_boxes = tagList(
  number_stations_vb,
  number_stations_declining,
  number_stations_increasing
)

# Trend selection options.
trend_select_options = fluidRow(
  oldest_data_filter_ui,
  varchoice_ui,
  timescale_ui,
  summary_boxes
)

# station_plot = card(height = '20%',
#   card_body(height = '20%',
#     plotOutput('myplot')
#   )
# )

station_plot = tagList(
  # h5("Station Plot",style = 'text-align:center;'),
  plotlyOutput('myplot', height = 275)
)

hydrograph = tagList(
  # h5("Hydrograph",style = 'text-align:center;'),
  plotlyOutput('my_hydrograph', height = 275)
)

the_sidebar = sidebar(
  width = '20%',
  # height = '100%',
  trend_select_options#,
  # some value panels?
)


# # Absolute Panel with trend selection.
# trend_select_abs_panel = absolutePanel(
#   id = 'trend_selector',
#   top = 240, left = 10, width = 450, height = 550,
#   draggable = T,
#   tabsetPanel(
#     id = 'tabset',
#     tabPanel('Trend Options',trend_select_options_tab),
#     tabPanel('Station Plot',station_plot_tab)
#     # tabPanel('Datview',DT::DTOutput('test'))
#   )
# )

# # Absolute panel with map as background.
# map_panel = fluidRow(
#   div(
#     style="padding: 8px; border-bottom: 1px solid #CCC; background: #FFFFEE;",
#     fluidRow(
#       leafletOutput('leafmap',
#                     height = '600px')
#     )
#   )
# )

map = leafletOutput('leafmap',height = '500px')

main_bit = tagList(
  # fluidRow(
  #   column(6,
  #          station_plot
  #          ),
  #   column(6,
  #          hydrograph
  #          )
  # ),
  map,
  tabsetPanel(
    id = 'tabset',
    tabPanel(title = 'Flow Metric Plot', station_plot),
    tabPanel(title = 'Hydrograph', hydrograph)
  )
)
