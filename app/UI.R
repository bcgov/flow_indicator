library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(envreportutils)
library(sf)
library(EnvStats)
library(data.table)
library(tidyverse)
library(ggtext)
library(shinyjs)
library(shinyBS)

# Trend selection options
trend_select_options_tab = wellPanel(
  fluidRow(
    column(width= 8,
    selectizeInput(inputId = 'hydrozone_choice',
                   label = 'Hydrologic Zone',
                   choice = c('All',
                              "Northern Coast Mountains",
                              "Stikine Plateau",
                              "Haida Gwaii",
                              "Central Coast Mountains",
                              "Northern Rocky Mountains",
                              "Northern Central Uplands",
                              "Western Vancouver Island",
                              "Western South Coast Mountains",
                              "Southern Hazelton Mountains",
                              "Eastern Vancouver Island",
                              "Central South Coast Mountains",
                              "Nechako Plateau",
                              "Eastern South Coast Mountains",
                              "Northern Interior Plains",
                              "Fraser Plateau",
                              "Southern Rocky Mountain Foothills",
                              "Southern Interior Plains",
                              "Mcgregor Basin",
                              "Northern Columbia Mountains",
                              "Northern Thompson Plateau",
                              "Southern Quesnel Highland",
                              "Upper Fraser Basin",
                              "Southern Thompson Plateau",
                              "Okanagan Highland",
                              "Lower Columbia Basin",
                              "Upper Columbia Basin",
                              "Lower Kootenay Basin",
                              "Upper Kootenay Basin",
                              "Central Kootenay Basin")
    )
    ),
    div(style = "margin-top:2.5rem;",
        column(
      width = 2,
  actionButton("reset_inputs","Reset")),
  column(width = 2)),
  ),
  fluidRow(
    column(width = 6,
           radioButtons(inputId = 'time_scale',
                        label = 'Yearly or Monthly Data',
                        choices = c('Annual','Monthly'),
                        selected = 'Annual'
           )
    ),
    column(width = 6,
           uiOutput('month_selector_UI')
    )
  ),
  selectizeInput(inputId = 'user_var_choice',
                 label = 'Trend to Display',
                 choices = c('Average Flow' = 'Average',
                             'Date of Freshet' = 'DoY_50pct_TotalQ',
                             'Low Flow (7-day)' = 'Min_7_Day',
                             'Date of Low Flow (7-day)' = 'Min_7_Day_DoY',
                             'Peak Flow (3-day)' = 'Max_3_Day',
                             'Date of Peak Flow (3-day)' = 'Max_3_Day_DoY'),
                 selected = 'Mean',
                 width = '100%'),
  fluidRow(
    column(width = 7,
           radioButtons(inputId = 'user_period_choice',
                        label = 'Timespan for Trend Analysis',
                        choices = c(
                                    'Three decades (1992 - 2022)' = '1990+',
                                    'All available years' = 'all'),
                        selected = 'all',
                        inline = F)
    ),
    column(width = 5,
           checkboxInput(inputId = 'recent',
                         label = 'Include recently installed stations (1990+)',
                         value = FALSE),
           checkboxInput(inputId = 'upstream',
                         label = 'Include upstream stations',
                         value = F)
    )
  )
)

flow_metric_plot_tab = card(
  card_body(
    plotOutput('myplot', height = 400)
  )
)

hydrograph_plot_tab = card(
  card_body(
    plotOutput('myhydrograph', height = 400)
  )
)

# Absolute Panel with trend selection.
trend_select_abs_panel = absolutePanel(
  id = 'trend_selector',
  top = 200, left = 10, width = 450, #height = 800,
  draggable = F,
  tabsetPanel(
    id = 'tabset',
    tabPanel('Trend Options',trend_select_options_tab),
    tabPanel('Flow Metric Plot',flow_metric_plot_tab),
    tabPanel('Station Hydrograph', hydrograph_plot_tab)
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
                    height = '650px')
    )
  )
)
