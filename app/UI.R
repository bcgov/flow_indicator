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
                             'Low Flow (3-day)' = 'Min_3_Day',
                             'Date of Low Flow (7-day)' = 'Min_7_Day_DoY',
                             'Date of Low Flow (3-day)' = 'Min_3_Day_DoY',
                             'Peak Flow (7-day)' = 'Max_7_Day',
                             'Peak Flow (3-day)' = 'Max_3_Day',
                             'Date of Peak Flow (7-day)' = 'Max_7_Day_DoY',
                             'Date of Peak Flow (3-day)' = 'Max_3_Day_DoY'),
                 selected = 'Mean',
                 width = '100%'),
  radioButtons(inputId = 'user_period_choice',
               label = 'Timespan for Trend Analysis',
               choices = c('Recent (2010 - 2022)' = '2010+',
                           'Three decades (1990 - 2022)' = '1990+',
                           'All available years' = 'all'),
               selected = 'all',
               inline = F)
)

# station_plot_tab = wellPanel(
#   plotOutput('myplot', height = 300)
# )

flow_metric_plot_tab = card(
  card_body(
    plotOutput('myplot', height = 300)
  )
)

hydrograph_plot_tab = card(
  card_body(
    plotOutput('myhydrograph', height = 300)
  )
)

# Absolute Panel with trend selection.
trend_select_abs_panel = absolutePanel(
  id = 'trend_selector',
  top = 240, left = 10, width = 450, #height = 800,
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
                    height = '600px')
    )
  )
)
