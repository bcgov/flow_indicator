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
                 choices = c('Mean Flow' = 'Mean',
                             'Median Flow' = 'Median',
                             'Date of 50% Flow' = 'DoY_50pct_TotalQ',
                             'Minimum Flow (7-day)' = 'Min_7_Day',
                             'Date of Minimum Flow (7-day)' = 'Min_7_Day_DoY',
                             'Minimum Flow (30-day)' = 'Min_30_Day',
                             'Date of Minimum Flow (30-day)' = 'Min_30_Day_DoY',
                             'Total Flow' = 'Total_Volume_m3'),
                 selected = 'Mean',
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
    tabPanel('Station Plot',station_plot_tab),
    tabPanel('Datview',DT::DTOutput('test'))
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
