flow_variables = reactive({
  if(input$time_scale == 'Annual'){
    return(
      c('Mean Flow',
        'Median Flow',
        'Date of 50% Annual Flow',
        'Minimum Flow (7day)',
        'Total Flow'
      )
    )
  }
  if(input$time_scale == 'Monthly'){
    return(
      c('Mean Flow',
        'Median Flow',
        'Minimum Flow (7day)',
        'Total Flow'
      )
    )
  }
})

# output$month_selector_UI = renderUI({
#   if(!input$time_scale == 'Monthly') return(NULL)
#   selectizeInput(inputId = 'month_selector',
#                  label = 'Month',
#                  multiple = F,
#                  choices = month.abb,
#                  selected = month.abb[1])
# })

# output$time_selector_ui = renderUI({
#   if(input$custom_daterange) return(NULL)
#   selectizeInput(inputId = 'time_selector',
#                label = 'Time Selector',
#                multiple = F,
#                choices = c('Annual' = 'All',
#                            month.abb),
#                selected = 'Annual')
# })

output$custom_daterange_selectors = renderUI({
  if(!input$custom_daterange) return(NULL)
  tagList(
    fluidRow(
      column(width = 6,
             selectizeInput(inputId = 'start_month',
                            label = 'Start Month',
                            choices = c(1:12),
                            selected = 1)
      ),
      column(width = 6,
             numericInput(inputId = 'start_day',
                          label = 'Start Day',
                          min = 1,
                          max = 31,
                          value = 1)
      ),
    ),
    fluidRow(
      column(width = 6,
             selectizeInput(inputId = 'end_month',
                            label = 'End Month',
                            choices = c(1:12),
                            selected = 3)
      ),
      column(width = 6,
             numericInput(inputId = 'end_day',
                          label = 'End Day',
                          min = 1,
                          max = 31,
                          value = 31)
      )
    )
  )
})
