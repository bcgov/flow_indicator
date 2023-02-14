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

output$month_selector_UI = renderUI({
  if(input$time_scale == 'Annual') return(NULL)
  selectizeInput(inputId = 'month_selector',
                 label = 'Month',
                 multiple = F,
                 choices = month.abb,
                 selected = month.abb[1])
})
