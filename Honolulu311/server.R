
# Honolulu 311
# Server
# Adriana Bartlett Gray


shinyServer(function(input, output) {
  state <- reactiveValues()
  
  observe({
    state$radio <- input$radio
    state$geom <- ifelse(input$radio==1, 'geom_bar()', 'geom_boxplot()')
    state$ggplot <- ifelse(input$radio==1, 'ggplot(aes(HLReportType))', 'ggplot(aes(HLReportType,WeeksToClose))')
    state$dfvarcombo <- ifelse(input$select %in% c(unique(hnl311_wrg_mhp$Area)),hnl311_wrg_mhp$Area,0) # Area,),
    state$dfvarcombo <- ifelse(input$select %in% c(unique(hnl311_wrg_mhp$ComputedCity)),hnl311_wrg_mhp$ComputedCity,0) # ComputedCity,),
    state$dfvarcombo <- ifelse(input$select %in% c(unique(hnl311_wrg_mhp$MedianHomePrice)),hnl311_wrg_mhp$MedianHomePrice,0) # MedianHomePrice)
    state$dfvarcombo <- ifelse(input$select %in% c('ALL AREAS','ALL CITIES','ALL MHP RANGES') & state$radio==1, hnl311,0)
    state$dfvarcombo <- ifelse(input$select %in% c('ALL AREAS','ALL CITIES','ALL MHP RANGES') & state$radio==2, hnl311_nonulldates,0)
    
    if(state$dfvarcombo == hnl311 | state$dfvarcombo == hnl311_nonulldates) {
      df = state$dfvarcombo
    }
    else {
      df = filter(state$dfvarcombo == input$select)
      }
    })
})
  
  output$distPlot <- renderPlot({
    
    df %>%
      state$ggplot %>%
      state$geom
    
  }
  )

  
  