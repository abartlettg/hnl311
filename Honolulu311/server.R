# Honolulu 311
# Server
# Adriana Bartlett Gray

library(shiny)

shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        
        case_when(
            input$radio==1 & input$select %in% c(unique(hnl311_wrg$Area)) == TRUE ~
                hnl311_wrg %>%
                filter(Area==input$select) %>%
                ggplot(aes(HLReportType)) +
                geom_bar(),
            input$radio==1 & input$select %in% c(unique(hnl311_wrg$ComputedCity)) == TRUE ~
                hnl311_wrg %>%
                filter(ComputedCity==input$select) %>%
                ggplot(aes(HLReportType)) +
                geom_bar(),
            input$radio==1 & input$select %in% c(unique(hnl311_wrg_mhp$MedianHomePrice)) == TRUE ~
                hnl311_wrg_mhp %>%
                filter(MedianHomePrice==input$select) %>%
                ggplot(aes(HLReportType)) +
                geom_bar(),
            input$radio==2 & input$select %in% c(unique(hnl311_wrg$Area)) == TRUE ~ 
                hnl311_wrg %>%
                filter(Area==input$select) %>%
                ggplot(aes(HLReportType,WeeksToClose)) +
                geom_boxplot(),
            input$radio==2 & input$select %in% c(unique(hnl311_wrg$ComputedCity)) == TRUE ~
                hnl311_wrg %>%
                filter(ComputedCity==input$select) %>%
                ggplot(aes(HLReportType,WeeksToClose)) +
                geom_boxplot(),
            input$radio==2 & input$select %in% c(unique(hnl311_wrg_mhp$MedianHomePrice)) == TRUE ~
                hnl311_wrg_mhp %>%
                filter(MedianHomePrice=='900k+') %>%
                ggplot(aes(HLReportType,WeeksToClose)) +
                geom_boxplot(),
            input$radio==1 & input$select %in% c('ALL AREAS','ALL CITIES','ALL MHP RANGES') == TRUE ~
                ggplot(data = hnl311, aes(HLReportType)) +
                geom_bar(),
            input$radio==2 & input$select %in% c('ALL AREAS','ALL CITIES','ALL MHP RANGES') == TRUE ~
                ggplot(data = hnl311_nonulldates, aes(HLReportType, WeeksToClose)) +
                geom_boxplot()
        )
            
    })
})


