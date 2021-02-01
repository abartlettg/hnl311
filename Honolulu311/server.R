# SERVER 
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

hnl311 <- read_csv("hnl311.csv")
hnl311_nonulldates <- read_csv("hnl311_nonulldates.csv")
hnl311_wrg <- read_csv("hnl311_wrg.csv")
hnl311_wrg_mhp <- read_csv("hnl311_wrg_mhp.csv")

library(shiny)

function(input, output) {

    output$count <- renderPlot({

        hnl311_wrg %>%
            filter(Area==input$area) %>%
            ggplot(aes(HLReportType)) +
            geom_bar()
        
        
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

}
