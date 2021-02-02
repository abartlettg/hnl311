# Honolulu 311
# UI
# Adriana Bartlett Gray

library(shiny)


shinyUI(fluidPage(

    # Application title
    titlePanel("Honolulu 311"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("radio", label = h3("Report Types:"),
                         choices = list("By Volume:" = 1, "By Time To Close" = 2), 
                         selected = 1),
            selectInput("select", label = h3("Select By Area or City or Median Home Price Range:"), 
                        choices = c('ALL AREAS',(unique(hnl311_wrg$Area)),'ALL CITIES',unique(hnl311_wrg$ComputedCity),
                                    'ALL MHP RANGES','300ks','400ks','500ks','600ks','700ks','800ks','900ks'),
                        selected = 1),
            selectInput("select", label = h3("By City"), 
                        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                        selected = 1),
            selectInput("select", label = h3("By Median Home Price Range:"), 
                        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                        selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
