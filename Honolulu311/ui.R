# UI
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


fluidPage(

    titlePanel("Honolulu 311"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = 'area',
                           label = 'Area',
                           choices = unique(hnl311_wrg$Area)
        ),

        
        mainPanel(
            plotOutput(outputId = "count")
        )
    )))