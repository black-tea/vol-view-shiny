###############################
# ATSAC Volume Viewer UI Code #
###############################

### Libraries
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(datasets) # temporary!

### User Interface
ui <- fluidPage(
  titlePanel("ATSAC Volume Viewer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("type_select", "Type of Summary",
                   c("Intersection", "Corridor"),
                   selected = "Intersection", inline = TRUE, width = NULL),
      conditionalPanel(condition="input.type_select == 'Intersection'",
                       uiOutput("int_select"),
                       sliderInput("time_slider", label = "Hour Range", min = 0, 
                                   max = 23, value = c(14, 20)))),
    mainPanel(
      # Map Output
      leafletOutput("map"),
      plotOutput("volPlot"))))
  

