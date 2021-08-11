#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinycssloaders)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Victoria Geology information for GPS tracks"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("gpx",
                      "Upload gpx file",
                      multiple = FALSE,
                      accept = c(".gpx")
                      
            ),
            radioButtons("x_axis", label = "x-axis", choiceNames = c("distance (m)", "time"), choiceValues = c('d', 't'), inline = TRUE)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("main_map")
        )
        
        
    ),
    
    column(width = 10, 
           sliderInput("range", 
                label = "Range of interest:",
                min = 0, max = 100, value = c(0, 100), width= '100%'
           ), 
           offset = 1
    ),
    
    withSpinner(
        plotOutput("elePlot", width='100%', brush = "plot_brush", dblclick = "plot_click")
    ),
    
))