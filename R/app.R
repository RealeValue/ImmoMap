rm(list = ls())

library(shiny)
library(shinydashboard)
library(jsonlite)

source("extract_polygon_data.R")
source("data_module.R")
source("map_data.R")
source("verlauf.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ImmoMap"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          extract_polygon_data_ui("coords"),
          data_module_ui("data"),
          width = 4,
        ),

        # Show a plot of the generated distribution
        mainPanel(

          map_data_ui("result_data"),
          verlauf_ui("Verlauf"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  polygon_data <- extract_polygon_data_server("coords")

  daten <- data_module_server("data", polygon_data = polygon_data)


  map_data_server("result_data", daten = daten)
  verlauf_server("Verlauf", polygon_data = polygon_data)

}

# Run the application
shinyApp(ui = ui, server = server)
