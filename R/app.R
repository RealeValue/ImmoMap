rm(list = ls())

library(shiny)
library(shinydashboard)
library(jsonlite)

source("id_module.R")
source("action_module.R")
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
          id_module_ui("uuid"),
          extract_polygon_data_ui("coords"),
          data_module_ui("data"),
          action_module_ui("start"),
          width = 2,
        ),

        # Show a plot of the generated distribution
        mainPanel(

          verlauf_ui("Verlauf"),
          map_data_ui("result_data"),

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  id_module_server("uuid")
  action_module <- action_module_server("start")

  polygon_data <- extract_polygon_data_server("coords", action = action_module)
  daten        <- data_module_server("data", action = action_module, polygon_data = polygon_data)


  map_data_server("result_data", polygon_data = polygon_data, daten = daten)
  verlauf_server("Verlauf",      polygon_data = polygon_data, action = action_module)

}

# Run the application
shinyApp(ui = ui, server = server)
