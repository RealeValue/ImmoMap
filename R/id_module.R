library(uuid)

#' id_module_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
id_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    textOutput(ns("text"))
  )
}

#' id_module_server Server Functions
#'
#' @noRd
id_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    uuid <- reactive({
      UUIDgenerate()

    })

    output$text <- renderText({
      id <- uuid()
      print(id)
    })

  return(uuid)
  }
  )
}
