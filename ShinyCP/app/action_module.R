#' action_module_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
action_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("start"), "Start!"),
  )
}

#' action_module_server Server Functions
#'
#' @noRd
action_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Action <- reactive({
      input$start
    })

    return(Action)
    }
  )
}
