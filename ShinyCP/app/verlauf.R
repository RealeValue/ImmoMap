library(tmap)
library(ggrepel)

marktschwankung <- readRDS("S:/liebe_irg/LIEBE/cpechhacker/Validierung/Marktmonitoring/MarktmonitoringIndex.rds")

choices_bez <- marktschwankung %>% dplyr::ungroup() %>%  dplyr::distinct(bez_id) %>% pull()

#' verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
verlauf_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(2,
           selectInput(ns("Modelltyp"), "Modelltyp", choices = c("EFH", "ETW"), multiple = F),
           selectInput(ns("bez_id"), "BezirkId",     choices = c("", choices_bez),  multiple = F),
           ),

    column(4, tableOutput(ns("tabelle"))),
    column(6, plotOutput(ns("verlauf_plot"))),
  )
}

#' verlauf_server Server Functions
#'
#' @noRd
verlauf_server <- function(id){  ## , action, polygon_data
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      input_verlauf  <- reactiveValuesToList(input)
      input_verlauf <<- input_verlauf
    })

    verlauf_data <- reactive({ ##  eventReactive(action(),{
      print("Suche nach Angaben!")


      if( is.null(input$Modelltyp) | is.null(input$bez_id))  shiny::validate("Waehlen Sie einen Modelltyp und einen Bezirk")

      daten_filter <- marktschwankung %>%
        dplyr::filter(bez_id == input$bez_id, Modelltyp == input$Modelltyp)
      daten_filter
    })

    # observeEvent(polygon_data(), {
    #   print("Update data!")
    #   df_coords <- polygon_data()
    #   df_coords <<- df_coords
    #   if("data.frame" %in% class(df_coords)) updateSelectInput(session, "bez_id", selected = df_coords$bez_id)
    # })

    output$tabelle <- renderTable({
      daten_filter <- verlauf_data()

      daten_filter %>%
        dplyr::ungroup() %>%
        dplyr::select(Jahr, diff_mean, Index) %>%
        dplyr::mutate(
          diff_mean = formattable:::percent(diff_mean),
          Index = round(Index)
          )
    })

    output$verlauf_plot <- renderPlot({
      daten_filter <- verlauf_data()
      print("Plot graphic")

      p_index2 <- daten_filter %>%
        ggplot() +
        geom_line(aes(x = Jahr, y = Index),       linewidth = 1.5) +
        geom_segment(aes(x = Jahr, xend = Jahr, y = Index_lower, yend = Index_upper)) +

        geom_label_repel(aes(x = Jahr, y = Index_lower, label = Index_lower %>% round()), color = 'grey', size = 2.8) +
        geom_label_repel(aes(x = Jahr, y = Index_upper, label = Index_upper %>% round()), color = 'grey', size = 2.8) +
        geom_label_repel(aes(x = Jahr, y = Index,       label = Index       %>% round()), color = 'black', size = 3.5) +
        theme_minimal() +
        ggtitle(label = paste("Indexverlauf"))

      p_index2
    })

  })
}

