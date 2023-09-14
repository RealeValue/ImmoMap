library(leaflet)
library(tidyverse)

server_adresse <- "http://ds.realevalue.at:5164/"

choices_objektart <- c("Eigentumswohnung" = 10, "Einfamilienhaus" = 20, "Zweifamilienhaus" = 21, "Reihenhaus" = 30,
                       "Doppelhaus" = 31, "Kleingartenhaus" = 40, "Grundstück" = 50)




#' map_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
map_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    leafletOutput(ns("map")),
    tableOutput(ns("summary")),
    dataTableOutput(ns("data")),

  )
}

#' map_data_server Server Functions
#'
#' @noRd
map_data_server <- function(id, daten){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      df_immonet <- daten()
      df_immonet_all <<- df_immonet

      leafMap <- leaflet(data = df_immonet) %>%
        addTiles() %>%
        ## addProviderTiles(c("Satellite", "Stamen.Toner")) %>%
        addMarkers(~xco_wgs84 , ~yco_wgs84, popup = ~as.character(Kaufpreis))
      leafMap
    })

    output$data <- renderDataTable({
      df_immonet <- daten()

      df_immonet
    })

    output$summary <- renderTable({
      df_immonet <- daten()

      df_immonet %>%
        dplyr::reframe(
          Lage        = c("maessig", "durchschnittlich", "gut", "Bestlage"),
          Kaufpreis   = quantile(Kaufpreis,   c(0.25, 0.5, 0.75, 1), na.rm = T) %>% round(-3),
          KaufpreisQM = quantile(KaufpreisQM, c(0.25, 0.5, 0.75, 1), na.rm = T) %>% round(-1),
          )
    })

  })
}

