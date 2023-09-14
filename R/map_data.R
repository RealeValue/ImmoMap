library(leaflet)
library(leaflet.extras)
library(tidyverse)

server_adresse <- "http://ds.realevalue.at:5164/"

Euro <- "\u20AC"
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
  fluidRow(
    column(3,  tableOutput(ns("summary"))),
    column(9,  leafletOutput(ns("map"))),
    column(12, dataTableOutput(ns("data"))),
  )
}

#' map_data_server Server Functions
#'
#' @noRd
map_data_server <- function(id, polygon_data, daten){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$map <- renderLeaflet({
      df_immonet <- daten()
      df         <- polygon_data()

      df_immonet_all <<- df_immonet

      leafMap <- leaflet(data = df_immonet) %>%
        addTiles() %>%
        addFullscreenControl() %>%
        leaflet::addProviderTiles("Stamen.Toner", group = "Toner") %>%
        leaflet::addProviderTiles( "OpenTopoMap", group = "TopoMap") %>%
        leaflet::addProviderTiles( "Esri.WorldImagery", group = "Satellit") %>%

        ## Layer Control
        leaflet::addLayersControl( baseGroups    = c("OSM (default)", "Toner", "TopoMap", "Satellit"),
                                   options       = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        addMarkers(df$xco_wgs84, df$yco_wgs84)

      point_popup <-  paste(
        "<br>", "<b> ID:               </b>",  df_immonet$BewertungId,
        "<br>", "<b> Objektart:        </b>",  df_immonet$Objektart,
        "<br>",
        "<br>", "<b> Kaufpreis:        </b>",  df_immonet$Kaufpreis %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Kaufdatum:        </b>",  df_immonet$Kaufdatum,
        "<br>", "<b> Kaufpreis pro QM:  </b>", df_immonet$KaufpreisQM %>% round() %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Erwerbsart:        </b>", df_immonet$Erwerbsart,
        "<br>",
        "<br>", "<b> Grundflaeche:  </b>",  df_immonet$Grundflaeche,
        "<br>", "<b> Wohnflaeche:   </b>",  df_immonet$NutzflaecheBerechnet,
        "<br>"
      )


      leafMap <- leafMap %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
          lng = ~ xco_wgs84, lat = ~ yco_wgs84,
          popup = point_popup,
          stroke = FALSE, fillOpacity = 0.7
        )


      leafMap
    })

    output$data <- renderDataTable({
      df_immonet <- daten()

      df_immonet %>%
        dplyr::select(Id = BewertungId, Kaufpreis, KaufpreisQM, Kaufdatum, Grundflaeche, NutzflaecheBerechnet, Objektart, Tagebuchzahl, EintrageJahr)
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

