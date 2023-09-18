library(leaflet)
library(leaflet.extras)
library(tidyverse)

server_adresse <- "http://ds.realevalue.at:5164/"

Euro <- "\u20AC"
choices_objektart <- c("Eigentumswohnung" = 10, "Einfamilienhaus" = 20, "Zweifamilienhaus" = 21, "Reihenhaus" = 30,
                       "Doppelhaus" = 31, "Kleingartenhaus" = 40, "Grundstück" = 50)


shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

js <- function(dtid, ns) {
  c(
    "$('[id^=checkb]').on('click', function(){",
    "  var id = this.getAttribute('id');",
    "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
    "  var value = $(this).prop('checked');",
    "  var info = [{row: i, col: 3, value: value}];",
    sprintf(
      "Shiny.setInputValue('%s', info);",
      ns(sprintf("%s_cell_edit:DT.cellInfo", dtid))
    ),
    "})"
  )
}



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
    column(6,  tableOutput(ns("summary"))),
    column(6,  leafletOutput(ns("map"))),
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


      leafMap <- leaflet(data = df_immonet) %>%
        leaflet::addTiles() %>%
        addFullscreenControl() %>%
        leaflet::addProviderTiles("Stamen.Toner", group = "Toner") %>%
        leaflet::addProviderTiles( "OpenTopoMap", group = "TopoMap") %>%
        leaflet::addProviderTiles( "Esri.WorldImagery", group = "Satellit") %>%

        ## Layer Control
        leaflet::addLayersControl( baseGroups    = c("OSM (default)", "Toner", "TopoMap", "Satellit"),
                                   options       = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        addMarkers(df$xco_wgs84, df$yco_wgs84) %>%
        addDrawToolbar(
          targetGroup = "draw",
          polylineOptions = F, rectangleOptions = F, markerOptions = F, circleMarkerOptions = F, circleOptions = F,
          editOptions = editToolbarOptions(
            edit = FALSE, remove = TRUE,
            selectedPathOptions = selectedPathOptions()
          )
        )

      leafMap
    })

    observe({
      df_immonet <- data_selected()

      point_popup <-  paste(
        "<br>", "<b> ID:               </b>",  df_immonet$BewertungId,
        "<br>", "<b> Objektart:        </b>",  df_immonet$Objektart,
        "<br>",
        "<br>", "<b> Kaufpreis:               </b>",  df_immonet$Kaufpreis %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Kaufdatum:               </b>",  df_immonet$Kaufdatum,
        "<br>", "<b> Kaufpreis pro QM:        </b>", df_immonet$KaufpreisQM %>% round() %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Erwerbsart:              </b>", df_immonet$Erwerbsart,
        "<br>", "<b> Kaufpreis valorisiert:   </b>",  df_immonet$NettoPreisValorisiert %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> QM Preis valorisiert:    </b>",  df_immonet$PreisProQMValorisiert,

        "<br>",
        "<br>", "<b> Grundflaeche:  </b>",  df_immonet$Grundflaeche,
        "<br>", "<b> Wohnflaeche:   </b>",  df_immonet$NutzflaecheBerechnet,
        "<br>"
      )


      leafletProxy("map", data = df_immonet) %>%
        clearGroup(group = "immonet") %>%
        leaflet::addCircleMarkers(
          lng = df_immonet$xco_wgs84, lat = df_immonet$yco_wgs84,
          group  = "immonet",
          popup = point_popup,
          color = if_else( df_immonet$Selected == 1, "red", "blue"),
          stroke = FALSE, fillOpacity = 0.7
        )
    })

    poly_drawn <- reactive({ ## eventReactive(input$map_draw_new_feature, {
      input_map_list <- reactiveValuesToList(input)
      input_map_list <<- input_map_list

      feat   <- input$map_draw_new_feature
      if(is.null(feat)) {
        empty_poly <- st_sf(st_sfc(), crs = 4326)
        return(empty_poly)
      }
      if( ! is.null(input$map_draw_deletestop)) {
        if(input$map_draw_deletestop & input$map_draw_new_feature$properties$`_leaflet_id` == input$map_draw_deleted_features$features[[1]]$properties$`_leaflet_id`) {
          print("Feature wurde geloescht!")
          empty_poly <- st_sf(st_sfc(), crs = 4326)
          return(empty_poly)
        }
      }

      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)
      poly   <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
      poly
    })

    data_selected <- reactive({
      poly           <- poly_drawn()
      poly_all <<- poly
      df_immonet     <- daten()

      if( nrow(poly) == 0) return( df_immonet %>% dplyr::mutate(Selected = 1L))

      df_immonet <- df_immonet %>%
        st_as_sf(coords = c("xco_wgs84", "yco_wgs84"), crs = 4326) %>%
        st_intersection(poly) %>%
        st_drop_geometry() %>%
        dplyr::select(BewertungId) %>%
        dplyr::mutate(Selected = 1L) %>%
        dplyr::right_join(df_immonet, by = "BewertungId") %>%
        dplyr::mutate(Selected = if_else(is.na(Selected), 0L, Selected))

      df_immonet_all <<- df_immonet
      df_immonet
    })

    output$data <- renderDT({
      df_immonet <- data_selected() ## data_selected()

      df_immonet <- df_immonet %>% dplyr::filter(Selected == 1)

      dat_display <-  cbind(
        check = shinyInput(checkboxInput, nrow(df_immonet), "checkb"),
        df_immonet %>% dplyr::select(Id = BewertungId, Kaufpreis, Kaufdatum, KaufpreisQM, zins, NettoPreisValorisiert, PreisProQMValorisiert, Strasse, Hausnummer, Grundflaeche, NutzflaecheBerechnet, Objektart, Tagebuchzahl, EintrageJahr)
      )
      dat_display_all <<-dat_display

      dat_display %>%
        datatable(
          rownames = FALSE,
          escape = FALSE,
          editable = list(target = "cell", disable = list(columns = 3)),
          selection = "none",
          callback = JS(js("data", session$ns))
        )
    }, server = FALSE)


    observeEvent(input[["data_cell_edit"]], {
      ## Here is a bug !!
      rows_selected <- c()
      rows_selected <- c(rows_selected, input[["data_cell_edit"]]$row)
      print(rows_selected)
    })



    output$summary <- renderTable({
      df_immonet <- data_selected()

      df_immonet %>%
        dplyr::filter(Selected == 1) %>%
        dplyr::reframe(
          Lage        = c("maessig", "durchschnittlich", "gut", "Bestlage"),
          Kaufpreis   = quantile(Kaufpreis,   c(0.25, 0.5, 0.75, 0.99), na.rm = T) %>% round(-3) %>% formattable::currency(symbol = "€", digits = 0),
          KaufpreisQM = quantile(KaufpreisQM, c(0.25, 0.5, 0.75, 0.99), na.rm = T) %>% round(-1) %>% formattable::currency(symbol = "€", digits = 0),

          KP_Index   = quantile(NettoPreisValorisiert, c(0.25, 0.5, 0.75, 0.99), na.rm = T) %>% round(-3) %>% formattable::currency(symbol = "€", digits = 0),
          KPQM_Index = quantile(PreisProQMValorisiert, c(0.25, 0.5, 0.75, 0.99), na.rm = T) %>% round(-1) %>% formattable::currency(symbol = "€", digits = 0),
          )
    })

  })
}

