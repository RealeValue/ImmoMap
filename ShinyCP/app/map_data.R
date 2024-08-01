library(leaflet)
library(leaflet.extras)
library(tidyverse)

leaflet_gruppen <- c("only selected", "all")

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
    column(6,
           leafletOutput(ns("map"))),
    column(6,  tmapOutput(ns("tmap"))),
    column(2,  tableOutput(ns("summary"))),
    column(12, dataTableOutput(ns("data_table"))),
  )
}

#' map_data_server Server Functions
#'
#' @noRd
map_data_server <- function(id, data, ref_object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      input_map_list <- reactiveValuesToList(input)
      input_map_list <<- input_map_list
    })

    data_within_polygon <- reactive({
      data            <- data()
      data
    })


    output$map <- renderLeaflet({
      df_reference_object <- ref_object()

      marker_popup <-  paste(
        "<br>", "<b> ID:               </b>",  df_reference_object$Bewertungsnummer,
        "<br>", "<b> Objektart:        </b>",  df_reference_object$Objektart,
        "<br>", "<b> Ort:              </b>",  df_reference_object$Ort,
        "<br>", "<b> Strasse:          </b>",  df_reference_object$Strasse,
        "<br>", "<b> Hausnummer:       </b>",  df_reference_object$Hausnummer,
        "<br>",
        "<br>", "<b> Marktwert:        </b>",  df_reference_object$Marktwert %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Datum:            </b>",  df_reference_object$Aenderungsdatum,

        "<br>",

        "<br>"
      )


      leafMap <- leaflet(data = df_reference_object) %>%
        leaflet::addTiles() %>%
        addFullscreenControl() %>%
        leaflet::addProviderTiles("Stamen.Toner",      group = "Toner")    %>%
        leaflet::addProviderTiles("OpenTopoMap",       group = "TopoMap")  %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellit") %>%

        ## Layer Control
        leaflet::addLayersControl( baseGroups    = c("OSM (default)", "Toner", "TopoMap", "Satellit"),
                                   overlayGroups = leaflet_gruppen,
                                   options       = leaflet::layersControlOptions(collapsed = TRUE)) %>%
        addMarkers(df_reference_object$xco_wgs84, df_reference_object$yco_wgs84, popup = marker_popup) %>%
        setView(df_reference_object$xco_wgs84, df_reference_object$yco_wgs84, zoom = 9) %>%
        addDrawToolbar(
          targetGroup = "draw",
          polylineOptions = F, rectangleOptions = T, markerOptions = F, circleMarkerOptions = F, circleOptions = T,
          editOptions = editToolbarOptions(
            edit = FALSE, remove = TRUE,
            selectedPathOptions = selectedPathOptions()
          )
        )

      leafMap
    })

    output$tmap <- renderTmap({
      df_reference_object <- ref_object()
      zsp_dat_small       <- zsp_dat %>% dplyr::filter(bez_id %in% df_reference_object$bez_id)
      box                 <- df_reference_object %>% st_buffer(dist = 10000) %>% st_bbox()

      raster_small <- ra_score_raster %>% st_crop(box)

      df_reference_object %>%
        tm_shape(bbox = box) +
        tm_markers() +
        tm_shape(raster_small) +
        tm_raster(alpha = 0.6) +
        tm_shape(zsp_dat_small) +
        tm_borders()
    })



    observe({
      df         <- data_within_polygon()

      point_popup <-  paste(
        "<br>", "<b> ID:               </b>",  df$BewertungId,
        "<br>", "<b> Objektart:        </b>",  df$Objektart,
        "<br>",
        "<br>", "<b> Kaufpreis:               </b>",  df$Kaufpreis %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Kaufdatum:               </b>",  df$Kaufdatum,
        "<br>", "<b> Kaufpreis pro QM:        </b>", df$KaufpreisQM %>% round() %>% formattable::currency(symbol = Euro, digits = 0),
        "<br>", "<b> Erwerbsart:              </b>", df$Erwerbsart,
        "<br>", "<b> Kaufpreis valorisiert:   </b>",  df$PreisValorisiert %>% round(-3) %>% formattable::currency(symbol = Euro, digits = 0),

        "<br>",
        "<br>", "<b> Grundflaeche:  </b>",  df$Grundflaeche,
        "<br>", "<b> Wohnflaeche:   </b>",  df$NutzflaecheBerechnet,
        "<br>"
      )

      leafletProxy("map", data = df) %>%
        clearGroup(leaflet_gruppen) %>%
        addCircleMarkers(df$xco_wgs84, df$yco_wgs84,
                         radius = ifelse(df$Selected == 0, 1, 10),
                         color  = ifelse(df$Selected == 0, "black", "red"),
                         group  = ifelse(df$Selected == 1, leaflet_gruppen[1], leaflet_gruppen[2]),
                         fill   = TRUE, fillOpacity = 1,
                         popup = point_popup)


    })

    # observe({
    #   print("Zeichne Punkte neu!")
    #   df <- data_selected()
    #   df <- df %>% dplyr::mutate(Selected = if_else( row_number() %in% input$data_rows_selected, 0L, Selected))
    #

    #
    #
    #   leafletProxy("map", data = df) %>%
    #     clearGroup(group = "immonet") %>%
    #     leaflet::addCircleMarkers(
    #       lng = df$xco_wgs84, lat = df$yco_wgs84,
    #       group  = "immonet",
    #       popup = point_popup,
    #       color = if_else( df$Selected == 1, "red", "blue"),
    #       stroke = FALSE, fillOpacity = 0.7
    #     )
    # })
    #
    # poly_drawn <- reactive({ ## eventReactive(input$map_draw_new_feature, {
    #   feat   <- input$map_draw_new_feature
    #   if(is.null(feat)) {
    #     print("Generate empty polygon!")
    #     empty_poly <- st_sf(st_sfc(), crs = 4326)
    #     return(empty_poly)
    #   }
    #   if( ! is.null(input$map_draw_deletestop)) {
    #     if(input$map_draw_deletestop & input$map_draw_new_feature$properties$`_leaflet_id` == input$map_draw_deleted_features$features[[1]]$properties$`_leaflet_id`) {
    #       print("Feature wurde geloescht!")
    #       empty_poly <- st_sf(st_sfc(), crs = 4326)
    #       return(empty_poly)
    #     }
    #   }
    #
    #   coords <- unlist(feat$geometry$coordinates)
    #   coords <- matrix(coords, ncol = 2, byrow = T)
    #   poly   <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
    #   poly
    # })
    #
    # leaflet_id <- reactive({
    #   if(is.null(input$map_draw_new_feature$properties$`_leaflet_id`) & ! is.null(polygon_data())) id <- 1 else id <- input$map_draw_new_feature$properties$`_leaflet_id`
    #   print(paste("Leaflet ID:", id))
    #   id
    # })

  #   data_selected <- eventReactive(leaflet_id(), { ## reactive({
  #     print("Evaluliere Daten!")
  #
  #     poly           <- poly_drawn()
  #     poly_all <<- poly
  #     df     <- daten()
  #
  #     if( nrow(poly) == 0) {
  #       print("Poly leer! Returniere alle Daten!")
  #       df <- df %>% dplyr::mutate(Selected = 1L)
  #     } else {
  #       print("Verschneide mit Polygon!")
  #       df <- df %>%
  #         st_as_sf(coords = c("xco_wgs84", "yco_wgs84"), crs = 4326) %>%
  #         st_intersection(poly) %>%
  #         st_drop_geometry() %>%
  #         dplyr::select(BewertungId) %>%
  #         dplyr::mutate(Selected = 1L) %>%
  #         dplyr::right_join(df, by = "BewertungId") %>%
  #         dplyr::mutate(Selected = if_else(is.na(Selected), 0L, Selected))
  #     }
  #
  #     df_all <<- df
  #     df
  #
  #
  #   })
  #
    output$data_table <- renderDT({
      df         <- data_within_polygon()

      data_table <- df %>%
        st_drop_geometry() %>%
        dplyr::filter(Selected == 1) %>%
        dplyr::mutate(KaufpreisQM = KaufpreisQM %>% round(-1)) %>%
        dplyr::select(Id = BewertungId, Kaufpreis, Kaufdatum, KaufpreisQM, Zins = zins, PreisValorisiert,
                      ## Bezugszeitraum = zeit_diff_jahre,
                      Strasse, Hausnummer, Grundflaeche, NutzflaecheBerechnet, Objektart,
                      Tagebuchzahl, EintrageJahr)

      data_table %>%
        datatable(
          rownames = FALSE,
          escape = FALSE,
          ## editable = list(target = "cell", disable = list(columns = 3)),
          selection = list(mode = "multiple", target = "row")
          #callback = JS(js("data", session$ns))
        )
    }, server = TRUE)


    output$summary <- renderTable({
      df <- data_within_polygon()

      df %>%
        st_drop_geometry() %>%
        dplyr::filter(Selected == 1) %>%
        dplyr::summarise(
          Kaufpreis        = mean(Kaufpreis, na.rm = TRUE) %>% round(-3),
          PreisValorisiert = mean(PreisValorisiert, na.rm = TRUE) %>% round(-3)
        )
    })


  })
}

