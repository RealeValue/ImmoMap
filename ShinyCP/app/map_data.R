library(leaflet)
library(leaflet.extras)
library(tidyverse)

leaflet_gruppen <- c("only selected", "all")

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
           sliderInput(ns("n_vergleichsobjekte"), "Anzahl Vergleichsobjekte", value = 10, min = 3, max = 30),
           leafletOutput(ns("map"))),
    # column(6,  tmapOutput(ns("tmap"))),
    column(2,  dataTableOutput(ns("summary"))),
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
      poly            <- select_polygon()

      data %>% st_intersection(poly) %>% find_selected_objects(AnzahlVergleichsobjekte = input$n_vergleichsobjekte)
    })


    select_polygon <- reactive({

      df_reference_object <- ref_object()
      polygon_selected    <- df_reference_object %>% st_buffer(dist = 5 * 1000) %>% dplyr::select(geometry)

      if(is.null(input$map_draw_new_feature)) return(polygon_selected)

      feat   <- input$map_draw_new_feature

      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)

      if(feat$properties$feature_type == "circle") {
        coords <- tibble(x = coords[1], y = coords[2]) %>% st_as_sf(coords = c("x", "y"), crs = st_crs(zsp_dat)) %>%
          st_buffer(feat$properties$radius)
        return(coords)
      }
      poly   <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)

      poly
    })

    observe({
      polygon_selected <- select_polygon()
      coords <- polygon_selected %>% st_coordinates()

      leafletProxy("map", data = df) %>%
        clearGroup("poly") %>%
        addPolygons(coords[,1], coords[,2], group = "poly")
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
        "<br>", "<b> Datum:            </b>",  df_reference_object$Aenderungsdatum
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

    # output$tmap <- renderTmap({
    #   df_reference_object <- ref_object()
    #   zsp_dat_small       <- zsp_dat %>% dplyr::filter(bez_id %in% df_reference_object$bez_id)
    #   box                 <- df_reference_object %>% st_buffer(dist = 10000) %>% st_bbox()
    #
    #   raster_small <- ra_score_raster %>% st_crop(box)
    #
    #   df_reference_object %>%
    #     tm_shape(bbox = box) +
    #     tm_markers() +
    #     tm_shape(raster_small) +
    #     tm_raster(alpha = 0.6) +
    #     tm_shape(zsp_dat_small) +
    #     tm_borders()
    # })



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


      if( ! is.null(input$data_table_rows_selected))
        df <- df %>% dplyr::mutate(Selected = if_else(row_number() %in% input$data_table_rows_selected, 1L, 0L))

      leafletProxy("map", data = df) %>%
        clearGroup(leaflet_gruppen) %>%
        addCircleMarkers(df$xco_wgs84, df$yco_wgs84,
                         radius = ifelse(df$Selected == 0, 1, 10),
                         color  = ifelse(df$Selected == 0, "black", "red"),
                         group  = ifelse(df$Selected == 1, leaflet_gruppen[1], leaflet_gruppen[2]),
                         fill   = TRUE, fillOpacity = 1,
                         popup = point_popup)


    })


    table_formatting <- reactive({
      df         <- data_within_polygon()

      data_table <- df %>%
        st_drop_geometry() %>%
        dplyr::arrange(desc(Selected)) %>%
        dplyr::mutate(KaufpreisQM = KaufpreisQM %>% round(-1)) %>%
        dplyr::select(Id = BewertungId, Kaufpreis, Kaufdatum, KaufpreisQM, Zins = zins, PreisValorisiert,
                      Bezugszeitraum = zeit_diff_jahre,

                      Strasse, Hausnummer, Grundflaeche, NutzflaecheBerechnet, Objektart,
                      Tagebuchzahl, EintrageJahr)
      data_table
    })

    output$data_table <- renderDT({
      data_table <- table_formatting()
      ## select all column (indixes) with expection of index of column "Zins" (index is 0 based)
      index <- 1:length(names(data_table))
      index <- index[-which(names(data_table) == "Zins")] - 1


      data_table %>%
        datatable(
          rownames = FALSE,
          escape = FALSE,
          editable = list(target = "cell", disable = list(columns = index)),
          selection = list(mode = "multiple", target = "row", selected = 1:input$n_vergleichsobjekte)
          #callback = JS(js("data", session$ns))
        )
    }, server = TRUE)


    # edit a single cell (see https://yihui.shinyapps.io/DT-edit/)
    proxy = dataTableProxy('data_table')
    observeEvent(input$data_table_cell_edit, {
      data_table_neu <- table_formatting()
      daten_alt <<- data_table_neu

      info = input$data_table_cell_edit
      info$col <- info$col + 1 ## 0 based
      info

      data_table_neu <<- editData(data_table_neu, info) %>%
        dplyr::mutate(PreisValorisiert = Kaufpreis * (1 + Zins)^Bezugszeitraum)

      ## ToDO Update Table
      ## replaceData(proxy, data_table_neu, resetPaging = FALSE)  # important
    })

    output$summary <- renderDT({
      df <- data_within_polygon()

      df %>%
        st_drop_geometry() %>%
        dplyr::select(Kaufpreis, KaufpreisQM, PreisValorisiert) %>%
        dplyr::filter(row_number() %in% input$data_table_rows_selected) %>%
        dplyr::summarise_all(
          c(Minimum            = function(x) min(x, na.rm = T),
            Mittelwert         = function(x) mean(x, na.rm = T),
            Maximum            = function(x) max(x, na.rm = T),
            Standardabweichung = function(x) sd(x, na.rm = T),
            Anzahl             = function(x) sum(!is.na(x))
            )
        ) %>%
        pivot_longer(everything()) %>%
        separate(name, into = c("Kaufpreis", "Statistik")) %>%
        pivot_wider(names_from = "Kaufpreis") %>%
        datatable(options = list(dom = 't'), rownames = FALSE) %>%
        formatCurrency(c("Kaufpreis", "KaufpreisQM", "PreisValorisiert"), currency = "€", before = F, interval = 3, mark = ".")
    })


    data_selected <- reactive({
      df <- data_within_polygon()

      df %>%
        dplyr::filter(row_number() %in% input$data_table_rows_selected)
    })

    return(data_selected)
  })
}

