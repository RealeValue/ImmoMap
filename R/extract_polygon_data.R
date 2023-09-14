load("S:/Rohdaten/Aufbereitung/Lagedaten/shapes/zsp_2017.Rdata")
library(sf)

#' verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
extract_polygon_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(ns("xco_wgs84"), "X - Koordinate", value = NA_real_),
    numericInput(ns("yco_wgs84"), "Y - Koordinate", value = NA_real_),
  )
}

#' extract_polygon_data_server Server Functions
#'
#' @noRd
extract_polygon_data_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

   polygon_data <- reactive({
      input_liste_hier <- reactiveValuesToList(input)
      input_liste_hier <<- input_liste_hier
     if( is.na(input$xco_wgs84) | is.na(input$yco_wgs84)) validate("Need Coordinates") else{

     tibble(xco_wgs84 = input$xco_wgs84, yco_wgs84 = input$yco_wgs84) %>%
       dplyr::mutate(xco_wgs84_copy = xco_wgs84, yco_wgs84_copy = yco_wgs84) %>%
       st_as_sf(coords = c("xco_wgs84_copy", "yco_wgs84_copy"), crs = st_crs(zsp_dat)) %>%
       st_join(zsp_dat)
     }
   })

   return(polygon_data)

  })
}

