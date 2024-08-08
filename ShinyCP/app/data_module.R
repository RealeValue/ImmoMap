#' data_module_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
data_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("Objektart"), "Objektart", choices = choices_objektart, selected = choices_objektart, multiple = T),
    sliderInput(ns("Kaufpreis"), "Kaufpreis", min = 0, max = 3000000, value = c(0,3000000), step = 1000),
    sliderInput(ns("KaufpreisQM"), "QM - Kaufpreis ", min = 0, max = 15000, value = c(0,15000), step = 10),
    dateRangeInput(ns("Kaufdatum"), label = "Kaufdatum", start = as.Date("2019-01-01"), end = lubridate::now(), min = as.Date("2019-01-01"), max = lubridate::now()),

    sliderInput(ns("Grundflaeche"), "Grundstuecksgroesse", min = 0, max = 3500, value = c(0,3500), step = 10),
    sliderInput(ns("Nutzflaeche"), "Wohnflaeche", min = 0, max = 1000, value = c(0,1000), step = 1),
  )
}

#' data_module_server Server Functions
#'
#' @noRd
data_module_server <- function(id, action, ref_object){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      df_reference_object <- ref_object()

      KaufpreisUpdate <- c(df_reference_object$Marktwert * 0.85, df_reference_object$Marktwert * 1.15) %>% round(-4)
      end_date   <- df_reference_object$Aenderungsdatum %>% as.Date()
      start_date <- end_date %m-% years(2)

      updateSelectInput(session, inputId = "Objektart", selected = df_reference_object$Objektart_num)
      updateSliderInput(session, inputId = "Kaufpreis", value = KaufpreisUpdate)
      updateDateRangeInput(session, inputId = "Kaufdatum", start = start_date, end = end_date)
    })


    data <- eventReactive( action(), {
      df_reference_object <- ref_object()

      search_polygon_sf <-  df_reference_object %>% search_vergleichspreise_polygon(buffer_radius = 30000) ## input$search_radius * 1000)
      df_vergleichswerte <- find_ComparableObjectsWithinPolygon(search_polygon_sf, df_reference_object)

      df_vergleichswerte <<- df_vergleichswerte
      df_vergleichswerte
    })

    return(data)

  }
)}
