server_adresse <- "http://ds.realevalue.at:5164/"

choices_objektart <- c("Eigentumswohnung" = 10, "Einfamilienhaus" = 20, "Zweifamilienhaus" = 21, "Reihenhaus" = 30,
                       "Doppelhaus" = 31, "Kleingartenhaus" = 40, "Grundstück" = 50)




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
    dateRangeInput(ns("Kaufdatum"), label = "Kaufdatum", start = as.Date("2019-01-01"), end = lubridate::now(), min = as.Date("2019-01-01"), max = lubridate::now()),

    sliderInput(ns("Grundflaeche"), "Grundstuecksgroesse", min = 0, max = 3500, value = c(0,3500), step = 10),
    sliderInput(ns("Nutzflaeche"), "Wohnflaeche", min = 0, max = 1000, value = c(0,1000), step = 1),
    actionButton(ns("start"), "Start!"),
  )
}

#' data_module_server Server Functions
#'
#' @noRd
data_module_server <- function(id, polygon_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data <- eventReactive( input$start, {
      df_coordinates <- polygon_data()

      input_map <- reactiveValuesToList(input)
      input_map <<- input_map
      input_map$Bereich <- c(df_coordinates$xco_wgs84 - 0.05, df_coordinates$xco_wgs84 + 0.05, df_coordinates$yco_wgs84 - 0.05, df_coordinates$yco_wgs84 + 0.05)

      immonet_datenbankabfrage <- input_map %>% toJSON()
      res_datenbankabfrage_immonet <- httr::POST(paste0(server_adresse, "search_immonet_objects/immonet_objects"),
                                                 body = list(immonet_datenbankabfrage), encode = "json")
      df_immonet <- httr::content(res_datenbankabfrage_immonet, simplifyVector = T) %>% as_tibble()

      if(nrow(df_immonet) == 0)   shiny::validate("No data!")
      if(nrow(df_immonet) > 2500) shiny::validate("Too much data!")

      df_immonet_module_all <<- df_immonet
      df_immonet
    })

    return(data)

  }
)}
