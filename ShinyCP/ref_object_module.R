#' ref_object_module_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
ref_object_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("Bewertungsnummer"), "Bewertungsnummer"),
  )
}

#' ref_object_module_server Server Functions
#'
#' @noRd
ref_object_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      input_ref_object_module <- reactiveValuesToList(input)
      input_ref_object_module <<- input_ref_object_module
    })

    getReferenceObject <- reactive({
      Bewertungsnummer_sel <- input$Bewertungsnummer

      Bew <- tbl(con, "Bewertungen") %>% dplyr::filter(Bewertungsnummer == Bewertungsnummer_sel) %>%
        dplyr::select(BewertungId = Id, BewertungsobjektId, Bewertungsnummer, Marktwert, Aenderungsdatum, Aenderungsbenutzer) %>%
        head(1)

      shiny::validate(need(Bew %>% dplyr::collect() %>% nrow() > 0, "Bewertungsnummer konnte nicht gefunden werden!"))

      Obj <- tbl(con, "BW_Bewertungsobjekte") %>%
        dplyr::semi_join(Bew, by = "BewertungId") %>%
        dplyr::select(BewertungId, Objektart, Grundflaeche, NutzflaecheBerechnet, SummeNutzflaecheGewichtet,
                      Plz, Ort, Strasse, Hausnummer, GeocodeWgs84X, GeocodeWgs84Y)

      df_reference_object <- Bew %>% dplyr::left_join(Obj, by = "BewertungId") %>% dplyr::collect() %>%
        add_Objektart() %>%
        dplyr::mutate(xco_wgs84 = GeocodeWgs84X, yco_wgs84 = GeocodeWgs84Y,
                      xco_copy = GeocodeWgs84X,  yco_copy  = GeocodeWgs84Y) %>%
        st_as_sf(coords = c("xco_copy", "yco_copy"), crs = st_crs(zsp_dat))

      df_reference_object <<- df_reference_object
      df_reference_object

    })


    return(getReferenceObject)
  })
}
