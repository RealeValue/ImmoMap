#' download_handler_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
download_handler_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadReport"), "Download Report")
  )
}

#' download_handler_server Server Functions
#'
#' @noRd
download_handler_server <- function(id, daten){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      input_download_handler <- reactiveValuesToList(input)
      input_download_handler <<- input_download_handler
    })

    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("Report.html")
      },
      content = function(file) {
        # Define the report name
        report_file <- paste0("Report.html")

        ## weil Daten nicht im R Format (zB Datum oder NA's) versuche diesen Zugang:
        daten          <- daten()
        save_data_dir  <- "C:/tmp" ## tempdir()
        save_data_file <- paste0(save_data_dir, "/data_report.rds")
        print(save_data_file)
        daten %>% saveRDS(save_data_file)

        params <- list(save_data_file = save_data_file)
        params_all <<- params

        # Render the Quarto document
        quarto::quarto_render(
          input = "report.qmd",
          execute_params = params,
          output_file = report_file
        )

        # Check if the file was successfully created and copy it to the download path
        if (file.exists(report_file)) {
          file.copy(report_file, file, overwrite = TRUE)
          print(paste("Report generated and copied to:", file)) # Debugging line

          # Remove the temporary file
          unlink(report_file)
          print(paste("Temporary file deleted:", report_file)) # Debugging line
        } else {
          showModal(modalDialog(
            title = "Error",
            "The report generation failed. Please try again."
          ))
        }
      }
    )


  }
  )
}
