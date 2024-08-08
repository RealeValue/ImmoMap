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
    numericInput(ns("num"), "Number of observations:", 100),
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

        report_file <<- report_file
        daten       <- daten()


        # Render the Quarto document
        quarto::quarto_render(
          input = "report.qmd",
          execute_params = list(num = input$num, daten = daten),
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
