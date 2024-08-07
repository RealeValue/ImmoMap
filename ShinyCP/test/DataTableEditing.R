library(shiny)
library(DT)

dt_output = function(title, id) {
  fluidRow(column(
    12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
    hr(), DTOutput(id)
  ))
}
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

shinyApp(
  ui = fluidPage(
    title = 'Double-click to edit table cells',

    dt_output('server-side processing (editable = "cell")', 'x5'),
  ),

  server = function(input, output, session) {


    observe({
      input_all <- reactiveValuesToList(input)
      input_all <<- input_all
    })

    d1 = iris
    d1$Date = Sys.time() + seq_len(nrow(d1))

    d5 = d1

    options(DT.options = list(pageLength = 5))

    # server-side processing
    output$x5 = render_dt(d5, 'cell')


    # edit a single cell
    proxy5 = dataTableProxy('x5')
    observeEvent(input$x5_cell_edit, {
      info = input$x5_cell_edit
      str(info)  # check what info looks like (a data frame of 3 columns)

      d_vorher <<- d5

      d_nachher <<- editData(d_vorher, info) %>%
        dplyr::mutate(Petal.Width = Sepal.Length + Sepal.Width + Petal.Length)

      replaceData(proxy5, d_nachher, resetPaging = FALSE)  # important
    })


  }
)
