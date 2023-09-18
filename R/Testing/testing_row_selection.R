library(shiny)
library(DT)

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

dat0 <- data.frame(
  fruit  = c("apple", "cherry", "pineapple", "pear"),
  letter = c("a", "b", "c", "d")
)

dat1 <- cbind(dat0, bool = FALSE)

dat2 <- cbind(
  dat0,
  check = shinyInput(checkboxInput, nrow(dat0), "checkb")
)

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


tableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      6,
      DTOutput(ns("dtable"))
    ),
    column(
      6,
      verbatimTextOutput(ns("reactiveDF"))
    )
  )
}

tableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    Dat <- reactiveVal(dat1)

    output[["dtable"]] <- renderDT(
      {
        datatable(
          dat2,
          rownames = TRUE,
          escape = FALSE,
          editable = list(target = "cell", disable = list(columns = 3)),
          selection = "none",
          callback = JS(js("dtable", session$ns))
        )
      },
      server = FALSE
    )

    observeEvent(input[["dtable_cell_edit"]], {
      info <- input[["dtable_cell_edit"]]
      info_all <<- info
      Dat_all <<- Dat()
      #Dat(editData(Dat(), info))
      xx <- editData(Dat(), info)
      Dat <- xx
      xx_all <<- xx
      xx
    })

    output[["reactiveDF"]] <- renderPrint({
      Dat()
    })
  })
}


ui <- fluidPage(
  br(),
  tableUI("xxx")
)

server <- function(input, output, session) {
  tableServer("xxx")
}

shinyApp(ui, server)
