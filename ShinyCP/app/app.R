rm(list = ls())

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(DT)
library(sf)
library(stars)
library(tmap)
## library(rev.fun)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

load("../data/zsp_2017.Rdata")

choices_objektart <- c("Eigentumswohnung" = 10, "Einfamilienhaus" = 20, "Zweifamilienhaus" = 21,
                       "Reihenhaus" = 30, "Doppelhaus" = 31, "Kleingartenhaus" = 40, "Grundstück" = 50)


Euro <- "€" ## "/u20AC"

config_liebe <- config::get("qs")

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = ifelse(.Platform$OS.type == "windows", config_liebe$driver, "ODBC Driver 17 for SQL Server"),
                      Server = config_liebe$server, Database = config_liebe$database, UID = config_liebe$uid,
                      PWD    = config_liebe$pwd,
                      Port   = config_liebe$port)


config_immonet <- config::get("immonet")
con_immonet    <- DBI::dbConnect(odbc::odbc(),
                      Driver = ifelse(.Platform$OS.type == "windows", config_immonet$driver, "ODBC Driver 17 for SQL Server"),
                      Server = config_immonet$server, Database = config_immonet$database, UID = config_immonet$uid,
                      PWD    = config_immonet$pwd,
                      Port   = config_immonet$port)

df_immonet     <- tbl(con_immonet, "immonet")

ra_score_raster        <- read_stars("../data/ra_score_raster.tif")
names(ra_score_raster) <- "ra_score_raster"

source("id_module.R",         local = T)
source("ref_object_module.R", local = T)
source("action_module.R",     local = T)
source("data_module.R",       local = T)
source("map_data.R",          local = T)
source("Functions.R",         local = T)
# source("verlauf.R")


ui <- fluidPage(
  titlePanel("ImmoMap"),
  sidebarLayout(
    sidebarPanel(
      id_module_ui("uuid"),
      ref_object_module_ui("reference_object"),
      data_module_ui("data"),
      action_module_ui("start"),
      width = 2
    ),
    mainPanel(
      ##verlauf_ui("Verlauf"),
      map_data_ui("result_data"),
      downloadButton("report", "Generate report")
    )
  )
)

server <- function(input, output, session) {
  id_module_server("uuid")
  ref_object <- ref_object_module_server("reference_object")
  action_module <- action_module_server("start")

  daten <- data_module_server("data", action = action_module, ref_object = ref_object)
  map_data_server("result_data", data = daten, ref_object = ref_object)
  ## verlauf_server("Verlauf") ## , polygon_data = polygon_data, action = action_module)

  output$report <- downloadHandler(
    filename <-  "XXX_volcanoes_report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Report.Rmd")
      file.copy("Report.Rmd", tempReport, overwrite = TRUE)

      daten <- daten()


      params <- list(n = 20, daten = daten)
      file_all <<- file
      tempReport_all <<- tempReport

      rmarkdown::render("Report.Rmd", output_file = "C:/tmp/CPNEU_volcanoes_report.pdf",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


}

shinyApp(ui = ui, server = server)
