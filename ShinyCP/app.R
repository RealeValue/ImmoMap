rm(list = ls())

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(DT)
library(sf)
library(rev.fun)

load("S:/Rohdaten/Aufbereitung/Lagedaten/shapes/zsp_2017.Rdata")

choices_objektart <- c("Eigentumswohnung" = 10, "Einfamilienhaus" = 20, "Zweifamilienhaus" = 21,
                       "Reihenhaus" = 30, "Doppelhaus" = 31, "Kleingartenhaus" = 40, "Grundstück" = 50)

Euro <- "\u20AC"

konfig <- config::get()
con <- db_connect()

df_immonet <- readRDS("S:/Rohdaten/Aufbereitung/ImmoNet/03_immoNet_cols_as_liebe.rds")

source("id_module.R")
source("ref_object_module.R")
source("action_module.R")
source("data_module.R")
source("map_data.R")
# source("verlauf.R")
source("Functions.R")

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
      map_data_ui("result_data")
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
}

shinyApp(ui = ui, server = server)
