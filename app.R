library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(shiny)
library(tidyr)
library(rvest)
library(stringr)
library(foreach)
library(doSNOW)
library(openxlsx)

source('R/helper_functions.R')
source('R/parellel.R')
source("R/auto_ui.R")
source("R/auto_server.R")

ui <- fluidPage(
  title = 'tenniscrape',
  fluidRow(
    style = 'background-color:ghostwhite;color:grey;
                         margin-bottom:5px;
                         box-shadow:20px 20px 50px 10px white inset;',
    
    column(1),
    column(10, tags$h1('tenniscrape')),
    column(1)
  ),
  fluidRow(
    style = 'background-color:ghostwhite;
                         box-shadow:20px 20px 50px 10px #e6ecf0 inset;',
    column(1),
    column(10, auto_ui('auto')),
    column(1)
  )
)
server <- function(input, output) {
  autoServer('auto')
}
shinyApp(ui = ui, server = server)
