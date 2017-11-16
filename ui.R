library(shiny)
library(leaflet)
library(plyr)

shinyUI(fluidPage(
  # Year Picker
  fluidRow(
    column(12, selectInput("year", "Year", choices = c(" ", seq(2000,2017)))),
    column(12, selectInput("month", "Month", choices = c(" ", seq(1,12)))),
    column(12, actionButton("date_submit", "Submit"))
  ),
  fluidRow(
    column(12, leafletOutput("map"))
  ),
  fluidRow(
    column(12, dataTableOutput('table'))
  )
))
