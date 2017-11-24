library(shiny)
library(leaflet)
library(plyr)

shinyUI(fluidPage(
  # Year Picker
  fluidRow(
    column(12, selectInput("year", "Year", choices = c(" ", seq(2014,2017)))),
    column(12, selectInput("month", "Month", choices = c(" ", seq(1,12)))),
    column(12, selectInput("violence", "Violent Or Non Violent", choices = c("Violent Protests", "Non-Violent Protests"))),
    column(12, actionButton("date_submit", "Submit"))
  ),
  fluidRow(
    column(12, leafletOutput("map"))
  ),
  fluidRow(
    column(10, dataTableOutput('selected_table'))
  ),
  fluidRow(
    column(10, dataTableOutput('seq_table'))
  ),
  fluidRow(    
    column(10, dataTableOutput('mentions'))
  )
))
