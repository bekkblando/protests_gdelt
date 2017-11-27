library(shiny)
library(leaflet)
library(plyr)
library(countrycode)

shinyUI(fluidPage(
  # Year Picker
  fluidRow(
    column(12, selectInput("year", "Year", choices = c(" ", seq(2014,2017)))),
    column(12, selectInput("month", "Month", choices = c(" ", seq(1,12)))),
    column(12, selectInput("country", "Country Code", choices = countrycode_data$country.name.en)),
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
  ),
  fluidRow(    
    column(10, plotOutput('mentions_to_avgtone')),
    column(10, plotOutput('goldstein_to_mentions')),
    column(10, plotOutput('code_tone')),
    column(10, plotOutput('sunflowerplots1')),
    column(10, plotOutput('sunflowerplots2')),
    column(10, plotOutput('mentions_and_avgtone')),
    column(10, plotOutput('eventcode_count')),
    column(10, plotOutput('avgtone_quadclass')),
    column(10, plotOutput('avgtone_time'))
  )
))
