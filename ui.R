library(shiny)
library(leaflet)
library(plyr)
library(countrycode)
library(shinyWidgets)

shinyUI(
  fluidPage(
    tags$head(
      tags$style("
        html, body {
          max-width: 100%;
          overflow-x: hidden;
        };
      "),
      tags$meta(
        name="viewport",
        content="width=device-width"
      )
    ),
    tags$div(class="nav-wrapper",
      navbarPage("GDELT Protests",
       tabPanel("Intro"),
       tabPanel("Sequence Explorer"),
       tabPanel("Holistic Statistics")
      )
    ),
    theme= "materialize.min.css",
    tags$head(tags$script(src="materialize.min.js")),
    tabPanel("Intro"),
    tabPanel("Holistic Statistics"),
    tabPanel("Sequence Explorer", 
      title = "Sequence Explorer",
      # Year Picker
      fluidRow(
        column(4, dateInput("date", "Date: ")),
        column(4, selectInput("country", "Country", choices = countrycode_data$country.name.en)),
        column(4, selectInput("violence", "Violent Or Non Violent", choices = c("Violent Protests", "Non-Violent Protests")))
      ),
      fluidRow(
        column(12, actionButton("date_submit", "Get Protests With These Parameters"))
      ),
      fluidRow(
        column(12, leafletOutput("map"))
      ),
      fluidRow(
        column(12, switchInput(inputId = "analyze", label="Analyze Protest", value = FALSE))
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
        column(10, plotOutput('code_tone')),
        column(10, plotOutput('sunflowerplots1')),
        column(10, plotOutput('sunflowerplots2')),
        column(10, plotOutput('eventcode_count'))
      )
    )
  )
)