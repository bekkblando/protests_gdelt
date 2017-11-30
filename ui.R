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
      ),
      tags$script(src="materialize.min.js"),
      theme= "materialize.min.css"
    ),
    tags$div(class="nav-wrapper",
      navbarPage("GDELT Protests",
        id = "nav",
           
        tabPanel("Intro",
          value = "intro"
        ),
        
        tabPanel("Documentation",
          value = "Documentation"
        ),
        
        tabPanel("Holistic Statistics",
          value = "holistic_statistics"
        ),
        
        tabPanel("Protest Explorer",
          value = "protest_explorer",
          # Year Picker
          fluidRow(
            column(12, selectInput("year", "Year", choices = c(" ", seq(2014,2017)))),
            column(12, selectInput("month", "Month", choices = c(" ", seq(1,12)))),
            column(12, selectInput("country_ex", "Country Code", choices = countrycode_data$country.name.en)),
            column(12, actionButton("explore_date_submit", "Explore Protests"))
          ),
          fluidRow(
            column(12, leafletOutput("map_ex"))
          ),
          fluidRow(
            column(10, dataTableOutput('selected_table_ex'))
          ),
          fluidRow(
            column(10, dataTableOutput('seq_table_ex'))
          ),
          fluidRow(    
            column(10, dataTableOutput('mentions_ex'))
          )
        ),
        
        tabPanel("Sequence Explorer", 
          value = "sequence_explorer",
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
            tags$style("
             div#selected_table{
                       overflow-x: scroll;
                       width:100%;
                       };
            "),
              column(12, tableOutput('selected_table'))
          ),
          fluidRow(
            tags$style("
             div#seq_table{
              overflow-x: scroll;
              width:100%;
             };
            "),
              column(12, tableOutput('seq_table'))
          ),
          fluidRow(  
            tags$style("
             div#mentions{
              overflow-x: scroll;
              width:100%;
             };
            "),
              column(12, tableOutput('mentions'))
          ),
          fluidRow(
            column(10, plotOutput('mentions_to_avgtone')),
            column(10, plotOutput('goldstein_to_mentions')),
            column(10, plotOutput('code_tone')),
            column(10, plotOutput('sunflowerplots1')),
            column(10, plotOutput('sunflowerplots2')),
            column(10, plotOutput('mentions_and_avgtone')),
            column(10, plotOutput('eventcode_count')),
            column(10, plotOutput('avgtone_quadclass'))
          )
        )
      )
    )
  )
)