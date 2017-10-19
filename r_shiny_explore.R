library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
      options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(root_events$Actor1Geo_Long, root_events$Actor1Geo_Lat, popup = root_events$Actor1Name)
  })
}

shinyApp(ui, server)
