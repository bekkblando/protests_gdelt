library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("map")
)

ricons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)
nicons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
      options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(root_events$Actor1Geo_Long, root_events$Actor1Geo_Lat, layerId=root_events$GLOBALEVENTID, 
                        icon=ricons, popup = root_events$Actor1Name, group = "root")
      # addAwesomeMarkers(root_events$Actor2Geo_Long, root_events$Actor2Geo_Lat, icon=ricons, popup = root_events$Actor2Name)
  })
  
  #Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    # Show non-root events based on the root event
    print(paste0(data.frame(root_events[which(root_events$GLOBALEVENTID == click$id),][17])$Actor1Geo_Long))
    
    text<-paste("Lattitude ", click$id, "Longtitude ", click$lng)
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
}

shinyApp(ui, server)
