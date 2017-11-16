library(shiny)
library(leaflet)
# install.packages("plyr")
library(plyr)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)

# Jitter the values yo
jitter(root_events$ActionGeo_Long, factor = 0.0001)
jitter(root_events$ActionGeo_Lat, factor = 0.0001)

ui <- fluidPage(
  fluidRow(
    column(12, leafletOutput("map"))
  ),
  fluidRow(
    column(12, dataTableOutput('table'))
  )
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

      addAwesomeMarkers(root_events$ActionGeo_Long, root_events$ActionGeo_Lat, layerId=root_events$GLOBALEVENTID, 
                        icon=ricons, popup = root_events$Actor1Name, group = "root_events")
    })
  
  # Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    # Show non-root events based on the root event
    sequence = root_events[which(root_events$GLOBALEVENTID == click$id),]$sequence
    # = non_root[which(non_root$GLOBALEVENTID == sequence)]
    non_root_seq = non_root[which(non_root$GLOBALEVENTID %in% sequence),]

    if(identical(non_root_seq$ActionGeo_Long, numeric(0)) || length(sequence) == 0){
      print(paste0(sequence))
      print(paste0("No Coords Yo"))
      return("No Coordinates")
    }
    
    print(paste0(typeof(non_root_seq)))
    
    
    # text<-paste("Lattitude ", click$id, "Longtitude ", click$lng)
    proxy <- leafletProxy("map")

    # Hide all the root events
    clearGroup(proxy, "root_events")
    clearGroup(proxy, "sequence")
    
    # Show the sequence
    proxy %>% addAwesomeMarkers(non_root_seq$ActionGeo_Long, non_root_seq$ActionGeo_Lat, layerId=non_root_seq$GLOBALEVENTID, 
                          icon=nicons, popup = non_root_seq$Actor1Name, group = "sequence")
    output$table <- renderDataTable(data.frame(non_root_seq))
  })
}

shinyApp(ui, server)
