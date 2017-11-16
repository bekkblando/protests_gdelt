library(shiny)
library(leaflet)
library(plyr)
library("bigrquery")

project <- "datascienceprotest"

get_violent_protest <- function(year, month){
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:full.events] WHERE EventCode='141' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")))
  return(query_exec(sql, project = project, max_pages = Inf))
}

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


shinyServer(function(input, output, session) {

  # Render Year Picker
    # When year is selected, do google query for root events within the year
  # Render this map once the google query has been selected
  observeEvent(input$date_submit, {
    # Render Loading Icon
    click <- input$date_submit

    # Violent Protest Query
    violent_protest <- get_violent_protest(input$year, input$month)
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%

        addAwesomeMarkers(violent_protest$ActionGeo_Long, violent_protest$ActionGeo_Lat, layerId=violent_protest$GLOBALEVENTID,
                          icon=ricons, popup = violent_protest$Actor1Name, group = "root_events")
    })
  })

  # Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click

    # Show non-root events based on the violent protest
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
    # Mark the root events as a different color
    proxy %>% addAwesomeMarkers(non_root_seq$ActionGeo_Long, non_root_seq$ActionGeo_Lat, layerId=non_root_seq$GLOBALEVENTID,
                                icon=ricons, popup = non_root_seq$Actor1Name, group = "sequence")
    proxy %>% addAwesomeMarkers(non_root_seq$ActionGeo_Long, non_root_seq$ActionGeo_Lat, layerId=non_root_seq$GLOBALEVENTID,
                                icon=nicons, popup = non_root_seq$Actor1Name, group = "sequence")
    # Render more notes if a violent protest exist within this
    output$table <- renderDataTable(data.frame(non_root_seq))
  })
})
