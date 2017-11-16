<<<<<<< Updated upstream
#remove objects from workspace
rm(list=ls())

#load libraries
library("bigrquery")
library(DBI)
library(RSQLite)
library(shiny)
library(leaflet)

#PROJECT ID
pvp1 <- "pvp1-183719"

#select predictors for protest events in between Jan-Feb 2016
#Note: event root code 14 = CAMEO Protest
sql_14_2016 <- "SELECT Actor1Name, Actor2Name, EventCode, AvgTone, 
      GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, 
      Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, 
      FractionDate FROM [gdelt-bq:full.events] WHERE EventRootCode='14' 
      and MonthYear > 201601 and MonthYear < 201604"


#execute bigQuery query and store result in protests list
#query_exec(query string, project name, use_legacy_sql, max_pages)
protests <- query_exec(sql_14_2016, project = pvp1, useLegacySql = FALSE, 
                       max_pages = Inf)


#construct file path to store
db_name <- file.path('/Users/walkn/Desktop/CPSC_4300-001/PVP/QueryData',
                     'events.sqlite')
db_conn <- dbConnect(SQLite(), db_name)

#getQuery (calls dbSendQuery/dbFetch/dbClearand create Events table
if(!dbExistsTable(db_conn, 'Events')) {
dbGetQuery(conn = db_conn, 
            'CREATE TABLE Events(Actor1Name STRING, Actor2Name STRING, 
            EventCode STRING, AvgTone FLOAT, GoldsteinScale FLOAT, 
            IsRootEvent INTEGER, QuadClass INTEGER, NumMentions INTEGER, 
            NumSources INTEGER, Actor1Geo_Lat FLOAT, Actor1Geo_Long FLOAT, 
            Actor2Geo_Lat FLOAT, Actor2Geo_Long FLOAT, FractionDate FLOAT)')

}

#copy data frames to database tables
dbWriteTable(db_conn, 'Events', protests, append = TRUE)

#read in table to workspace
events <- dbReadTable(db_conn, 'Events')

dbDisconnect(db_conn)

#define objects for visualization
event_tone = data.frame(EventCode = EventCode, AvgTone = AvgTone)
events$EventCode <- as.factor(events$EventCode)
sampled_data = events[sample(nrow(events), 500),]
root_event = data.frame(sampled_data[which(sampled_data$IsRootEvent == TRUE),])
non_root_event = data.frame(sampled_data[which(sampled_data$IsRootEvent == FALSE),])

root_event$sequence <- NA

create_sequence <- function(row) { 
  event_query = non_root_event[which(non_root_event$Actor1Name == 
                row["Actor1Name"], non_root_event$Actor2Name == 
                row["Actor2Name"]),]
  if(!identical(event_query, character(0)) && length(event_query) != 0) {
    return(event_query)
  }
}

root_event$sequence = apply(root_event, 1, function(row) { 
  create_sequence(row) })

#____________________________________GRAPHS______________________________________

symbols(events$NumMentions, events$AvgTone, circles=events$EventCode, inches=0.35, 
        fg="white", bg="blue", xlab = "Number of Mentions", ylab = "Average Tone") 

symbols(events$GoldsteinScale, events$NumMentions, circles = events$IsRootEvent,
        inches=0.15, fg="white", bg="red", xlab = "Goldstein Scale", 
        ylab = "Number of Mentions")

this_base <- "events"

my_data <- data.frame(
  event_code = factor(events$EventCode), mentions = data(events$NumMentions)
)
p <- ggplot2::ggplot(my_data, aes(x = event_code, y = mentions)) +
  geom_bar(stat = "identity", fill = "orange", colours(distinct = TRUE), 
  width = .02) + scale_y_continuous(breaks = sequence(events$NumMentions)) +
  labs(x = "Event Code", y = "Number of Mentions") +
  ggtitle("Number of Mentions per Event Code") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), face = "bold", 
                                  vjust = 1.5), axis.title = element_text(face = "bold"))
p



#________________________RShiny Visualization__________________________________
jitter(root_event$ActionGeo_Lat, factor = 0.0001)

ui <- fluidPage(
  fluidRow(column(12, leafletOutput("map"))),
  fluidRow(column(12, dataTableOutput('table')))
  )

ricons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)
bicons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

event_map <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
              options = providerTileOptions(noWrap = TRUE)) %>%
    
    addAwesomeMarkers(root_event$Actor1Geo_Long, 
              root_event$Actor1Geo_Lat, layerId=root_event$GLOBALEVENTID, 
              icon = ricons, popup = root_event$Actor1Name, group = "root")
  })
  
  #Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
  # Show non-root events based on the root event
  sequence = root_events[which(root_event$GLOBALEVENTID == click$id),]$sequence
  non_root_event = non_root_event[which(non_root_event$GLOBALEVENTID %in% sequence),]
  
  if(identical(non_root_event$ActionGeo_Long, numeric(0)) || length(sequence) == 0) {
    print(paste0(sequence))
    print(paste0("No Coordinates"))
    return("No Coordinates")
  }
  
  proxy <- leafletProxy("map")
  
  #hide all root events
  clearGroup(proxy, "root_events")
  clearGroup(proxy, "sequence")
  
  #show sequence
  proxy %>% addAwesomeMarkers(non_root_event$ActionGeo_Long, non_root_event$ActionGeo_Lat,
            layerId = non_root_event$GLOBALEVENTID, icons = bicons, 
            popup = non_root_event$Actor1Name, group = "sequence")
  
  output$table <- renderDataTable(data.frame(non_root_seq))
  })
}

shinyApp(ui, event_map)
=======
#remove objects from workspace
rm(list=ls())

#load libraries
library("bigrquery")
library(DBI)
library(RSQLite)
library(shiny)
library(leaflet)

#PROJECT ID
pvp1 <- "datascienceprotest"

#select predictors for protest events in between Jan-Mar 2016
#Note: event root code 14 = CAMEO Protest
sql_14_2016 <- "SELECT Actor1Name, Actor2Name, EventCode, AvgTone, 
      GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, 
      Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, 
      FractionDate FROM [gdelt-bq:full.events] WHERE EventRootCode='14' 
      and MonthYear > 201601 and MonthYear < 201604"


#execute bigQuery query and store result in protests list
#query_exec(query string, project name, use_legacy_sql, max_pages)
protests <- query_exec(sql_14_2016, project = pvp1, useLegacySql = FALSE, 
                       max_pages = Inf)


#construct file path to store
db_name <- file.path('/Users/BekkBlando/Documents/github/clemson/data_science/protests/data/', 'events.sqlite')
db_conn <- dbConnect(SQLite(), db_name)

#getQuery (calls dbSendQuery/dbFetch/dbClearand create Events table
if(!dbExistsTable(db_conn, 'Events')) {
dbGetQuery(conn = db_conn, 
            'CREATE TABLE Events(Actor1Name STRING, Actor2Name STRING, 
            EventCode STRING, AvgTone FLOAT, GoldsteinScale FLOAT, 
            IsRootEvent INTEGER, QuadClass INTEGER, NumMentions INTEGER, 
            NumSources INTEGER, Actor1Geo_Lat FLOAT, Actor1Geo_Long FLOAT, 
            Actor2Geo_Lat FLOAT, Actor2Geo_Long FLOAT, FractionDate FLOAT)')

}

#lists names of tables accessed by db_conn
#dbListTables(db_conn)

#lists field names of remote table accessed by db_conn
#dbListFields(db_conn, 'Events')

#copy data frames to database tables
#dbWriteTable(connection, table name, data)
dbWriteTable(db_conn, 'Events', protests, append = TRUE)

#read in table to workspace
events <- dbReadTable(db_conn, 'Events')

#define objects for visualization
event_tone = data.frame(EventCode = EventCode, AvgTone = AvgTone)
events$EventCode <- as.factor(events$EventCode)
sampled_data = events[sample(nrow(events), 500),]
root_event = data.frame(sampled_data[which(sampled_data$IsRootEvent == TRUE),])
non_root_event = data.frame(sampled_data[which(sampled_data$IsRootEvent == FALSE),])

root_event$sequence <- NA

create_sequence <- function(row) { 
  event_query = non_root_event[which(non_root_event$Actor1Name == 
                row["Actor1Name"], non_root_event$Actor2Name == 
                row["Actor2Name"]),]
  if(!identical(event_query, character(0)) && length(event_query) != 0) {
    return(event_query)
  }
}

root_event$sequence = apply(root_event, 1, function(row) { 
  create_sequence(row) })

#________________________RShiny Visualization__________________________________

ui <- fluidPage(
  leafletOutput("map")
)

ricons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "red"
)
bicons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

event_map <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
              options = providerTileOptions(noWrap = FALSE)) %>%
    
    addAwesomeMarkers(root_event$Actor1Geo_Long, 
              root_event$Actor1Geo_Lat, layerId=root_event$GLOBALEVENTID, 
              icon = ricons, popup = root_event$Actor1Name, group = "root")
  })
  
  #Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
  # Show non-root events based on the root event
  print(paste0(data.frame(root_event[which(root_event$GLOBALEVENTID == 
    click$id),][17])$Actor1Geo_Long))
    
    text<-paste("Lattitude ", click$id, "Longtitude ", click$lng)
    proxy <- leafletProxy("map")
    proxy %>% clearPopups() %>%
    addPopups(click$lng, click$lat, text)
  })
}

shinyApp(ui, event_map)
>>>>>>> Stashed changes
