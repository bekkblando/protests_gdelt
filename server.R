library(shiny)
library(leaflet)
library(plyr)
library("bigrquery")
library(countrycode)
library("ggplot2")
library(DT)
library(scales)
library(dplyr)
library(timevis)
source('helper_functions.R')
source('db_help.R')

# High Leverage - Low Cost
  # Working on Heroku - Bekk - Sunday - 30 min - Done
  # CSS Styling - Bekk - Sunday - 2 Hours - Done
  # Graphs Per Sequence - Sadie 
    # Events in Order
  # Holistic Statistics - Sadie/Bekk - Done
    # Different types of protests increasing over time - Saide/Bekk - Done
    # AvgTone and Goldstein Scale Per year - Sadie/Bekk - Done
  # Explore Protests By Month - Done
  
  # Render Average Stats - Bekk - Done
  # Date object render instead of fraction date - Bekk - Done
  # Flagging Functionality - Bekk
  # Loading Icon - Bekk
  # Less Strict Query - Bekk

# Need to be done
  # Fix crashing - Bekk - Done
  # More Specific Sequences - Tyler and Bjerken - Complete

project <- "DataScienceProtest" 


# set_service_token("DataScienceProtest-2dc6d98778fa.json") #change this
set_service_token(Sys.getenv("BIGQUERYCRED"))

# set_service_token(Sys.getenv("BIGQUERYCRED"))
#Sadie project ID pvp1-182616


shinyServer(function(input, output, session) {
  
  stayAlive <- reactiveTimer(intervalMs = 21000)
  observe({
    print("Staying Alive Ah Ah Ah Ah Staying Alive")
    stayAlive()
  })
  
  # Render Documentation
  render_documentation(output)
  
  observeEvent(input$explore_date_submit, {
    # Render Loading Icon
    click <- input$explore_date_submit
    
    # Protest Query
    protest <- get_violent_protest_ex(input$year, input$month, input$country_ex)

    output$map_ex <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        
        # clearGroup("sequence")
        addAwesomeMarkers(protest$ActionGeo_Long, protest$ActionGeo_Lat, layerId=protest$GLOBALEVENTID,
                          icon=ricons, popup = protest$Actor1Name, group = "root_events")
    })
  })


  # Render Year Picker
    # When year is selected, do google query for root events within the year
  # Render this map once the google query has been selected
  observeEvent(input$date_submit, {
    # Render Loading Icon
    click <- input$date
    
    date = as.Date(input$date)
    year = format(date, "%Y")
    month = format(date, "%m")
    day = format(date, "%d")

    # Protest Query

    if(input$violence == "Violent Protests"){
      protest <- get_violent_protest(year, month, day, input$country)
    }else{
      protest <- get_non_violent_protest(year, month, day, input$country)
    }
    
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%

        # clearGroup("sequence")
        addAwesomeMarkers(protest$ActionGeo_Long, protest$ActionGeo_Lat, layerId=protest$GLOBALEVENTID,
                          icon=ricons, popup = paste0(protest$GLOBALEVENTID), group = "root_events")
    })
  })
  
  observeEvent(input$map_ex_marker_click, {
    click <- input$map_ex_marker_click
    # print("in the christmas spirit")
    # Loading
    # output$loading_ex <- renderImage({list(src =  "static/loading.gif")}, deleteFile = FALSE)
    
    root_protest = get_protest(click$id)
    output$selected_table_ex <- renderTable(data.frame(root_protest))
    non_root_seq_mentions = get_mentions(click$id)
    output$mentions_ex <- renderTable(data.frame(non_root_seq_mentions))
    # removeUI("loading_ex")
  })

  # Show popup on click - TODO Clean Up Needed
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    # Loading Icon
    # output$loading <- renderImage({list(src =  "static/loading.gif")}, deleteFile = FALSE)
    
    root_protest = get_protest(click$id)
    
    if(!is.na(db_init())){
      if(input$flag){
        set_flagged(click$id)
      }
      
      output$flagged <- renderTable(get_flagged(click$id))
      db_dis()
    }
    
    # Date in human readable format
    root_protest$Date = apply(root_protest, 1, function(row) { fraction_to_date_view(as.double(row["FractionDate"])) })
    root_protest = root_protest[,c(ncol(root_protest), 1:(ncol(root_protest) - 1))]
    
    output$selected_table <- renderTable(data.frame(root_protest))
    non_root_seq_mentions = get_mentions(click$id)
    output$mentions <- renderTable(data.frame(non_root_seq_mentions))
    
    # Just Exploring if analyze is false - Check the tab not the analyze button
    if(!input$analyze){
      return("Exploring the data")
    }

    # Show non-root events based on the violent protest
    raw_data = get_sequence(root_protest)
    non_root_seq = data.frame(raw_data[1])
    summary_stats = data.frame(raw_data[2])


    if(identical(non_root_seq$ActionGeo_Long, numeric(0)) || length(sequence) == 0){
      print(paste0(sequence))
      print(paste0("No Coords Yo"))
      return("No Coordinates")
    }

    proxy <- leafletProxy("map")
    
    # Hide all the root events
    clearGroup(proxy, "root_events")
    clearGroup(proxy, "sequence")

    # Show the sequence
    proxy %>% addAwesomeMarkers(non_root_seq$ActionGeo_Long, non_root_seq$ActionGeo_Lat, layerId=non_root_seq$GLOBALEVENTID,
                                icon=nicons, popup = non_root_seq$Actor1Name, group = "sequence")
    # Mark the root events as a different color
    proxy %>% addAwesomeMarkers(root_protest$ActionGeo_Long, root_protest$ActionGeo_Lat, layerId=root_protest$GLOBALEVENTID,
                                icon=ricons, popup = root_protest$Actor1Name, group = "root_events")
  
    # Render Summary Statistics
    output$summary_stats <- renderTable(data.frame(summary_stats))
    # Render more notes if a violent protest exist within this
    non_root_seq$Date = apply(non_root_seq, 1, function(row) { fraction_to_date_view(as.double(row["FractionDate"])) })
    non_root_seq = non_root_seq[,c(ncol(non_root_seq), 1:(ncol(non_root_seq) - 1))]
    output$seq_table <- renderTable(data.frame(non_root_seq))
    # Render Sadie's Graphs
    output$mentions_to_avgtone <- mentions_to_avgtone(non_root_seq)
    output$code_tone <- code_tone(non_root_seq)
    output$eventcode_count <- eventcode_count(non_root_seq)
    output$event_time <- event_time(non_root_seq)
    output$events_to_timeline <- renderTimevis({ timevis(events_to_timeline(non_root_seq)) })
    
    # removeUI("loading")
  })
})
