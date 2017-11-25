library(shiny)
library(leaflet)
library(plyr)
library("bigrquery")
library(countrycode)

# Working on Heroku
# CSS Styling
# Download button for a report on what's on the page


project <- "datascienceprotest"

set_service_token("DataScienceProtest-2dc6d98778fa.json")

get_violent_protest <- function(year, month, day, country){
  fraction_date = signif(strtoi(year) + (strtoi(month) * 30 + strtoi(day))/365, digits=8)
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode='141' and FractionDate=", fraction_date, " and ActionGeo_CountryCode='", paste0(countrycode(country, "country.name.en" ,"fips105")), "'")
  return(query_exec(sql, project = project))
}

get_non_violent_protest <- function(year, month){
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode='140' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")))
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_protest <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE GLOBALEVENTID=", global_id)
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_mentions <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID, MentionSourceName, MentionIdentifier FROM [gdelt-bq:gdeltv2.eventmentions] WHERE GLOBALEVENTID=", global_id)
  print(paste0(sql))
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_sequence <- function(violent_protest){
  violent_actor1 = violent_protest$Actor1Name
  violent_actor2 = violent_protest$Actor2Name
  fractional_date = violent_protest$FractionDate

  lower_date = fractional_date - 45/365
  higher_date = fractional_date + 45/365

  sql <-paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:full.events] WHERE EventRootCode in ('10','11','12','13', '14') and FractionDate <=", higher_date ," and FractionDate >=", lower_date)
  if(!is.na(violent_actor1)){
    sql <- paste0(sql, " and Actor1Name='", violent_actor1, "'")
  }
  if(!is.na(violent_actor2)){
    sql <- paste0(sql, " and Actor2Name='", violent_actor2, "'")
  }
  print(sql)

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

    # Protest Query

    if(input$violence == "Violent Protests"){
      protest <- get_violent_protest(input$year, input$month, input$day, input$country)
    }else{
      protest <- get_non_violent_protest(input$year, input$month, input$day, input$country)
    }
    print("test")
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%

        # clearGroup("sequence")
        addAwesomeMarkers(protest$ActionGeo_Long, protest$ActionGeo_Lat, layerId=protest$GLOBALEVENTID,
                          icon=ricons, popup = protest$Actor1Name, group = "root_events")
    })
  })

  # Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click

    # Show non-root events based on the violent protest
    root_protest = get_protest(click$id)
    non_root_seq = get_sequence(root_protest)
    non_root_seq_mentions = get_mentions(click$id)


    if(identical(non_root_seq$ActionGeo_Long, numeric(0)) || length(sequence) == 0){
      print(paste0(sequence))
      print(paste0("No Coords Yo"))
      return("No Coordinates")
    }

    print(paste0(typeof(non_root_seq)))

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

    # Render more notes if a violent protest exist within this
    output$seq_table <- renderDataTable(data.frame(non_root_seq))
    output$selected_table <- renderDataTable(data.frame(root_protest))
    output$mentions <- renderDataTable(data.frame(non_root_seq_mentions))
  })
})
