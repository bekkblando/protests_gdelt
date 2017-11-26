library(shiny)
library(leaflet)
library(plyr)
library("bigrquery")
library(countrycode)
library("ggplot2")

# High Leverage - Low Cost
  # Working on Heroku - Bekk - Sunday
  # CSS Styling - Bekk - Sunday
  # Graphs Per Sequence - Sadie
  # Holistic Statistics - Sadie/Bekk
    # Different types of protests increasing over time - Saide/Bekk
    # AvgTone and Goldstein Scale Per year - Sadie/Bekk

# Need to be done
  # Fix crashing - Bekk
  # More Specific Sequences - Tyler and Bjerken

# Download button for a report on what's on the page


project <- "datascienceprotest"

set_service_token(Sys.getenv("BIGQUERYCRED"))

get_violent_protest <- function(year, month, day, country){
  fraction_date = signif(as.numeric(year) + (as.numeric(month) * 30 + as.numeric(day))/365, digits=8)
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode='145' and FractionDate=", fraction_date, " and ActionGeo_CountryCode='", paste0(countrycode(country, "country.name.en" ,"fips105")), "'")
  print(sql)
  
  query_result = tryCatch({
    return(query_exec(sql, project = project))
  }, warning = function(w) {
    # Put an error message
  }, error = function(e) {
    # Put an error message,
  }, finally = {
    # Done
  })
  return(query_result)
}

get_non_violent_protest <- function(year, month){
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, NumMentions, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode='140' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")))
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_protest <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE GLOBALEVENTID=", global_id)
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

# Render Saidie's Graphs Yo

mentions_to_avgtone <- function(events){
  return(renderPlot(symbols(events$NumMentions, events$AvgTone, circles=events$EventCode, inches=0.35, 
          fg="white", bg="blue", xlab = "Number of Mentions", ylab = "Average Tone")))
}

goldstein_to_mentions <- function(events){
  return(renderPlot(symbols(events$GoldsteinScale, events$NumMentions, circles = events$IsRootEvent,
          inches=0.15, fg="white", bg="red", xlab = "Goldstein Scale", 
          ylab = "Number of Mentions")))
}

sunflowerplots1 <- function(events){
  return(renderPlot(sunflowerplot(events$AvgTone, events$NumMentions)))
  
}

sunflowerplots2 <- function(events){
  return(renderPlot(sunflowerplot(events$NumMentions, events$NumSources)))
  
}


mentions_and_avgtone <- function(events){
  
  p <- ggplot2::ggplot(events, aes(x = EventCode, y = NumMentions)) +
    geom_bar(stat = "identity", fill = "orange", 
             width = .02) + scale_y_continuous(breaks = sequence(sum(events$NumMentions))) +
    labs(x = "Event Code", y = "Number of Mentions") +
    ggtitle("Number of Mentions per Event Code") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey50"),
          plot.title = element_text(size = rel(1.5), face = "bold", 
                                    vjust = 1.5), axis.title = element_text(face = "bold"))
  return(renderPlot(p))

}

eventcode_count <- function(events){

  pie_data <- data.frame(
    variable = events$EventCode,
    value = events$IsRootEvent)
  
    p <- ggplot2::ggplot(pie_data, aes(x = variable, y= value, 
                                       fill=variable)) + 
    geom_bar(width = 1, colour ="black", stat="identity") +
    ggtitle("Is a Root Event Based on Event Code") +
    theme_bw() + theme(panel.grid.major = element_blank(), 
                       panel.border = element_blank(),
                       plot.title = element_text(size = rel(1.5), face = "bold"),
                       axis.title = element_blank(), 
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       legend.text = element_text(events$EventCode))
   return(renderPlot(p))
}


# End Rendering Saidie's Graphs my hellsink






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

    print(paste0(non_root_seq))

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
    # Render Sadie's Graphs
    output$mentions_to_avgtone <- mentions_to_avgtone(non_root_seq)
    output$goldstein_to_mentions <- goldstein_to_mentions(non_root_seq)
    output$sunflowerplots1 <- sunflowerplots1(non_root_seq)
    output$sunflowerplots2 <- sunflowerplots1(non_root_seq)
    output$mentions_and_avgtone <- mentions_and_avgtone(non_root_seq)
    output$mentions_and_avgtone <- eventcode_count(non_root_seq)
  })
})
