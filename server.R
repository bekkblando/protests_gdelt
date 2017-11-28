library(shiny)
library(leaflet)
library(plyr)
library("bigrquery")
library(countrycode)
library("ggplot2")
library(DT)
library(scales)
library(dplyr)

# High Leverage - Low Cost
  # Working on Heroku - Bekk - Sunday - 30 min
  # CSS Styling - Bekk - Sunday - 2 Hours
  # Graphs Per Sequence - Sadie 
    # Events in Order
  # Holistic Statistics - Sadie/Bekk - 3 Hours
    # Different types of protests increasing over time - Saide/Bekk
    # AvgTone and Goldstein Scale Per year - Sadie/Bekk
  # Explore Protests By Month

# Need to be done
  # Fix crashing - Bekk - Done
  # More Specific Sequences - Tyler and Bjerken - Complete

project <- "datascienceprotest" 

set_service_token("DataScienceProtest-2dc6d98778fa.json") #change this
# set_service_token(Sys.getenv("BIGQUERYCRED"))

above_average_mentions <- function(row, AvgMen){
  return(AvgMen[which(AvgMen$Group.1 == row["EventRootCode"]),]$x <= as.integer(row["NumMentions"]))
}


get_violent_protest <- function(year, month, day, country){
  fraction_date = signif(as.numeric(year) + (as.numeric(month) * 30 + as.numeric(day))/365, digits=8)
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, Actor1Geo_FullName, Actor2Geo_FullName, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145' and FractionDate=", fraction_date, " and ActionGeo_CountryCode='", paste0(countrycode(country, "country.name.en" ,"fips105")), "'")
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
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, NumMentions, FractionDate, SOURCEURL FROM [gdelt-bq:gdeltv2.events] WHERE EventCode='140' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")))
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_protest <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID, FractionDate, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, Actor1Geo_FullName, Actor2Geo_FullName, SOURCEURL FROM [gdelt-bq:gdeltv2.events] WHERE GLOBALEVENTID=", global_id)
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
  violent_actor1_geo = violent_protest$Actor1Geo_FullName
  violent_actor2_geo = violent_protest$Actor2Geo_FullName
  fractional_date = violent_protest$FractionDate

  lower_date = fractional_date - 45/365
  higher_date = fractional_date + 45/365

  sql <-paste0("SELECT GLOBALEVENTID, FractionDate, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long FROM [gdelt-bq:full.events] WHERE EventRootCode in ('10','11','12','13', '14') and FractionDate <=", higher_date ," and FractionDate >=", lower_date)

  # Add a parameter for the actors names
  if(!is.na(violent_actor1)){
    sql <- paste0(sql, " and Actor1Name='", violent_actor1, "'")
  }else{
    sql <- paste0(sql, " and Actor1Name is NULL")
  }
  if(!is.na(violent_actor2)){
    sql <- paste0(sql, " and Actor2Name='", violent_actor2, "'")
  }else{
    sql <- paste0(sql, " and Actor2Name is NULL")
  }
  
  # Add parameter for the violent actor geo
  if(!is.na(violent_actor1_geo)){
    sql <- paste0(sql, " and Actor1Geo_FullName='", violent_actor1_geo, "'")
  }
  if(!is.na(violent_actor2_geo)){
    sql <- paste0(sql, " and Actor2Geo_FullName='", violent_actor2_geo, "'")
  }
  
  print(sql)
  
  
  # TODO
    # Average tone maximum for occurances - violent protests
    # Above Average NumMentions for each event code - DONE
    # Summary Statistics - Average Goldstien, AvgTone, NumMentions per EventCode 

  sequence = query_exec(sql, project = project, max_pages = Inf)
  
  
  # aggregate
  AvgMen <- aggregate(x = sequence$NumMentions, by = list(sequence$EventRootCode), FUN = mean)
  
  # Create subsets of the sequence that have average or above average number of mentions
  sequence$AboveAvgMen <- apply(sequence, 1, function(row) { above_average_mentions(row, AvgMen) })
  
  non_root_sequence <- filter(sequence, AboveAvgMen == TRUE)
  
  return(non_root_sequence)
}

# Render Saidie's Graphs Yo

mentions_to_avgtone <- function(events){
  return(renderPlot(symbols(events$GoldsteinScale, events$AvgTone, 
          squares=sqrt(events$NumMentions), inches=0.85, fg="black", bg="maroon2", 
          xlab = "Goldstein Scale", ylab = "Average Tone", 
          main = "Number of Mentions by Goldstein Scale and Average Tone")))
}

goldstein_to_mentions <- function(events){
  return(renderPlot(symbols(events$GoldsteinScale, events$QuadClass, 
          circles = rescale(events$AvgTone, to = c(0,200)), inches=0.45, 
          fg="black", bg="slateblue2", xlab = "Goldstein Scale", 
          ylab = "Quad Class", 
          main = "Average Tone by Goldstein Scale and Quad Class" )))
}

code_tone <- function(events) {
  return(renderPlot(symbols(events$EventCode, events$NumMentions, 
          circles = rescale(events$AvgTone, to = c(0,200)), inches=0.35,
          fg = "darkblue", bg = "slateblue1", xlab = "Event Code",
          ylab = "Number of Mentions", 
          main = "Average Tone by Event Code and Number of Mentions")))
}

sunflowerplots1 <- function(events){
  return(renderPlot(sunflowerplot(events$AvgTone, events$NumMentions, xlab = "Average Tone",
          ylab = "Number of Mentions", col = "blue")))
  
}

sunflowerplots2 <- function(events){
  return(renderPlot(sunflowerplot(events$AvgTone, events$GoldsteinScale, xlab = "Average Tone",
         ylab = "Goldstein Scale")))
  
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
          plot.title = element_text(size = rel(2), face = "bold", vjust = 1.5), 
          axis.title = element_text(face = "bold")
    )
  
  return(renderPlot(p))

}

eventcode_count <- function(events){

  bar_data <- data.frame(
    Event_Code = events$EventCode,
    Average_Tone = events$AvgTone)
  
    b <- ggplot2::ggplot(bar_data, aes(x = Event_Code, y= Average_Tone, 
                                       fill=Event_Code)) + 
    geom_bar(width = 1, stat="identity", show.legend = TRUE) + 
    labs(x = "Event Code", y = "Average Tone") + 
      labs(title = "Average Tone Based on Event Code") +
    theme_light() + 
      theme(panel.grid = element_line(colour = "grey50"),
            panel.border = element_rect(linetype = "solid", fill = NA),
            plot.title = element_text(size = rel(2), face = "bold"),
            axis.title = element_text(face = "bold"), 
            legend.text = element_text(events$EventCode)
      )
    
   return(renderPlot(b))
}


avgtone_quadclass <- function(events) {
  
  plot_data1 <- data.frame(
    tone = events$AvgTone,
    quad = events$QuadClass)
  
  a <- ggplot2::ggplot(plot_data1, aes(x=tone, y=quad, fill = quad)) +
    geom_point(color="firebrick") +
    theme_light() + theme( panel.grid = element_line(colour = "grey50"),
                           panel.border = element_rect(linetype = "solid", fill = NA),
                           plot.title = element_text(size = rel(1.5), face = "bold"))
  
  return(renderPlot(a))
}


avgtone_time <- function(events) {
  
  plot_data2 <- data.frame(
    time = events$FractionDate,
    tone = events$AvgTone)
  
  t <- ggplot2::ggplot(plot_data2, aes(time, tone)) +
    #scale_x_date(format = "%d/%b") + 
    xlab("Fraction Date") + ylab("Average Tone") +
    geom_line()
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
  
  stayAlive <- reactiveTimer(intervalMs = 21000)
  observe({
    print("Staying Alive Ah Ah Ah Staying Alive")
    stayAlive()
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

  # Show popup on click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    
    root_protest = get_protest(click$id)
    output$selected_table <- renderTable(data.frame(root_protest), extensions="Responsive")
    non_root_seq_mentions = get_mentions(click$id)
    output$mentions <- renderTable(data.frame(non_root_seq_mentions), extensions="Responsive")
    
    # Just Exploring if analyze is false
    if(!input$analyze){
      return("Exploring the data")
    }

    # Show non-root events based on the violent protest
    non_root_seq = get_sequence(root_protest)


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
  
    # Render more notes if a violent protest exist within this
    output$seq_table <- renderTable(data.frame(non_root_seq), extensions="Responsive")
    # Render Sadie's Graphs
    output$mentions_to_avgtone <- mentions_to_avgtone(non_root_seq)
    output$goldstein_to_mentions <- goldstein_to_mentions(non_root_seq)
    output$code_tone <- code_tone(non_root_seq)
    output$sunflowerplots1 <- sunflowerplots1(non_root_seq)
    output$sunflowerplots2 <- sunflowerplots2(non_root_seq)
    output$mentions_and_avgtone <- mentions_and_avgtone(non_root_seq)
    output$eventcode_count <- eventcode_count(non_root_seq)
    output$avgtone_quadclass <- avgtone_quadclass(non_root_seq)
  })
})
