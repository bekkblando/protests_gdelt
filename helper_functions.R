project <- "datascienceprotest" 


render_documentation <- function(output){
  output$explore_venezuela <- renderImage({list(src =  "static/documentation/explore_venezuela.jpg", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$seq_explore_deetz <- renderImage({list(src =  "static/documentation/seq_explore_deetz.jpg", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$seq_explore_pt1 <- renderImage({list(src =  "static/documentation/seq_explore_pt1.jpg", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$seq_explore_pt2 <- renderImage({list(src =  "static/documentation/seq_explore_pt2.jpg", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure1 <- renderImage({list(src =  "static/documentation/Figure1.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure2 <- renderImage({list(src =  "static/documentation/Figure2.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure3 <- renderImage({list(src =  "static/documentation/Figure3.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure4 <- renderImage({list(src =  "static/documentation/Figure4.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure5 <- renderImage({list(src =  "static/documentation/Figure5.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure6 <- renderImage({list(src =  "static/documentation/Figure6.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure7 <- renderImage({list(src =  "static/documentation/Figure7.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure8 <- renderImage({list(src =  "static/documentation/Figure8.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$Figure9 <- renderImage({list(src =  "static/documentation/Figure9.png", class="responsive-img", width="650")}, deleteFile = FALSE)
  
  output$USnvp <- renderImage({list(src =  "static/documentation/USnvp.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$WWnvp <- renderImage({list(src =  "static/documentation/WWnvp.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$WWnvp_goldstein <- renderImage({list(src =  "static/documentation/WWnvp_goldstein.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$USnvp_goldstein <- renderImage({list(src =  "static/documentation/USnvp_goldstein.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$WWvp_tone <- renderImage({list(src =  "static/documentation/WWvp_tone.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$USvp_tone <- renderImage({list(src =  "static/documentation/USvp_tone.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$USvp <- renderImage({list(src =  "static/documentation/USvp.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
  output$WWvp <- renderImage({list(src =  "static/documentation/WWvp.JPG", class="responsive-img", width="650")}, deleteFile = FALSE)
}

events_to_timeline <- function(events){

  timeline <- data.frame(
    start   = events$Date,
    content = events$EventCode
  )
  return(timeline)
}

fraction_to_date_view <- function(fractional){
  year <- trunc(fractional)
  day <- round(((fractional - trunc(fractional)) * 365) %% 30)
  month <- round((((fractional - trunc(fractional)) * 365)-day)/30)
  
  if(month<10) month = paste(0, month, sep= "")
  if(day<10) day = paste(0, day, sep = "")
  dateString <- paste(year, month, day, sep ="-")
  
  return (dateString)
}

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

get_violent_protest_ex <- function(year, month, country){
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")), " and ActionGeo_CountryCode='", paste0(countrycode(country, "country.name.en" ,"fips105")), "'")
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

get_non_violent_protest_ex <- function(year, month){
  sql <- paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventRootCode='14' and EventCode <> '145' and MonthYear=", year, paste0(formatC(as.integer(month), width=2, flag="0")))
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_non_violent_protest <- function(year, month, day, country){
  fraction_date = signif(as.numeric(year) + (as.numeric(month) * 30 + as.numeric(day))/365, digits=8)
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, Actor1Geo_FullName, 
                Actor2Geo_FullName, EventCode, FractionDate FROM [gdelt-bq:gdeltv2.events] WHERE EventRootCode='14' 
                and EventCode <> '145' and FractionDate=", fraction_date, " and ActionGeo_CountryCode='", 
                paste0(countrycode(country, "country.name.en" ,"fips105")), "'")
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

get_protest <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, 
                AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, 
                Actor2Geo_Lat, Actor2Geo_Long, Actor1Geo_FullName, Actor2Geo_FullName, FractionDate, SOURCEURL 
                FROM [gdelt-bq:gdeltv2.events] WHERE GLOBALEVENTID=", global_id)
  return(query_exec(sql, project = project, max_pages = Inf))
}

get_mentions <- function(global_id){
  sql <- paste0("SELECT GLOBALEVENTID, MentionSourceName, MentionIdentifier FROM [gdelt-bq:gdeltv2.eventmentions] 
                WHERE GLOBALEVENTID=", global_id)
  print(paste0(sql))
  return(query_exec(sql, project = project, max_pages = Inf))
}

is_significant_event <- function(row){
  if(row["EventRootCode"] == 14)
    return(row["AvgTone"] <= -1)
  else
    return(TRUE)
}


get_sequence <- function(violent_protest){
  violent_actor1 = violent_protest$Actor1Name
  violent_actor2 = violent_protest$Actor2Name
  violent_actor1_geo = violent_protest$Actor1Geo_FullName
  violent_actor2_geo = violent_protest$Actor2Geo_FullName
  fractional_date = violent_protest$FractionDate
  
  lower_date = fractional_date - 45/365
  higher_date = fractional_date + 45/365
  
  sql <-paste0("SELECT GLOBALEVENTID, ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate, SOURCEURL FROM [gdelt-bq:full.events] WHERE EventRootCode in ('10','11','12','13', '14') and FractionDate <=", higher_date ," and FractionDate >=", lower_date)
  
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
  
  # Create a separate sequence of just the quantitative information
  # We want GLOBAL EVENT ID, eventrootcode, avgTone, NumMentions, and Goldstein Scale Reading
  # !!! Unsure if the select matrix needs quotations!!! 
  non_root_quant <- subset(x = non_root_sequence, select = c("GLOBALEVENTID", "EventRootCode", "NumMentions", "AvgTone", "GoldsteinScale"))
  
  # Get means of AvgTone, NumMentions, and Goldstein Scale per EventRootCode
  avgRootMen <- setNames(aggregate(x = non_root_quant$NumMentions, by = list(non_root_quant$EventRootCode), FUN = mean), c("EventRootCode","AvgRootMen"))
  avgRootTone <- setNames(aggregate(x = non_root_quant$AvgTone, by = list(non_root_quant$EventRootCode), FUN = mean), c("EventRootCode","AvgRootTone"))
  avgRootGold <- setNames(aggregate(x = non_root_quant$GoldsteinScale, by = list(non_root_quant$EventRootCode), FUN = mean),  c("EventRootCode","AvgRootGold"))
  
  # Get a count of each type of event
  numOfEvent <- setNames(as.data.frame(count(non_root_quant, c(EventRootCode))), c("EventRootCode", "NumOfEvents"))
  
  # Combine the stats into one data frame
  seq_Stats <- cbind(numOfEvent, avgRootMen = avgRootMen[,2], avgRootTone = avgRootTone[,2], avgRootGold = avgRootGold[,2])
  
  return(list(non_root_sequence, seq_Stats))
}


# Render Saidie's Graphs Yo

mentions_to_avgtone <- function(events){
  return(renderPlot(symbols(events$GoldsteinScale, events$AvgTone, 
                            squares=sqrt(events$NumMentions), inches=0.85, fg="black", bg="deepskyblue2", 
                            xlab = "Goldstein Scale", ylab = "Average Tone", 
                            main = "Number of Mentions by Goldstein Scale and Average Tone")))
}

code_tone <- function(events) {
  return(renderPlot(symbols(events$EventCode, events$NumMentions, 
                            circles = rescale(events$AvgTone, to = c(0,200)), inches=0.35,
                            fg = "black", bg = "orange1", xlab = "Event Code",
                            ylab = "Number of Mentions", 
                            main = "Average Tone by Event Code and Number of Mentions")))
}

eventcode_count <- function(events){
  
  bar_data <- data.frame(
    Event_Code = events$EventCode,
    Average_Tone = mean(events$AvgTone))
  
  b <- ggplot2::ggplot(bar_data, aes(x = Event_Code, y= Average_Tone, 
                                     fill=Event_Code)) + 
    geom_bar(width = 1, stat="identity", show.legend = TRUE, na.rm = TRUE) +
    labs(x = "Event Code", y = "Average Tone", title = "Average Tone Based on Event Code") +
    theme_light() + 
    theme(
      panel.background = element_rect(fill = "grey60", colour = "grey60",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"),
      plot.title = element_text(size = rel(1.5), face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.border = element_rect(linetype = "solid", fill = NA))
    
  
  return(renderPlot(b))
}



event_time <- function(events) {
  
  fraction_to_date <- function(fractional){
    year <- trunc(fractional)
    day <- round(((fractional - trunc(fractional)) * 365) %% 30)
    month <- round((((fractional - trunc(fractional)) * 365)-day)/30)
    
    dateString <- paste(year, sep="-")
    return (as.Date(dateString))
  }
  
  fractional<- events$FractionDate
  
  
  year <- trunc(fractional)
  day <- round(((fractional - trunc(fractional)) * 365) %% 30)
  month <- round((((fractional - trunc(fractional)) * 365)-day)/30)
  
  dateString <- paste(year, month, day, sep="-")
  response <- as.Date(dateString)
  
  
  scat_data <- data.frame(
    Dates = response,
    Average_Tone = events$AvgTone,
    EventCode = events$EventCode)
  
  t <- ggplot2::ggplot(scat_data, aes(x = Dates, y = Average_Tone)) +
    geom_count(aes(x = Dates, y = Average_Tone), size = 4, colour = "magenta2",
                   stat="identity", show.legend = TRUE, inherit.aes = TRUE, na.rm = TRUE) + 
    geom_text(aes(x = Dates, y = Average_Tone, label = EventCode),
              position = position_dodge(width = 1), vjust = -.75, hjust = 0.5, size = 3) +
    labs(x = "Date", y = "Average Tone", title = "Average Tone Over 90 Day Sequence") +
    theme_light() + 
    theme(
      panel.background = element_rect(fill = "grey60", colour = "grey60",
                                      size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"),
      plot.title = element_text(size = rel(1.5), face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.border = element_rect(linetype = "solid", fill = NA))
  
  return(renderPlot(t))
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
