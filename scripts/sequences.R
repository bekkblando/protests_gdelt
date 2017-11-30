library("bigrquery")
library(dplyr)


project <- "datascienceprotest"

above_average_mentions <- function(row, AvgMen){
  return(AvgMen[which(AvgMen$Group.1 == row["EventRootCode"]),]$x <= as.integer(row["NumMentions"]))
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
  fractional_date = violent_protest$FractionDate
  
  lower_date = fractional_date - 45/365
  higher_date = fractional_date + 45/365
  
  sql <-paste0("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:full.events] WHERE EventRootCode in ('10','11','12','13', '14') and FractionDate <=", higher_date ," and FractionDate >=", lower_date)
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
  print(sql)
  
  
  # TODO
  # Average tone maximum for occurances - violent protests
  # Above Average NumMentions for each event code - needs to be troubleshot, KA POW
  # Summary Statistics - Average Goldstien, AvgTone, NumMentions per EventCod e - Should be done?
  
  sequence = query_exec(sql, project = project, max_pages = Inf)
  
  # To remove minor violent occurences:
  # sequence <- sequence[!(sequence$EventRootCode == 14 & sequence$AvgTone >= 0)]
  sequence$Significant <- apply(sequence, 1, function(row){ is_significant_event(row) })
  
  sequence <- filter(sequence, Significant == TRUE)
  
  # aggregate
  AvgMen <- aggregate(x = sequence$NumMentions, by = list(sequence$EventRootCode), FUN = mean)
  
  # Create subsets of the sequence that have average or above average number of mentions
  sequence$AboveAvgMen <- apply(sequence, 1, function(row) { above_average_mentions(row, AvgMen) })
  
  non_root_sequence <- filter(sequence, AboveAvgMen == TRUE)

  # Get the basic stats?
  # seq_Stats <- aggregate(non_root_sequence, 2, mean)

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
  
  print(seq_Stats)
    
  return(non_root_sequence)
}


non_violent <- get_sequence(events[11,])
