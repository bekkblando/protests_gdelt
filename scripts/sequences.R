library("bigrquery")

project <- "datascienceprotest"

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
  # minor_violence <- subset(sequence, EventRootCode == 14 & (AvgTone >= -50 || AvgTone <= 50))
  sequence <- sequence[!(sequence$EventRootCode == 14 & (AvgTone >= -50 || AvgTone <= 50))]
  
  # aggregate
  AvgMen <- aggregate(x = sequence$NumMentions, by = list(sequence$EventRootCode), FUN = mean)
  
  # Print to double check what aggregate did
  print(AvgMen)
  
  # Create subsets of the sequence that have average or above average number of mentions
  non_root_evTen <- subset(sequence, EventRootCode = 10 & NumMention >= floor(AvgMen[1,1]))
  
  #evElv_AvgMen <- aggregate
  non_root_evElv <- subset(sequence, EventRootCode = 11 & NumMention >= floor(AvgMen[2,1]))
  
  #evTwv_AvgMen
  non_root_evTwv <- subset(sequence, EventRootCode = 12 & NumMention >= floor(AvgMen[3,1]))
  
  #evThr_AvgMen
  non_root_evThr <- subset(sequence, EventRootCode = 13 & NumMention >= floor(AvgMen[4,1]))
  
  #evFrt_AvgMen
  non_root_evFrt <- subset(sequence, EventRootCode = 14 & NumMention >= floor(AvgMen[5,1]))
  
  non_root_sequence <- rbind(non_root_evTen, non_root_evElv, non_root_evTwv, non_root_evThr, non_root_evFrt)
  
  # Get the basic stats?
  seq_Stats <- apply(non_root_sequence, 2, mean)
  
  return(non_root_sequence, seq_Stats)
}


non_violent <- get_sequence(protests[10,])
