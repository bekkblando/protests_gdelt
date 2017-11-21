

install.packages("httr")
install.packages("bigrquery")
library("bigrquery")
library(DBI)
library(RSQLite)

db_name <- file.path('data','events.sqlite')

getwd()

# Use your project ID here
project <- "datascienceprotest" # put your project ID here

# Getting some relevent data
# Modify columns for different visualization
sql <- "SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:full.events] WHERE EventRootCode='14' and Year=2017"
sql <- "SELECT * FROM [gdelt-bq:gdeltv2.events] WHERE EventRootCode='14' and Year=2017"

# Execute the query and store the result NOTICE max_pages is set to Inf 
# Try not to run this too much or remove max_pages to get the first page
sql <-paste("SELECT GLOBALEVENTID,ActionGeo_Lat, ActionGeo_Long, Actor1Name, Actor2Name, EventCode, EventRootCode, AvgTone, GoldsteinScale, IsRootEvent, QuadClass, NumMentions, NumSources, Actor1Geo_Lat, Actor1Geo_Long, Actor2Geo_Lat, Actor2Geo_Long, FractionDate FROM [gdelt-bq:full.events] 
WHERE EventRootCode in ('10','11','12','13', '14') and ActionGeo_Lat=", violent_action_lat ," and ActionGeo_Long=", violent_action_long ,"and MonthYear=201701")
protests <- query_exec(sql, project = project, max_pages = Inf)

# NOTICE: Change the file path to yours
db_name <- file.path('/Users/BekkBlando/Documents/github/clemson/data_science/protests/data','events.sqlite')
db_conn <- dbConnect(SQLite(), db_name)


if (!dbExistsTable(db_conn, 'events')){
  dbSendQuery(conn = db_conn,
              'CREATE TABLE Events (GLOBALEVENTID INTEGER, ActionGeo_Lat FLOAT, 
              ActionGeo_Long FLOAT, Actor1Name TEXT, Actor2Name TEXT, EventCode INTEGER, EventRootCode INTEGER, AvgTone INTEGER, GoldsteinScale INTEGER, IsRootEvent INTEGER, QuadClass INTEGER, NumMentions INTEGER, NumSources INTEGER, Actor1Geo_Lat FLOAT, Actor1Geo_Long FLOAT, Actor2Geo_Lat FLOAT, Actor2Geo_Long FLOAT, FractionDate FLOAT)')
}

# Check it out yo
dbListTables(db_conn)
dbListFields(db_conn, 'Events') 

# Read our protests data in
dbWriteTable(db_conn, 'Events', protests, append=TRUE)

dbDisconnect(db_conn)
# Now that it's loaded in lets free some mem
rm(list = ls())

db_name <- file.path('/Users/BekkBlando/Documents/github/clemson/data_science/protests/data','events.sqlite')
db_conn <- dbConnect(SQLite(), db_name)

# Checkin out the data
local_data <- dbReadTable(db_conn, 'Events')


# Do Stuff yo

attach(local_data)

summary(local_data)


counts <- aggregate(local_data[4], list(EventCode), mean)
c(counts["Group.1"])[1]
c(counts$AvgTone)[2]

barplot(c(counts$AvgTone), names.arg=c(counts$Group.1))

nrow(local_data)

# Exploring The data
length(which(EventCode == 145 & (Actor1Name=="UNITED STATES" | Actor2Name=="UNITED STATES")))/length(which(Actor1Name=="UNITED STATES" | Actor2Name=="UNITED STATES")) * 100


nrow(local_data[ which(local_data$Actor1Name == "UNITED STATES" && local_data$EventCode == 145),])

length(which(EventCode == 145))/nrow(local_data) * 100

event_tone = data.frame(EventCode=EventCode, AvgTone=AvgTone)

local_data$EventCode <- as.factor(local_data$EventCode)

boxplot(AvgTone~EventCode,local_data)


sampled_data = local_data[sample(nrow(local_data), 5000), ]

root_events = data.frame(sampled_data[which(sampled_data$IsRootEvent == TRUE),])

non_root = sampled_data[which(sampled_data$IsRootEvent == FALSE),]


apply(root_events, 1, function(row){print(paste0(row))})
# Get any events with matching actors, within 1.5 months of the root event

# Add a new column to the root_events table of selected events
root_events$sequence<-NA


# Apply a function to return the IDs of events that fit the actors and are within the proper dates
create_sequence <- function(row){

  
  event_query = non_root[which(non_root$Actor1Name == row["Actor1Name"], non_root$Actor2Name == row["Actor2Name"]),]
  
  # TODO
  # Filter non_root events that happen more then 1.5 months from the root event
  # Filter event root codes based on https://www.hindawi.com/journals/ddns/2017/8180272/tab1/se

  if(!identical(event_query, character(0)) && length(event_query) != 0){
    print(paste0(event_query$GLOBALEVENTID))
    return(event_query$GLOBALEVENTID)
    }
}

root_events$sequence = apply(root_events, 1, function(row){create_sequence(row)})
