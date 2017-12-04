library(RPostgreSQL)
library(DBI)

db_init <- function(){
  if(Sys.getenv("HEROKU") != ""){
    conn <- dbConnect(
      drv = dbDriver("PostgreSQL", max.con = 100),
      dbname = Sys.getenv("DATABASE"),
      host = Sys.getenv("HOST"),
      user = Sys.getenv("USER"),
      password = Sys.getenv("PASSWORD")
    )
    
    query = "CREATE TABLE IF NOT EXISTS flagged (globaleventid integer not null, flagnum INTEGER not null);"
    
    outp <- dbGetQuery(conn, query)
  return(conn)
  }
  # Return NA if connection is not possible - Error Code would be ideal
  return(NA)
}


get_flagged <- function(id){
  conn <- db_init()
  query <- sqlInterpolate(conn, sql = "SELECT * from flagged WHERE globaleventid = ?id", id = id)
  outp <- dbGetQuery(conn, query)
  response = data.frame(outp)
  dbDisconnect(conn)
  return(response)
}

set_flagged <- function(id){
  conn <- db_init()
  flag <- get_flagged(id)
  # If it hasn't been flagged before
  if(!length(flag)){
    # Make a data frame to write
    globaleventid <- c(id);
    flagnum <- c(1)
    flag <- data.frame(globaleventid, flagnum)
    # Write the heroku postgres
    dbWriteTable(conn, "flagged", value = flag, append = TRUE, row.names= FALSE)
  }else{
    # It's already been flagged, flag it again
    query <- sqlInterpolate(conn, "UPDATE flagged SET flagnum = flagnum + 1
      WHERE globaleventid = ?id", id = id)
    flag <- dbGetQuery(conn, query)
  }
  dbDisconnect(conn)
  return(flag)
}

db_dis <- function(){
  all_cons <- dbListConnections(dbDriver("PostgreSQL", max.con = 100))
   for(con in all_cons)
    dbDisconnect(con)
}


