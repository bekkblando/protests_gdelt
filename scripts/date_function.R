fraction_to_date <- function(fractional){
  year <- trunc(fractional)
  day <- round(((fractional - trunc(fractional)) * 365) %% 30)
  month <- round((((fractional - trunc(fractional)) * 365)-day)/30)
    
  dateString <- paste(year, sep="-")
  return (as.Date(dateString))
}


fractional<- 2018.2849
fractional<- 2016.9999

year <- trunc(fractional)
day <- round(((fractional - trunc(fractional)) * 365) %% 30)
month <- round((((fractional - trunc(fractional)) * 365)-day)/30)

dateString <- paste(year, month, day, sep="-")
response <- as.Date(dateString)
