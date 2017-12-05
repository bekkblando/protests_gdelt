my_packages = c("shiny","leaflet", "plyr", "dplyr" , "httr", "bigrquery", "countrycode", "ggplot2", "shinyWidgets", "DT", "scales", "RPostgreSQL", "DBI","timevis", "extrafont")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
