library(bigrquery)
library("ggplot2")

project <- "pvp1-182616" 

#set_service_token("/Users/BekkBlando/Documents/github/clemson/data_science/protests/DataScienceProtest-2dc6d98778fa.json") 

# World Wide
# Graph of Violent Protests Over Time-------------------------------------------------------------

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] 
                  WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] 
                  WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
violent_protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

violent_protest_world = rbind(violent_protest_graph_new, violent_protest_graph_old)

violent_protest_world$Year = as.factor(violent_protest_world$Year)

g <- ggplot(violent_protest_world, aes(x = violent_protest_world$Year, 
                                       y = violent_protest_world$f0_)) + geom_bar(stat = "identity", fill = "violetred3") +
  geom_text(aes(x = violent_protest_world$Year, 
                y = violent_protest_world$f0_, label = violent_protest_world$f0_),
            position = position_dodge(width = 1), vjust = -0.5, hjust = 0.5, size = 3)
g + labs(x = "Year", y = "Violent Protests", 
         title = "WORLDWIDE: Number of Violent Protests per Year") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_y_continuous(breaks = c(0,12500,25000,37500,50000,62500,75000,87500,100000, 112500)) 

# Graph of Non-violent Protests Over Time------------------------------------------------------------------

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] 
                  WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] 
                  WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

protest_world = rbind(protest_graph_new, protest_graph_old)

protest_world$Year = as.factor(protest_world$Year)

g <- ggplot(protest_world, aes(x = protest_world$Year, y = protest_world$f0_)) +
  geom_bar(stat = "identity", fill = "green3") +
  geom_text(aes(x = protest_world$Year, 
                y = protest_world$f0_, label = protest_world$f0_),
            position = position_dodge(width = 1), vjust = -0.5, hjust = 0.5, size = 3)
g + labs(x = "Year", y = "Non-Violent Protests", 
         title = "WORLDWIDE: Number of Non-Violent Protests per Year") + 
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_y_continuous(breaks = c(0,125000,250000,375000,500000,625000,750000,875000,1000000)) 

# Average Tone Surrounding Non- Violent Protests Per Year-------------------------------------------------------

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_tone = rbind(protest_graph_new_tone, protest_graph_old_tone)

protest_world_tone$Year = as.factor(protest_world_tone$Year)

g <- ggplot(protest_world_tone, aes(x = protest_world_tone$Year, 
                                    y = protest_world_tone$f0_)) + geom_bar(stat = "identity", fill = "darkorchid3") 
g + labs(x = "Year", y = "Average Tone", 
         title = "WORLDWIDE: Average Tone Per Year Surrounding Non-Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))

# Average Tone Surrounding Violent Protests Per Year---------------------------------------------------

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
violent_protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

violent_protest_world_tone = rbind(violent_protest_graph_new_tone, violent_protest_graph_old_tone)

violent_protest_world_tone$Year = as.factor(violent_protest_world_tone$Year)
g <- ggplot(violent_protest_world_tone, aes(x = violent_protest_world_tone$Year, 
                                            y = violent_protest_world_tone$f0_)) + geom_bar(stat = "identity", fill = "orange1") 
g + labs(x = "Year", y = "Average Tone", 
         title = "WORLDWIDE: Average Tone Per Year Surrounding Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))



# Average Goldstein Scale for Protests Per Year---------------------------------------------------------

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_gold <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_gold <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_gold = rbind(protest_graph_new_gold, protest_graph_old_gold)

protest_world_gold$Year = as.factor(protest_world_gold$Year)
g <- ggplot(protest_world_gold, aes(x = protest_world_gold$Year, 
                                    y = protest_world_gold$f0_)) + geom_bar(stat = "identity", fill = "deepskyblue3") 
g + labs(x = "Year", y = "Goldstein Scale", 
         title = "WORLDWIDE: Average Goldstein Scale Per Year Surrounding Non-Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))

# Average Goldstein Scale for Violent Protests Per Year---------------------------------------------------

#new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] 
#                       WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
#old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] 
#                       WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

#violent_protest_graph_new_gold <- query_exec(new_sql_gold, project = project, max_pages = Inf)
#violent_protest_graph_old_gold <- query_exec(old_sql_gold, project = project, max_pages = Inf)

#violent_protest_world_gold = rbind(violent_protest_graph_new_gold, violent_protest_graph_old_gold)

#violent_protest_world_gold$Year = as.factor(violent_protest_world_gold$Year)
#g <- ggplot(violent_protest_world_gold, aes(x = violent_protest_world_gold$Year, 
#            y = violent_protest_world_gold$f0_)) + geom_bar(stat = "identity", fill = "magenta3") 
#            g + labs(x = "Year", y = "Goldstein Scale", 
#            title = "WORLDWIDE: Average Goldstein Scale Per Year Surrounding Violent Protests") +
#            theme(
#              panel.background = element_rect(fill = "grey60", colour = "grey60",
#                                              size = 0.5, linetype = "solid"),
#              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                              colour = "white"), 
#              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                              colour = "white"),
#              plot.title = element_text(size = rel(1.5), face = "bold"),
#              axis.title = element_text(face = "bold"),
#              axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
#              panel.border = element_rect(linetype = "solid", fill = NA))


# Country USA----------------------------------------------------------------------------------------
# Graph of Violent Protests Over Time----------------------------------------------------------------

project <- "pvp1-182616" 

#set_service_token("/Users/BekkBlando/Documents/github/clemson/data_science/protests/DataScienceProtest-2dc6d98778fa.json") 

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] 
                  WHERE EventCode LIKE '145%' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] 
                  WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

violent_protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
violent_protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

violent_protest_world = rbind(violent_protest_graph_new, violent_protest_graph_old)

violent_protest_world$Year = as.factor(violent_protest_world$Year)

g <- ggplot(violent_protest_world, aes(x = violent_protest_world$Year, 
                                       y = violent_protest_world$f0_)) + geom_bar(stat = "identity", fill = "violetred1") +
  geom_text(aes(x = violent_protest_world$Year, 
                y = violent_protest_world$f0_, label = violent_protest_world$f0_),
            position = position_dodge(width = 1), vjust = -0.5, hjust = 0.5, size = 3) 
g + labs(x = "Year", y = "Violent Protests", 
         title = "UNITED STATES: Number Of Violent Protests per Year") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_y_continuous(breaks = c(0,2500,5000,7500,10000,12500)) 

# Graph of Non-Protests Over Time------------------------------------------------------------------------

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] 
                  WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] 
                  WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

protest_world = rbind(protest_graph_new, protest_graph_old)
protest_world$Year = as.factor(protest_world$Year)

g <- ggplot(protest_world, aes(x = protest_world$Year, y = protest_world$f0_)) + 
  geom_bar(stat = "identity", fill = "green1") +
  geom_text(aes(x = protest_world$Year, y = protest_world$f0_, label = protest_world$f0_),
            position = position_dodge(width = 1), vjust = -0.5, hjust = 0.5, size = 3)
g + labs(x = "Year", y = "Non-Violent Protests", 
         title = "UNITED STATES: Number Of Non-Violent Protests per Year") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA)) +
  scale_y_continuous(breaks = c(0,25000,50000,75000,100000,125000,150000,175000,200000)) 

# Average Tone Surrounding Protests Per Year--------------------------------------------------------------

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_tone = rbind(protest_graph_new_tone, protest_graph_old_tone)

protest_world_tone$Year = as.factor(protest_world_tone$Year)

g <- ggplot(protest_world_tone, aes(x = protest_world_tone$Year, 
                                    y = protest_world_tone$f0_)) + geom_bar(stat = "identity", fill = "darkorchid1") 
g + labs(x = "Year", y = "Average Tone", 
         title = "UNITED STATES: Average Tone Per Year Surrounding Non-Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))


# Average Tone Surrounding Violent Protests Per Year------------------------------------------------------

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode LIKE '145%' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

violent_protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
violent_protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

violent_protest_world_tone = rbind(violent_protest_graph_new_tone, violent_protest_graph_old_tone)

violent_protest_world_tone$Year = as.factor(violent_protest_world_tone$Year)

g <- ggplot(violent_protest_world_tone, aes(x = violent_protest_world_tone$Year, 
                                            y = violent_protest_world_tone$f0_)) + geom_bar(stat = "identity", fill = "goldenrod1") 
g + labs(x = "Year", y = "Average Tone", 
         title = "UNITED STATES: Average Tone Per Year Surrounding Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))


# Average Goldstein Scale for Protests Per Year-----------------------------------------------------------

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] 
                       WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

protest_graph_new_gold <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_gold <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_gold = rbind(protest_graph_new_gold, protest_graph_old_gold)

protest_world_gold$Year = as.factor(protest_world_gold$Year)

g <- ggplot(protest_world_gold, aes(x = protest_world_gold$Year, 
                                    y = protest_world_gold$f0_)) + geom_bar(stat = "identity", fill = "deepskyblue1") 
g + labs(x = "Year", y = "Goldstein Scale", 
         title = "UNITED STATES: Average Goldstein Scale Per Year Surrounding Non-Violent Protests") +
  theme(
    panel.background = element_rect(fill = "grey60", colour = "grey60",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(size = rel(1.5), face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
    panel.border = element_rect(linetype = "solid", fill = NA))


# Average Goldstein Scale for Violent Protests Per Year---------------------------------------------------

#new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] 
#                       WHERE EventCode LIKE '145%' and Year>2014 and ActionGeo_CountryCode='US' GROUP BY YEAR")
#old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] 
#                       WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 and ActionGeo_CountryCode='US' GROUP BY YEAR")

#violent_protest_graph_new_gold <- query_exec(new_sql_gold, project = project, max_pages = Inf)
#violent_protest_graph_old_gold <- query_exec(old_sql_gold, project = project, max_pages = Inf)

#violent_protest_world_gold = rbind(violent_protest_graph_new_gold, violent_protest_graph_old_gold)

#violent_protest_world_gold$Year = as.factor(violent_protest_world_gold$Year)

#g <- ggplot(violent_protest_world_gold, aes(x = violent_protest_world_gold$Year, 
#            y = violent_protest_world_gold$f0_)) + geom_bar(stat = "identity", fill = "magenta1") 
#            g + labs(x = "Year", y = "Goldstein Scale", 
#            title = "UNITED STATES: Average Goldstein Scale Per Year Surrounding Violent Protests") +
#            theme(
#              panel.background = element_rect(fill = "grey60", colour = "grey60",
#                                              size = 0.5, linetype = "solid"),
#              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                              colour = "white"), 
#              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                              colour = "white"),
#              plot.title = element_text(size = rel(1.5), face = "bold"),
#              axis.title = element_text(face = "bold"),
#              axis.text.x = element_text(angle=45,hjust=1,vjust=0.5),
#              panel.border = element_rect(linetype = "solid", fill = NA))
