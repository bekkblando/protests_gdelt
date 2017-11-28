# World Wide
  # Graph of Violent Protests Over Time
project <- "datascienceprotest" 

set_service_token("/Users/BekkBlando/Documents/github/clemson/data_science/protests/DataScienceProtest-2dc6d98778fa.json") 

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
violent_protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

violent_protest_world = rbind(violent_protest_graph_new, violent_protest_graph_old)

violent_protest_world$Year = as.factor(violent_protest_world$Year)
g <- ggplot(violent_protest_world, aes(x = violent_protest_world$Year, y = violent_protest_world$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Number Of Violent Protests")

  # Graph of Non-Protests Over Time

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

protest_world = rbind(protest_graph_new, protest_graph_old)

protest_world$Year = as.factor(protest_world$Year)
g <- ggplot(protest_world, aes(x = protest_world$Year, y = protest_world$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Number Of Non-Violent Protests")
  
  # Average Tone Surrounding Protests Per Year

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_tone = rbind(protest_graph_new_tone, protest_graph_old_tone)

protest_world_tone$Year = as.factor(protest_world_tone$Year)
g <- ggplot(protest_world_tone, aes(x = protest_world_tone$Year, y = protest_world_tone$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Tone Per Year Surrounding Non-Violent Protests")


  
  # Average Tone Surrounding Violent Protests Per Year

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
violent_protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

violent_protest_world_tone = rbind(violent_protest_graph_new_tone, violent_protest_graph_old_tone)

violent_protest_world_tone$Year = as.factor(violent_protest_world_tone$Year)
g <- ggplot(violent_protest_world_tone, aes(x = violent_protest_world_tone$Year, y = violent_protest_world_tone$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Tone Per Year Surrounding Violent Protests")

  
  # Average Goldstein Scale for Protests Per Year

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_gold <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_gold <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_gold = rbind(protest_graph_new_gold, protest_graph_old_gold)

protest_world_gold$Year = as.factor(protest_world_gold$Year)
g <- ggplot(protest_world_gold, aes(x = protest_world_gold$Year, y = protest_world_gold$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Goldstein Scale Per Year Surrounding Non-Violent Protests")


  
  # Average Goldstein Scale for Violent Protests Per Year

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new_gold <- query_exec(new_sql_gold, project = project, max_pages = Inf)
violent_protest_graph_old_gold <- query_exec(old_sql_gold, project = project, max_pages = Inf)

violent_protest_world_gold = rbind(violent_protest_graph_new_gold, violent_protest_graph_old_gold)

violent_protest_world_gold$Year = as.factor(violent_protest_world_gold$Year)
g <- ggplot(violent_protest_world_gold, aes(x = violent_protest_world_gold$Year, y = violent_protest_world_gold$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average GoldsteinScale Per Year Surrounding Violent Protests")



# Country USA
# Graph of Violent Protests Over Time
project <- "datascienceprotest" 

set_service_token("/Users/BekkBlando/Documents/github/clemson/data_science/protests/DataScienceProtest-2dc6d98778fa.json") 

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 and CountryGROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
violent_protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

violent_protest_world = rbind(violent_protest_graph_new, violent_protest_graph_old)

violent_protest_world$Year = as.factor(violent_protest_world$Year)
g <- ggplot(violent_protest_world, aes(x = violent_protest_world$Year, y = violent_protest_world$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Number Of Violent Protests")

# Graph of Non-Protests Over Time

new_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql <- paste0("SELECT COUNT(GLOBALEVENTID), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new <- query_exec(new_sql, project = project, max_pages = Inf)
protest_graph_old <- query_exec(old_sql, project = project, max_pages = Inf)

protest_world = rbind(protest_graph_new, protest_graph_old)

protest_world$Year = as.factor(protest_world$Year)
g <- ggplot(protest_world, aes(x = protest_world$Year, y = protest_world$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Number Of Non-Violent Protests")

# Average Tone Surrounding Protests Per Year

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_tone = rbind(protest_graph_new_tone, protest_graph_old_tone)

protest_world_tone$Year = as.factor(protest_world_tone$Year)
g <- ggplot(protest_world_tone, aes(x = protest_world_tone$Year, y = protest_world_tone$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Tone Per Year Surrounding Non-Violent Protests")



# Average Tone Surrounding Violent Protests Per Year

new_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql_tone <- paste0("SELECT AVG(AvgTone), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new_tone <- query_exec(new_sql_tone, project = project, max_pages = Inf)
violent_protest_graph_old_tone <- query_exec(old_sql_tone, project = project, max_pages = Inf)

violent_protest_world_tone = rbind(violent_protest_graph_new_tone, violent_protest_graph_old_tone)

violent_protest_world_tone$Year = as.factor(violent_protest_world_tone$Year)
g <- ggplot(violent_protest_world_tone, aes(x = violent_protest_world_tone$Year, y = violent_protest_world_tone$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Tone Per Year Surrounding Violent Protests")


# Average Goldstein Scale for Protests Per Year

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>2014 GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] WHERE EventCode <> '145' and EventRootCode='14' and Year>1989 and Year<2015 GROUP BY YEAR")

protest_graph_new_gold <- query_exec(new_sql_tone, project = project, max_pages = Inf)
protest_graph_old_gold <- query_exec(old_sql_tone, project = project, max_pages = Inf)

protest_world_gold = rbind(protest_graph_new_gold, protest_graph_old_gold)

protest_world_gold$Year = as.factor(protest_world_gold$Year)
g <- ggplot(protest_world_gold, aes(x = protest_world_gold$Year, y = protest_world_gold$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Goldstein Scale Per Year Surrounding Non-Violent Protests")



# Average Goldstein Scale for Violent Protests Per Year

new_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:gdeltv2.events] WHERE EventCode LIKE '145%' and Year>2014 GROUP BY YEAR")
old_sql_gold <- paste0("SELECT AVG(GoldsteinScale), Year FROM [gdelt-bq:full.events] WHERE EventCode LIKE '145%' and Year>1989 and Year<2015 GROUP BY YEAR")

violent_protest_graph_new_gold <- query_exec(new_sql_gold, project = project, max_pages = Inf)
violent_protest_graph_old_gold <- query_exec(old_sql_gold, project = project, max_pages = Inf)

violent_protest_world_gold = rbind(violent_protest_graph_new_gold, violent_protest_graph_old_gold)

violent_protest_world_gold$Year = as.factor(violent_protest_world_gold$Year)
g <- ggplot(violent_protest_world_gold, aes(x = violent_protest_world_gold$Year, y = violent_protest_world_gold$f0_))
g + geom_bar(stat = "identity") + xlab("Year") + ylab("Average GoldsteinScale Per Year Surrounding Violent Protests")



