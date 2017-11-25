symbols(events$NumMentions, events$AvgTone, circles=events$EventCode, inches=0.35, 
        fg="white", bg="blue", xlab = "Number of Mentions", ylab = "Average Tone") 
#add in text
# text(events$NumMentions, events$AvgTone, events$Actor1Name, cex=0.25)
# text(events$NumMentions, events$AvgTone, events$Actor2Name, cex=0.10)

symbols(events$GoldsteinScale, events$NumMentions, circles = events$IsRootEvent,
        inches=0.15, fg="white", bg="red", xlab = "Goldstein Scale", 
        ylab = "Number of Mentions")
#to add in text labels for individual datum
# text(events$GoldsteinScale, events$NumMentions, events$Actor1Name, cex=0.25)
# text(events$GoldsteinScale, events$NumMentions, events$Actor1Name, cex=0.10)

sunflowerplot(events$AvgTone, events$NumMentions)
sunflowerplot(events$NumMentions, events$NumSources)

#------------------
this_base <- "events"

my_data <- data.frame(
  event_code = factor(events$EventCode), mentions = data(events$NumMentions)
)
p <- ggplot2::ggplot(my_data, aes(x = event_code, y = mentions)) +
  geom_bar(stat = "identity", fill = "orange", colours(distinct = TRUE), 
           width = .02) + scale_y_continuous(breaks = sequence(events$NumMentions)) +
  labs(x = "Event Code", y = "Number of Mentions") +
  ggtitle("Number of Mentions per Event Code") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey50"),
        plot.title = element_text(size = rel(1.5), face = "bold", 
                                  vjust = 1.5), axis.title = element_text(face = "bold"))
p

library("ggplot2")

pie_base <- "events"

pie_data <- data.frame(
  variable = events$EventCode,
  value = events$IsRootEvent)

pie <- ggplot2::ggplot(pie_data, aes(x = variable, y= value, 
                                     fill=variable)) + 
  geom_bar(width = 1, colour ="black", stat="identity") +
  guides(fill=TRUE) +
  ggtitle("Is a Root Event Based on Event Code") +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.border = element_blank(),
                     plot.title = element_text(size = rel(1.5), face = "bold"),
                     axis.title = element_blank(), 
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     legend.text = element_text(events$EventCode))

pie