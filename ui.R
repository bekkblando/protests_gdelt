library(shiny)
library(leaflet)
library(plyr)
library(countrycode)
library(shinyWidgets)
library(timevis)


shinyUI(
  fluidPage(
    tags$head(
      tags$style("
        html, body {
          max-width: 100%;
          overflow-x: hidden;
        };
      "),
      tags$meta(
        name="viewport",
        content="width=device-width"
      ),
      tags$script(src="materialize.min.js"),
      theme= "materialize.min.css"
    ),
    tags$div(class="nav-wrapper",
      navbarPage("GDELT Protests",
        id = "nav",
           
        tabPanel("Intro",
          value = "intro",
          tags$h3("Why analyze this data?"),
          tags$p("
            Effective protests are the landmark of a strong democracy. With a history ranging from early republics to modern day struggles, protests have been a form of expression and medium for change for hundreds of years. 
            However, violence can sometimes emerge from these events which not only hurts the surrounding community, but also the cause of the peaceful protesters. The goal of this project is to provide analysis on why a protest might turn violent. 
            The use of this knowledge is so that organizers and everyday citizens can be aware of potential violence and push in a peaceful direction."
          ),
          tags$h3("The GDELT Data Set"),
          tags$p("
            The Global Database of Events, Language and Tone (GDELT) uses deep learning algorithms to translate the world’s news in 65 languages:  extracting more than 300 categories of events, 
            using 60 predictor variables, millions of themes, and thousands of emotions into the GDELT Event Database."),
          tags$p("
                Each event is categorized by GDELT using machine learning. As the data is not categorized by humans the events can be mis-categorized or from unchecked news sources. 
                 Please verify the SOURCEURL's before investigating the event."),
          tags$p("Please checkout our code!"),
          tags$a(href="https://github.com/bekkblando/protests_gdelt", target="_blank", "Our Code!")
        ),
        
        tabPanel("Documentation",
          value = "Documentation",
          fluidRow(
            tags$div(
              class="container",
              tags$h1("Documentation"),
              tags$h5("ActorName:"),
              tags$p("The actual name of the Actor1 or Actor2. In the case of a political leader or organization, 
                       this will be the leader’s formal name (GEORGE W BUSH, UNITED NATIONS), for a geographic match it 
                       will be either the country or capital/major city name (UNITED STATES / PARIS), and for ethnic, religious, 
                       and type matches it will reflect the root match class (KURD, CATHOLIC, POLICE OFFICER, etc). May be 
                       blank if the system was unable to identify an Actor1 or Actor2."),
              tags$h5("GoldsteinScale:"),
              tags$p("Each event code is assigned a numeric score from -10 to +10, capturing the theoretical 
                       potential impact that type of event will have on the stability of a country. This is known as the Goldstein 
                       Scale. This field specifies the Goldstein score for each event type. NOTE: this score is based on the type 
                       of event, not the specifics of the actual event record being recorded – thus two riots, one with 10 people 
                       and one with 10,000, will both receive the same Goldstein score."),
              tags$h5("NumMentions:"),
              tags$p("This is the total number of mentions of this event across all source documents during 
                       the 15 minute update in which it was first seen. Multiple references to an event within a single 
                       document also contribute to this count. This can be used as a method of assessing the 'importance' of 
                       an event: the more discussion of that event, the more likely it is to be significant."),
              tags$h5("AvgTone:"),
              tags$p("This is the average “tone” of all documents containing one or more mentions of this event 
                      during the 15 minute update in which it was first seen. The score ranges from -100 (extremely negative) 
                       to +100 (extremely positive). Common values range between -10 and +10, with 0 indicating neutral. This 
                       can be used as a method of filtering the “context” of events as a subtle measure of the importance of an 
                       event and as a proxy for the “impact” of that event. For example, a riot event with a slightly negative 
                       average tone is likely to have been a minor occurrence, whereas if it had an extremely negative average 
                       tone, it suggests a far more serious occurrence. A riot with a positive score likely suggests a very minor 
                       occurrence described in the context of a more positive narrative (such as a report of an attack occurring 
                       in a discussion of improving conditions on the ground in a country and how the number of attacks per 
                       day has been greatly reduced)."),
              tags$h5("EventCode:"),
              tags$p("This is the action code describing the action that Actor1 performed upon Actor2."),
              tags$div("010 Make Public Statement"),
              tags$div("020 Appeal"),
              tags$div("030 Express Intent to Cooperate"),
              tags$div("040 Consult"),
              tags$div("050 Engage in Diplomatic Cooperation"),
              tags$div("060 Engage in Material Cooperation"),
              tags$div("070 Provide Aid"),
              tags$div("080 Yield"),
              tags$div("090 Investigate"),
              tags$div("100 Demand"),
              tags$div("110 Disapprove"),
              tags$div("120 Reject"),
              tags$div("130 Threaten"),
              tags$div("140 Protest"),
              tags$div("150 Exhibit Force Posture"),
              tags$div("160 Reduce Relations"),
              tags$div("170 Coerce"),
              tags$div("180 Assault"),
              tags$div("190 Fight"),
              tags$div("200 Use Unconventional Mass Violence"),
              tags$h3("Helper Functions for Sequencing:"),
              tags$p("For creating the non-root event sequences, the focus was on actors, date, number of
                mentions (NumMentions), and average tone (AvgTone) of protests."
                ),
              tags$p("Actors and dates formalize a relevancy amongst the sequence events. For simplicity,
                     Actor1 and Actor2 are guaranteed to match for each event. Events where the same actor1 is
                     targeting same actor2 should theoretically be related given time constraints. In one research
                     article, Predicting Social Unrest Events with Hidden Markov Models Using GDELT in the
                     journal Discrete Dynamics in Nature and Society, the researches mention normalizing the
                     sequence on a 90-day period as this typically will include all parts of the protest cycle. This
                     project includes this idea. Each event in the sequence happens within 45 days before or after the
                     root event."
                ),
              tags$p("Additionally, number of mentions and average tone of protests factor into the
                    significance of the events in the sequence. The sequences are made using only events that have
                     an above average number of mentions per event root code type. The more significant events will
                     probably have at least an average or higher amount of mentions for its event type. The overall
                     number of mention average would fail to account for certain less frequent event types that may
                     be significant but not have a high number of mentions for one reason or another. Furthermore,
                     each event with root code 14 is guaranteed to have a negative average tone. In the article
                     explaining each variable, it mentions that riots and protests that have a positive average tone are
                     more than likely to minor occurrences."
                ),
              tags$h1("Examples"),
              tags$p("
            This is a document sharing my experience using the GDELT Violent Protest Analysis Application to investigate and 
                       visualize recent events related to the Freddie Gray Case. The original case occurred in Baltimore, Maryland in 2015 
                       when Freddie Gray was into custody for carrying a knife in West Baltimore. Gray was handcuffed and shackled in the 
                       back of a police transport van, but was not restrained with a seatbelt. After being driven around in the van for a 
                       significant part of the day unrestrained, Gray sustained severe injuries to his spinal cord. He died a week later, 
                       and the officers involved did not receive any criminal prosecution at the time. This led to large scale riots, arson, 
                       and looting throughout Baltimore. 
                       "),
              tags$p("
                       Recently, two of the five officers accepted minor disciplinary actions (5 days of unpaid leave) in lieu of 
                       facing a trial board and possible termination. This led to additional violent protests as the public remained 
                       outraged by the actions of these officers, and it is the root event for this analysis. 
                       "),
              tags$style("#Figure1{height: 100%;}"),
              tags$style("#Figure1 {height: 100% !important;}"),
              tags$p(imageOutput("Figure1")),
              tags$p("
                       I started by inputting September 10, 2017 as the date of my query with the date drop down menu in the top left,
                       and then the USA as my country of interest in the middle drop down menu. This displayed the map that you can see 
                       above with 10 unique geotags. Each of those tags is a violent protest that occurred with the parameters that were 
                       input, specifically violent protests in the USA that occurred on September 10, 2017. You can zoom in and out and 
                       view the map in more detail by using the zoom buttons on the top left, or by scrolling. I zoomed into the Tri-State 
                       area and clicked on the geotag in Baltimore. In most cases, this would update the map to display a Red Tag on our 
                       root event (the violent protest) and a series of blue tags on any events that led up to the occurrence of the 
                       violent protest. In this example, however, all the events occurred in the same location, so only the main event tag 
                       is visible on the map.
                       "),
              tags$style("#Figure2 {height: 100% !important;}"),
              imageOutput("Figure2"),
              tags$p("
                       Selecting one of the Red Tagged violent protests supplies the user with more details about 
                       the event, including the date that it occurred on, the Global Event ID that the GDELT data 
                       set associates with the event, and the coordinates of where the event occurred. We are also 
                       given details about who was involved in the event in the form of Actor 1 and Actor 2, what 
                       the event was (Event Code), and how it was received by the public (average tone), and how much 
                       of an impact that this event likely had on the stability of an area. For my case, we see that Actor1 
                       is listed as NA. This means that no person or organization was the aggressor in the action, which is 
                       what we would expect from a general protest by the public. Actor 2 is the Police, which fits as the 
                       protest was against suspected police brutality. The event has a code of 145 means that it was a violent 
                       protest with a relatively negative average tone (-6.36). In the data set we used, average tone scale goes from 
                       -100 to 100, but typical values fall only between -10 and +10. We can also see that this event had a -7.5 on the 
                       Goldstein scale, which is a measure of the theoretical impact an event has on the stability of a geographic location 
                       measured from +10 to -10. In this case, -7.5 means that the event had a destabilizing effect on the area.
                       "),
              imageOutput("Figure3"),
              tags$p("
                       Above is the list of secondary events that are part of the sequence related to the violent protest in Baltimore. 
                       We can see that these events all occurred in the same location across the course of a few weeks and involved the 
                       general public and the Police. The event codes in the middle tell us what each event was and this sequence includes 
                       accusations (11), demands (10), and violent protests (14). We are also able to see the general tone that each of these events encountered.
                       "),
              imageOutput("Figure4"),
              tags$p("
                       We also have access to how many times each event was mentioned across all source documents during the 15-minute period in which this event
                       was first seen. This allows us to assess the importance of an event. Only events with a greater than average number of mentions in this critical
                       period are included in the sequence.  From this section of the application we can access these source materials to get more information before 
                       delving into the analysis. If we look at these articles, we see that not all of them seem to be directly related with the discipline of the officers 
                       that I initially mentioned, so what happened?
                       "),
              tags$p("
                       Upon further investigation, I found that only two of the articles listed in the sequence were unrelated to the case, and that is a side effect of how well 
                       the data can be filtered using the information given in the GDELT Database. The other 4 that don’t directly mention the officers are also based on the case, 
                       and revolved around the officers’ superiors and the trials that await them. While our algorithms aren’t 100% accurate at filtering out noise, only receiving 
                       two stray articles out of over a quarter of a billion records across 85 different languages from around the world is still impressive.
                       "),
              imageOutput("Figure5"),
              tags$p("
                       Above is plot displaying the relationship between the Average Tone of each of the events against the relative impact to the locations stability. 
                       We can also see the number of mentions each event has based on the width of the blocks. Here the larger the impact to stability, the more mentions an 
                       event seemed to have.
                       "),
              imageOutput("Figure6"),
              tags$p("
                       It doesn’t seem that the average number of mentions of the events in this sequence are directly correlated with the type of event that occurred, but we 
                       do see that one of the protests received significantly more attention than the other events.
                       "),
              imageOutput("Figure7"),
              tags$p("
                       Tone however, does seem to grow more negative with the nature of the event. Here we see that both the accusations and the protests received very negative 
                       scores as compared to accusations.
                       "),
              imageOutput("Figure8"),
              tags$p("
                       It also appears that after the initially negative tone near the beginning of the sequence, the public seemed to calm down during the events of the protest and 
                       cool off. The outlier in the bottom is an accusation unrelated to the sequence. 
                       "),
              imageOutput("Figure9"),
              tags$p("
                       Finally, we can see a breakdown of the timeline of the events. It seems that most the protests within the sequence occurred directly around the time that the officers 
                       accepted a small punishment in lieu of facing trial, and even more non-protest activity occurred around the times of the trials for the officer’s superiors.
                       "),
              tags$h3("Finding a Protest"),
              tags$p(
                "Finding a protest starts with choosing a country, a month, and a year. Protest Explorer
                allows you to look up and see if there are even protests during that period. As an example, take
                searching in Venezuela during July 2017."
              ),
              imageOutput("explore_venezuela"),
              tags$p(
                "Clearly for this example, there are numerous possible protests given the country’s current
                state. Within Protest Explorer, each marker will provide more details including a specific date.
                To extend the given example of Venezuela, next we’ll explore a protest that occurred on July 5 th ,
                2017."
              ),
              tags$h3("Exploring a sequence:"),
              tags$p("
                     When you have the specific date and country, it is possible to explore an event even
                     further. Taking the date July 5 th 2017, we can find"
              ),
              imageOutput("seq_explore_pt1"),
              tags$p("Further reducing, choose the protest in Caracas."),
              imageOutput("seq_explore_pt2"),
              tags$p("Once loaded, the sequence information will appear beneath the graph."),
              imageOutput("seq_explore_deetz"),
              tags$p("At this point, all the information is present to explore the sequence of events."),
              tags$h3("Using the information:"),
              tags$p(
                "From the tables of information, the two participating actors are protestors and the
                Venezuelan government and these will take place between 5-26- 2017 and 7-16- 2017. Each event
                has a source, and using the source article attached to the root event, it becomes evident that this
                sequence should deal primarily with the former Attorney General Luisa Ortega Diaz, and the
                president Maduro and his Constituent Assembly. The lack of any other marker on the mark on
                the map also indicates that the events happen in the same location, Caracas. According the table
                at the top of the figure above, this sequence most prevalent event type is 11, which is a
                disapproval of sorts. There were two event code 14, which are violent protests. The only other
                code to appear in this sequence is a 13 which is a threat. Now we can consider more into each
                event:"
              ),
              tags$p(
                     tags$div("5-26- 17"),
                     tags$div("The first event in the sequence has the event code 141. Using the Codebook, this tells us
                     the event is a demonstration. The average tone shows the event was negatively reported in the
                     news outlets, and the Goldstein Scale reading indicates that the demonstration was close to being
                     a threat with negative nonmilitary sanction.
                     Although this event appears in the sequence, using the source article url at the far right of
                     the table, the event appears to be a demonstration involving roadblocks that was between the
                     
                     Venezuelan opposition and the government. The connection between this demonstration and
                     Ortega is either not clear or non-existent. The parameters that narrow down the search are not
                     perfect, and if more research were done into this event and no connection was found, the event
                     can be flagged as incorrect."),
                     tags$div("6-6-2017"),
                     tags$div("The event code for both events is 112, and, using the Codebook, this tells us the event is
                     an accusation. The average tone shows the event was negatively reported in the news outlets, and
                     the Goldstein Scale reading indicates that the accusation were criticisms of disapproval and
                     blame.
                     Reading into both sources, it could be argued that GDELT has the same event listed
                     twice. The first event involves Ortega refusing to appear for her court date, and the second event
                     involves Ortega denying the legitimacy of her replacement. In both instances, she makes the
                     following accusations:")
                     ),
              tags$ul(
                tags$li("Pro-government supreme court of undermining democracy"),
                tags$li("Police of killing protestors"),
                tags$li("Maduro of violating the constitution with his plan to hold an unelected special assembly
                        to rewrite the document")
                ),
              tags$p(
                
                tags$div("6-17-2017"),
                tags$div("Again, the event code for this event is 112, and, using the Codebook, this tells us the
                event is an accusation. The average tone shows the event was negatively reported in the news
                outlets, and the Goldstein Scale reading indicates that the accusation were criticisms of
                disapproval and blame.
                Reading into the source article, it becomes obvious the actors in this situation are not the
                same. This is an event happening around the opposition trying to cancel the Constituent
                Assembly’s task of rewriting the Venezuelan constitution. This event is loosely connected to
                Ortega as she was accusing the assembly of being illegitimate at this point in time, but there is no
                direct connection."),
                tags$div("7-1-2017"),
                tags$div("The event code for this event is 1312, and, using the Codebook, this tells us the event is a
                threat of sanctions, boycott, and/or embargo. The average tone shows the event was negatively
                reported in the news outlets, and the Goldstein Scale reading indicates it is a threat of
                nonmilitary sanction.
                Reading into the source article, the event is a threat from the United States of America
                concerning the president’s concerning dictator-esque decisions. Specifically, the opposition had
                called for a boycott and mass demonstrations against the election, which it condemned as a bid to
                install a dictatorship."),
                tags$div("7-5-2017 – Root Event**"),
                tags$div("The root event of this sequence has the event code 145, which, using the Codebook, this
                tells us the event is a violent protest or riot. The average tone in the media was slightly negative
                
                but not as negative as compared to some of the earlier events. Using the Goldstein Scale reading,
                the event should involve a display or exercise of armed force. Reading into the source article,
                there were about three dozen armed guards that swarmed Ortega’s Caracas office when she was
                unanimously voted out of office by the new National Constituent Assembly."),
                tags$div("7-16-2017"),
                tags$div("The final event reported in this sequence had the code 140 which is an engagement in
                political dissent. The Goldstein Scale reading indicates that the demonstration was close to being
                a threat with negative nonmilitary sanction. Using the source article provided, it becomes evident
                that the engagement was directed at how the newly appointed Constituent Assembly was trying
                the civilian protestors. Their first move had been to fire Ortega who was one of the highest-
                ranking prosecutors accusing Maduro of mistreating civilian protestors. Now the assembly was
                receiving protests from Venezuelan Rights Groups, and the United Nations to try civilian
                protestors as civilians rather than military personnel.")
              ),
              tags$h3("Conclusion:"),
              tags$p(
                tags$div("For the example, the sequence shows the firing of the former Attorney General Luisa
                     Ortega Diaz for her accusations against the current Venezuelan president Maduro including his
                     treatment of protestors and appointment of the National Constituent Assembly, and it shows her
                     violent departure. However, it is not clear that each event in this sequence is related.
                     Overall, and for further use of the application, a basic understanding of each event is
                     clear using a combination of the Cameo Codebook of Event Codes, and the Goldstein Scale.
                     Although, it is definitely best to use the source articles to gain a deeper understand of the events
                     in the sequence and whether the events are truly related. That being said, all the tools are present
                     in the tables of information."),
                  tags$div("Table with event codes:"),
                  tags$div("https://www.hindawi.com/journals/ddns/2017/8180272/tab1/")
              )
            )
          )
        ),
        
        tabPanel("Holistic Statistics",
          value = "holistic_statistics",
          tags$style("div.shiny-image-output{height: 100% !important}"),
          fluidRow(
            column(12, tags$h3("Number of Violent Protests per Year:")),
            column(12, tags$h5("From 1990 – 2017, reporting of violent protests has been increasing exponentially in the United States and the World at similar rates."))
            ),
          fluidRow(column(6, 
              imageOutput("USvp"),
              column(6, tags$p("U.S. MIN – 1995 (65 violent protests)  6.8% of the world’s protests")),
              column(6, tags$p("U.S. MAX – 2016 (13,237 violent protests)  13.8% of the world’s protests"))       
                          )
              ),

          
          fluidRow(
            column(6, 
                imageOutput("WWvp"),
                 fluidRow(
                   column(6, tags$div("WW MIN – 1993 (961 violent protests)")),          
                   column(6, tags$div("WW MAX – 2016 (96,100 violent protests)"))
                 )
            )
          ),
          

          fluidRow(
            column(12, tags$h3("Number of Non-Violent Protests per Year:")),
            column(12, tags$h5("From 1990 – 2017, reporting of non-violent protests has been increasing exponentially in the United States and the World at similar rates."))          
          ),
          fluidRow(
            column(6, imageOutput("USnvp"),
            column(6, tags$p("U.S. MIN – 1993 (692 non-violent protests)  6.9% of the world’s protests")),
            column(6, tags$p("U.S. MAX – 2016 (209,675 non-violent protests)  19.6% of the world’s protests"))
            )
          ),

          fluidRow(
            column(6, imageOutput("WWnvp"),
            column(6, tags$p("WW MIN – 1992 (9963 non-violent protests)")),
            column(6, tags$p("WW MAX – 2016 (1,067,129 non-violent protests)"))
            )
          ),


          tags$h3("Average Tone Per Year Surrounding Violent Protests:"),
          tags$h5("WW – From 1990 to 2014, average tone was 3.8, but sharply dropped to -4.5 from 2015 to 2017."),
          fluidRow(
            column(6, imageOutput("WWvp_tone"))
          ),
          
          tags$h5("U.S. – From 1990 to 2014, average tone was 4.6, but sharply dropped to -4.4 from 2015 to 2017."),
          fluidRow(
            column(6, imageOutput("USvp_tone"))
          ),

          tags$h3("Average Goldstein Scale Per Year Surrounding Non-Violent Protests:"),
          fluidRow(
            column(12, imageOutput("WWnvp_goldstein"),
            column(12, tags$h5("WW – From 1990 to 2014, average Goldstein was 4.5, but sharply dropped to -4.4 from 2015 to 2017."))
            )
          ),
             
          fluidRow(
            column(12, imageOutput("USnvp_goldstein")),
            column(12, tags$h5("U.S. – From 1990 to 2014, average Goldstein was 4.2, but sharply dropped to -4.1 from 2015 to 2017."))
          )
          
        ),
        
        tabPanel("Protest Explorer",
          value = "protest_explorer",
          # Year Picker
          fluidRow(
            column(12, selectInput("year", "Year", choices = c(" ", seq(2015, 2017)))),
            column(12, selectInput("month", "Month", choices = c(" ", seq(1, 12)))),
            column(12, selectInput("country_ex", "Country Code", choices = countrycode_data$country.name.en)),
            column(12, actionButton("explore_date_submit", "Explore Protests"))
          ),
          fluidRow(
            column(12, leafletOutput("map_ex"))
          ),
          fluidRow(
            tags$style("
             div#selected_table_ex{
                       overflow-x: scroll;
                       width:100%;
                       };
            "),
            column(12, tableOutput('selected_table_ex'))
          ),
          fluidRow(
            tags$style("
             div#seq_table_ex{
                       overflow-x: scroll;
                       width:100%;
                       };
            "),
            column(12, tableOutput('seq_table_ex'))
          ),
          fluidRow(    
            tags$style("
             div#mentions_ex{
                       overflow-x: scroll;
                       width:100%;
                       };
             "),
            column(12, tableOutput('mentions_ex'))
          )
        ),
        
        tabPanel("Sequence Explorer", 
          value = "sequence_explorer",
          # Year Picker
          fluidRow(
            column(4, dateInput("date", "Date: ")),
            column(4, selectInput("country", "Country", choices = countrycode_data$country.name.en)),
            column(4, selectInput("violence", "Violent Or Non Violent", choices = c("Violent Protests", "Non-Violent Protests")))
          ),
          fluidRow(
            column(12, actionButton("date_submit", "Get Protests With These Parameters"))
          ),
          fluidRow(
            column(12, leafletOutput("map"))
          ),
          fluidRow(
            column(4, switchInput(inputId = "analyze", label="Analyze Protest", value = TRUE)),
            column(4, switchInput(inputId = "flag", label="Flag as Incorrect", value = FALSE)),
            column(4, tableOutput('flagged'))
          ),
          fluidRow(
            tags$style("
             div#selected_table{
                       overflow-x: scroll;
                       width:100%;
                       };
            "),
              column(12, tableOutput('selected_table'))
          ),
          fluidRow(
            tags$style("
                       div#summary_stats{
                       overflow-x: scroll;
                       width:100%;
                       };
                       "),
            column(12, tableOutput('summary_stats'))
            ),
          fluidRow(
            tags$style("
             div#seq_table{
              overflow-x: scroll;
              width:100%;
             };
            "),
              column(12, tableOutput('seq_table'))
          ),
          fluidRow(  
            tags$style("
             div#mentions{
              overflow-x: scroll;
              width:100%;
             };
            "),
              column(12, tableOutput('mentions'))
          ),
          fluidRow(
            column(6, plotOutput('mentions_to_avgtone')),
            column(6, plotOutput('code_tone'))
          ),
          fluidRow(
            column(6, plotOutput('eventcode_count')),
            column(6, plotOutput('event_time'))
          ),
          fluidRow(
            column(12, timevisOutput('events_to_timeline'))
          )
        )
      )
    )
  )
)