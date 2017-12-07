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
          value = "intro"
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
                  tags$div("Table with event code stuff:"),
                  tags$div("https://www.hindawi.com/journals/ddns/2017/8180272/tab1/"),
                  
                  tags$div("Event:"),
                  tags$div("http://gephardtdaily.com/national-international/venezuela- assembly-ousts- attorney-general- after-
                  guards-swarm- her-office/"),
                  tags$div("Global Event ID: 678966998"),
                  tags$div("Country: Venezuela"),
                  tags$div("Day: 7-5- 2017"),
                  tags$div("Fraction date: 2017.59"),
                  tags$div("Actor1: Protestor"),
                  tags$div("Actor2: Venezuela"),
                  tags$div("Notes from the app:")
                     ),
              tags$ul(
                tags$li("Event Code is 14 =>; Violent protest"),
                tags$li("More specific code: 145 =>; violent protest, riot"),
                tags$li("Protest involves a protestor vs the Venezuela government")
              ),
              tags$p("Notes from the article:"),
              tags$ul(
                tags$li("Attorney General Luisa Ortega Diaz unanimously voted out of office by the new National
                        Constituent Assembly", tags$li("Replaced by Tarek William Saab")),
                tags$li("Ortega had announced opening an Investigation into the Maduro-backed National
                        Constituent Assembly."),
                tags$li("Ortega clashed with new leader Maduro on repression of street protestors"),
                tags$li("Three dozen uniformed guards “sieged” her office"),
                tags$li("Her office assistants experienced abuse from Guards")
                ),
              tags$p(
                     tags$div("So Diaz protested Maduro’s repression of protestors and the election of the new National
                     Constituent Assembly but was met with a violent ousting for it??"),
                     tags$div("Sequence:"),
                     tags$div("http://news.trust.org/item/20170626174612-eb0k4"),
                     tags$div("Fraction Date: .48"),
                     tags$div("Eventrootcode: 14"),
                     tags$div("Notes from app:")
                     ),
              tags$ul(
                tags$li("Event Code is 14 =>; protest"),
                tags$li("Actual code is 141 =&gt; Demonstrate or rally")
              ),
              tags$p("Notes from the article:"),
              tags$ul(
                tags$li("Rally against Maduro’s government presence in Caracas"),
                tags$li("Protestors held up the national flag on a roadblock during the rally")
              ),
              tags$p(
                    tags$div("http://www.japantimes.co.jp/news/2017/07/05/world/politics-diplomacy- world/venezuelas-top-
                     prosecutor-snubs- court-summons- faces-suspension- showdown-maduro/"),
                     tags$div("Fraction Date: .51"),
                     tags$div("Eventrootcode: 11"),
                     tags$div("Notes from app:")
              ),
              tags$ul(
                tags$li("Event code is 11 =>; Disapprove – criticize, accuse, lawsuit, rally opposition"),
                tags$li("Actual code is 112 =>; Accuse")
              ),
              tags$p("Notes from the article:"),
              tags$ul(
                tags$li("Ortega refuses to appear in court"),
                tags$li("Accusations"),
                tags$li("Her assests were frozen and she was banned from leaving the country")
              ),
              tags$p(
                tags$div("https://guardian.ng/news/venezuelan-attorney- general-again- defies-leftist- president/"),
                tags$div("Fraction Date: .51"),
                tags$div("Eventrootcode: 11"),
                tags$div("Notes from app:")
                     ),
              tags$ul(
                tags$li("Event code is 11 =>; Disapprove – criticize, accuse, lawsuit, rally opposition"),
                tags$li("Actual code is 112 =>; Accuse")
              ),
              tags$p("Notes from article:"),
              tags$ul(
                tags$li("Haringhton was named deputy Attorney General in Caracas"),
                tags$li("Ortega tried denying her entry - “unconstitutional, illegal, and illegitimate” as National Assembly did not make
                        this appointment."),
                tags$li("Ortega accused - Pro-gov’t supreme court of undermining democracy,
                        Police of killing protestors, Maduro of violating the constitution with his plan to hold an unelected special
                        assembly to rewrite the document, National Constituent Assembly I take it"),
                tags$li("On Thursday protesters tried to march on the Supreme Court, but riot police pushed them
                        back and even chased some fleeing demonstrators into a huge shopping mall, where they
                        fired tear gas.")
                ),
              tags$li("A total of 45 people in the mall, including 17 children, received emergency medical
                      treatment after the incident, said Ramon Muchacho, mayor of the Caracas district
                      Chacao, which is an opposition stronghold."
              ),
              tags$p(
                tags$div("http://news.sky.com/story/one-killed- as-gunmen- attack-venezuelan- opposition-vote- 10951193"),
                tags$div("Fraction Date: .54"),
                tags$div("Eventrootcode: 11"),
                tags$div("Notes from app:")
              ),
              tags$ul(
                tags$li("Event code is 11 =>; Disapprove"),
                tags$li("112 - Accuse")
              ),
              tags$p("Notes from article:"),
              tags$ul(
                tags$li("Targeting Venezuelans voting in a National Consultation organized by the Opposition
                        - Vote was against the rewriting of the constitution")
                ),
              tags$p(
                tags$div("** Not directly relate to Ortega"),
                tags$div("https://www.newstodaynet.com/index.php/2017/07/31/10-killed- as-venezuela- vote-turns- violent/"),
                tags$div("Fraction Date: .58"),
                tags$div("Eventrootcode: 13"),
                tags$div("Notes from app:")
                     ),
              tags$ul(
                tags$li("Event code is 13 =>; Threaten"),
                tags$li("Actual code is 1312 =&gt; threaten with sanctions, boycott, embargo")
              ),
              tags$p("Notes from Article:"),
              tags$ul(
                tags$li("US threatens Venezuela to get their shit together after 10 people are killed in the vote
                        organized by the Opposition"),
                tags$li("The opposition had called for a boycott and mass demonstrations against the election,
                        which it condemned as a bid to install a dictatorship.")
                ),
              tags$p(
               tags$div("https://in.reuters.com/article/venezuela-politics- court-idINKCN1AV2LP"),
               tags$div("Fraction Date: .62"),
               tags$div("Eventrootcode: 14"),
               tags$div("Notes from app:")
              ),
              tags$ul(
                tags$li("Event code is 14 =>; Protests"),
                tags$li("Actual code is 140 =>; Engage in political dissent
                        o All civilian demonstrations and other collective actions carried out as protests
                        against target actor")
                ),
              tags$p("Notes from article:"),
              tags$ul(
                tags$li("Constituent Assembly ordered protests be detained and tried as civilians rather than
                        military as done previously"),
                tags$li("UN and various rights group protested the military detainment"),
                tags$li("The assembly’s first move was firing Ortega"),
                tags$li("Assembly trying to discourage protests"),
                tags$li("Oh no, possible dictatorship")
                )
              )
            )
        ),
        
        tabPanel("Holistic Statistics",
          value = "holistic_statistics",
          tags$h3("Number of Violent Protests per Year:"),
          imageOutput("violent_protests_yearly"),
          tags$div("From 1990 – 2017, reporting of violent protests has been increasing exponentially in the United States and the World at similar rates."),
          tags$h3("U.S. MIN – 1995 (65 violent protests)  6.8% of the world’s protests"),
          tags$h3("U.S. MAX – 2016 (13,237 violent protests)  13.8% of the world’s protests"),
          tags$h3("WW MIN – 1993 (961 violent protests)"),
          tags$h3("WW MAX – 2016 (96,100 violent protests)"),
          tags$h3("Number of Non-Violent Protests per Year:"),
          tags$h3("From 1990 – 2017, reporting of non-violent protests has been increasing exponentially in the United States and the World at similar rates."),
          tags$h3("U.S. MIN – 1993 (692 non-violent protests)  6.9% of the world’s protests"),
          tags$h3("U.S. MAX – 2016 (209,675 non-violent protests)  19.6% of the world’s protests"),
          tags$h3("WW MIN – 1992 (9963 non-violent protests)"),
          tags$h3("WW MAX – 2016 (1,067,129 non-violent protests)"),
          tags$h3("Average Tone Per Year Surrounding Violent Protests:"),
          tags$h3("WW – From 1990 to 2014, average tone was 3.8, but sharply dropped to -4.5 from 2015 to 2017."),
          tags$h3("U.S. – From 1990 to 2014, average tone was 4.6, but sharply dropped to -4.4 from 2015 to 2017."),
          tags$h3("Average Tone Per Year Surrounding Non-Violent Protests:"),
          tags$h3("WW – From 1990 to 2014, average tone was 4.3, but sharply dropped to -3.3 from 2015 to 2017."),
          tags$h3("U.S. – From 1990 to 2014, average tone was 5.2, but sharply dropped to -2.2 from 2015 to 2017."),              
          tags$h3("Average Goldstein Scale Per Year Surrounding Non-Violent Protests:"),
          tags$h3("WW – From 1990 to 2014, average Goldstein was 4.5, but sharply dropped to -4.4 from 2015 to 2017."),
          tags$h3("U.S. – From 1990 to 2014, average Goldstein was 4.2, but sharply dropped to -4.1 from 2015 to 2017.")
          
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