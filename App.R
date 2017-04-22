#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 
# library(extrafont)
# font_import()
library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(shinythemes)
library(networkD3)
library(leaflet)
library(scales)
# library(extrafont)
# font_import(paths = NULL, recursive = TRUE, prompt = FALSE,
#             pattern = NULL)


source("30_ModelFunc.R")

#import datasets needed:
data16 = read.csv("data16.csv")
linegraphdata = read.csv("linegraphdata.csv")
lng_lat_df = read.csv("mapdata.csv")
beachLinks = read.csv("beachLinks.csv")
beachLinks$value <- ((beachLinks$value ^ 4)*10) #allows you to see the differences between values more readily. is this misleading without a legend?
beachNodes = read.csv("beachNodes.csv")
#                                          orange      blue        green      purple     pink        red
ColourScale <- 'd3.scaleOrdinal().range([ "#F57C00", "#0288D1",  "#229954", "#7B1FA2", "#EC407A", "#B71C1C"]);'

#list all possible options for the selection menus:
beach_options = c("12th","31st","57th", "63rd", "Albion", "Calumet", "Foster", "Howard", "Jarvis", "Juneway","Leone", "Montrose","North Avenue", "Oak Street", "Ohio", "Osterman", "Rainbow", "Rogers", "South Shore", "39th")
predictor_options <- c("Water_Temperature", "Dew_Point", "Humidity", "Rain_Intensity", "Wind_Speed",
                       "Barometric_Pressure", "Visibility", "Cloud_Cover")

#load fonts for graphs:
# windowsFonts(Arial=windowsFont("TT Arial"))
# windowsFonts(Times=windowsFont("TT Times New Roman"))
# windowsFonts(Eras=windowsFont("Eras Light ITC"))
# windowsFonts(PR=windowsFont("Poor Richard"))

# Create a palette that maps factor levels to colors for the interactive map:
#                          blue       red        green       pink       orange     purple
factpal <- colorFactor(c("#0288D1", "#B71C1C",  "#229954", "#EC407A", "#F57C00", "#7B1FA2"), lng_lat_df$Group)

#Create label content for interactive map:
content2 <- paste(sep = "",
                  lng_lat_df$Client.ID,
                  ": You caught ",
                  "12",
                  " unsafe beach days, and you missed",
                  "7",
                  " unsafe beach days."
)

content5 <- paste(sep = "",
                  lng_lat_df$Client.ID,
                  " || ",
                  lng_lat_df$AvgEcoliLabel
)

content6 <- paste0("<strong>Beach: </strong>", 
                   lng_lat_df$Client.ID, 
                   "<br><strong>Avg E. coli level: </strong>", 
                   lng_lat_df$AvgEcoliLabel
)

content7 <- paste0("<strong>Beach: </strong>", 
                   lng_lat_df$Client.ID, 
                   "<br><strong>Unsafe beach days you caught: </strong>", 
                   "12",
                   "<br><strong>Unsafe beach days you missed: </strong>", 
                   "7"
)






# Define UI for application that draws graphs
ui <- fixedPage(
  #to test out themes, uncomment next line and comment out the line below it.
  #shinythemes::themeSelector(), 
  theme = shinytheme("yeti"),
  tabsetPanel(
  
  tabPanel("Home", 
           fixedRow(
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
                    ),
             column(4, align="center",
                    tags$h1("City of Chicago Beaches")
             ),
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
                   )
             ),
           fixedRow(
               column(12, #align="center",
                    tags$h5("This is an interactive site for understanding the ", tags$i("E. coli"), " levels at your beaches in Chicago.
                            Please visit the tabs above to access different interactive features."),
                    tags$h5("Everyone knows and loves Chicago's beaches. What they might not know is the work that goes on behind the scenes in order to provide Chicagoans 
                            with a timely, accurate, and efficient measure of water quality."),
                    tags$h5("Every day during the summer, the US Geological Survey (USGS) collects water samples from each beach, and grows the samples in the lab.
                        This is how we know the", tags$i("E. coli"), "levels in the water. However, this test takes 12 hours to run, and can be costly. By the time the
                            test is complete, the beach day is over."),
                    tags$h5("In order to inform the public whether the water is safe for swimming, the City of Chicago uses a combination of the", tags$i("E. coli"), "levels
                            from the previous days, environmental factors, and knowledge of the relationships between beaches to", tags$em("predict"), "the current",
                            tags$i("E. coli"), "levels.")
                   )
                ),

           fixedRow(
             column(4,
                    wellPanel(tags$h5("The ", tags$strong("Map"), " tab describes the beach locations and relationships."),
                    tags$img(height = 238.015625,
                             width = 326.03125,
                             src = "Map1.png")),
                    wellPanel(tags$h5("The", tags$strong("Beach Days"), "tab allows you to see how the ", tags$i("E. coli"), " cutoff level affects the number 
                            of swimmable beach days in the summer."),
                    tags$img(height = 203.1061589,
                             width = 281.77326289,
                             src = "BeachDays.PNG"))
                    ),
             column(4,
                    wellPanel(tags$h5("The ", tags$strong("Predictors"), "tab allows you to explore how different elements of the weather and environment
                            trend with ", tags$i("E. coli"), " levels."),
                    tags$img(height = 239.994,
                             width = 286.39284,
                             src = "Predictors.PNG"))
                    ),
             column(4,
                    wellPanel(tags$h5("The ", tags$strong("Build a Model"), "tab gives you the tools you need to build a model that predicts the ", tags$i("E. coli"), " 
                            levels at each beach, and see if you're as smart as you think you are."),
                    tags$img(height = 266.0989356,
                             width = 308.3321,
                             src = "Model.PNG")),
                    wellPanel(tags$h5("The ", tags$strong("Network"), "tab describes the relationships that help us predict the ", tags$i("E. coli"), "  levels."),
                    tags$img(height = 245.767189,
                             width = 302.205778,
                             src = "Network.PNG"))
                    )
           ),
           fixedRow(
             column(12, align="center",
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
             ),
             column(12,
                    tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                            information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                                        "linkedin.com/in/renel-chesak"), "")
           ),
           column(12, align="center",
                  tags$h4("__________________________________________________________________________________________________________________________________________________")
           )
                    )
  ),
  
  
  
  
  
  
  tabPanel("Map",
           fixedRow(
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(4, align="center",
                    tags$h1("Which Beaches Behave Similarly?")
             ),
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             )
           ),
           fixedRow(
             column(12, align="center", 
                    leafletOutput('mymap2', width = 1200, height = 700)
             ),
             column(12, #align="center", 
                    absolutePanel(
                      bottom = 200, right = 0, width = 350,
                      draggable = TRUE,
                      wellPanel(tags$h4("Draggable Box"),
                      tags$h5("Each beach is represented on the map as a circle, and they are color-coded to show which beaches have ", tags$i("E. coli"), " 
                         levels that fluctuate together. The size of the circle represents the average ", tags$i("E. coli"), " level at that beach."),
                      tags$h5("By understanding these
                              relationships, data scientists can use only one beach out of a group to predict the ", tags$i("E. coli"), " levels at the other beaches
                              in that group. This means that scientists only need to collect ", tags$i("E. coli"), " samples from one beach in the group, which can eliminate
                              unnecessary spending.")),
                      style = "opacity: 0.92"
                    )
                    )
                    ),
           fixedRow(
             column(12, align="center",
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
             ),
             column(12,
                    tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                            information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                                        "linkedin.com/in/renel-chesak"), "")
           ),
           column(12, align="center",
                  tags$h4("__________________________________________________________________________________________________________________________________________________")
           )
                    )
  ),
  
  
  
  
  
  
  
  tabPanel("Beach Days",
           fixedRow(
               column(4, align="center",
                      tags$img(height = 110.1333,
                               width = 166.4,
                               src = "SmallerChicagoFlag.PNG")
               ),
               column(4, align="center",
                      tags$h1("Set Your Own Limit")
               ),
               column(4, align="center",
                      tags$img(height = 110.1333,
                               width = 166.4,
                               src = "SmallerChicagoFlag.PNG")
               )
           ),
           fixedRow(
             column(12, #align="center",
                    absolutePanel(
                      bottom = -375, right = 100, width = 300,
                      draggable = TRUE,
                      wellPanel(
                        sliderInput("slider", label = h5("", tags$i("E. coli"), "cutoff (in CFU/100mL)"), min = min(data16$Escherichia.coli), max = 2419, 
                                    value = 235),
                        tags$h5(
                          "The City of Chicago sets the", tags$i("E. coli"), "cutoff at no greater than 235 Colony Forming Units per 100 mL. This is a level reached 
                          in collaboration with microbiologists to ensure the safety of the public."),
                        tags$h5("Move the slider bar above to see 
                          how many swimmable beach days there are at different cutoff levels.")

                        ),
                      style = "opacity: 0.92"
                        )),
             column(8, align="center",
                    tags$head(tags$style(type="text/css", "
                                         #loadmessage {
                                         position: fixed;
                                         top: 0px;
                                         left: 0px;
                                         width: 100%;
                                         padding: 5px 0px 5px 0px;
                                         text-align: center;
                                         font-weight: bold;
                                         font-size: 100%;
                                         color: #000000;
                                         background-color: #CCFF66;
                                         z-index: 105;
                                         }
                                         ")),
                    plotOutput("graph2"),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div("Loading...",id="loadmessage"))
                  )
             ),
           
           fixedRow(
             column(12, align="center",
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
             ),
             column(12,
                    tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                            information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                                        "linkedin.com/in/renel-chesak"), "")
           ),
           column(12, align="center",
                  tags$h4("__________________________________________________________________________________________________________________________________________________")
           )
                    )
           ),
  
  
  
  
  
  
  tabPanel("Predictors",
           fixedRow(
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(4, align="center",
                    tags$h1("Pick a Predictor")
             ),
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(12, 
                    absolutePanel(
                      bottom = -335, right = 50, width = 300,
                      draggable = TRUE,
                      wellPanel(
                        selectInput("predictor", tags$h5("Select a Predictor:"), choices = predictor_options),
                        tags$h5(
                          "Which elements of the environment trend with ", tags$i("E. coli"), " levels? Data scientists use 
                          elements that trend together to predict each other. Look for 
                          the predictor whose line graph peaks and valleys with the average", tags$i("E. coli"), "levels."
                        )
                        ),

                      style = "opacity: 0.92"
                      )
             ),
             column(7, offset=1,
                    tags$head(tags$style(type="text/css", "
                                         #loadmessage {
                                         position: fixed;
                                         top: 0px;
                                         left: 0px;
                                         width: 100%;
                                         padding: 5px 0px 5px 0px;
                                         text-align: center;
                                         font-weight: bold;
                                         font-size: 100%;
                                         color: #000000;
                                         background-color: #CCFF66;
                                         z-index: 105;
                                         }
                                         ")),
                    plotOutput("graph3"),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                     tags$div("Loading...",id="loadmessage"))
                    )
                    ),
           fixedRow(
             column(12, align="center",
                    tags$h4("      "),
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
             ),
             column(12,
                    tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                            information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                                        "linkedin.com/in/renel-chesak"), "")
           ),
           column(12, align="center",
                  tags$h4("__________________________________________________________________________________________________________________________________________________")
           )
                    )
           ),
  
  
  
  
  
  
  
  tabPanel("Network",
           fixedRow(
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(4, align="center",
                    tags$h1("A Network of Beaches")
             ),
             column(4, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(12, #align="center",
                    tags$h4("Below, you can see which beaches have similar", tags$i("E. coli"), "levels. The size of the circle represents the average", 
                            tags$i("E. coli"), "level at that beach. Each color represents a group that fluctuates together.
                            The thickness of each line represents the strength of the connection between the", tags$i("E. coli"), "levels at those 2 beaches.
                            Understanding these relationships is a crucial part of predicting ", tags$i("E. coli"), " levels at each beach."),
             column(12, align="center",
                    forceNetworkOutput("force")

                    )
                    )
  ),

  fixedRow(
    column(12, align="center",
           tags$h4("__________________________________________________________________________________________________________________________________________________")
    ),
    column(12,
           tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                   information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                               "linkedin.com/in/renel-chesak"), "")
    ),
    column(12, align="center",
           tags$h4("__________________________________________________________________________________________________________________________________________________")
    )
           )
),
  
  
  
  
           
           
  
  tabPanel("Build a Model",
           fixedRow(
             column(2, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             ),
             column(3, align="center",
                    tags$h1("Can You Predict Better?")
             ),
             column(2, align="center",
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
             )
           ),
  
           fixedRow(
    column(8, offset=0,
                tags$h5("Decide which beaches will be predictive, hit the Update button, and your chosen beaches will be entered into an algorithm.
                       Give the algorithm ", tags$b("10 seconds"), " to run, and the resulting model predictions will populate for your model, and for 
                       the USGS model. Can you predict better that the US Geological Survey?"),
               tags$h5("After selecting beaches and hitting update, the interactive map that populates below will show you how your model did at 
              each beach. The graph that populates below, on the right, will show you how well your model performed compared
                       to the model used by the City of Chicago."), 
               tags$h5("Move the slider bar on the right to adjust the hit rate, which is the percentage of unsafe beach days that the models will 
                       catch. Remember, you want a high hit rate in order to catch all the bad days so that swimmers don't get sick. However, that 
                       comes at a cost: false alarms which costs taxpayer money because no one buys tickets to go to the beach. This presents an optimization problem where you are not only 
                       trying to get the most hits, but also the most hits for the lowest cost to taxpayers."),
           tags$h3(textOutput("results_verbiage")),
               tags$head(tags$style(type="text/css", "
             #loadmessage {
                                    position: fixed;
                                    top: 0px;
                                    left: 0px;
                                    width: 100%;
                                    padding: 5px 0px 5px 0px;
                                    text-align: center;
                                    font-weight: bold;
                                    font-size: 100%;
                                    color: #000000;
                                    background-color: #CCFF66;
                                    z-index: 105;
                                    }
                                    ")),
           ###########################################################
           absolutePanel(
             bottom = 0, right = -375, width = 350,
             draggable = TRUE,
             wellPanel(
               checkboxGroupInput("chosen_beaches", "Select which beaches will be predictive:", beach_options),
               actionButton(inputId = "go", label = "Update beaches (~10 sec)"),
               tags$h5("After building your model with the beaches, move the slider bar below to optimize the model so that you can achieve the most hits per dollar of taxpayer money."),
               sliderInput("slider2", label = h5("Hit rate:"), min = 5, 
                           max = 100, value = 95
               ),
               tags$h4("Model Results:"),
               plotOutput("graph1")
               ),
             style = "opacity: 0.92"
               ),
           ##########################################################
   
           leafletOutput('mymap', width = 725, height = 900),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage"))
    ),
    column(12, align="right",
           tags$h6(tags$b("False Alarms:"), "Safe beach days incorrectly flagged as unsafe, based upon the model."), 
           tags$h6(tags$b("Hits:"), "Unsafe beach days caught by the model.")
            )
           ),
    fixedRow(
             column(12, align="center",
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
             ),
            column(12,
                    tags$h5("The algorithm used in this app was built by the City of Chicago's Department of Innovation and Technology 
                           (", tags$a(href = "https://github.com/Chicago", "@cityofchicago"), "),
                            and it's incorporation into this app would not have been possible without the help of 
                            Callin Osborn (", tags$a(href = "https://github.com/CallinOsborn", "@CallinOsborn"), ")."), 
                    tags$h5("This Shiny app was built by Renel Chesak (", tags$a(href = "https://github.com/rchesak", "@rchesak"), "). For contact 
                            information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", 
                                                                                        "linkedin.com/in/renel-chesak"), "")
            ),
            column(12, align="center",
                    tags$h4("__________________________________________________________________________________________________________________________________________________")
                    )
                    )
  )

  
  )
  
  )

    
  

    




# Define server logic required to draw graphs
server <- function(input, output,session) {

  ####################
  #graph 1 and interactive map:
  observeEvent(input$go, { #everything inside here is tied to the update button
    #call the function given the input, and return the output as model_summary:
    model_summary <- beach_choose(beaches = as.character(input$chosen_beaches),thresh = as.numeric(235),num_of_folds = 3) 
    
    #map:
    # create a default web map 
    map <- leaflet::leaflet(lng_lat_df) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
    #customize the map:
    map2 <- map %>%
      #use a third-party tile that looks better:
      #addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      # map location:
      setView(lng=-87.6, lat = 41.85, zoom = 11) %>%                        
      # add some circles:
      addCircles(
        ~Longitude, ~Latitude,
        radius = ~AvgEcoli*3,
        color = ~factpal(Group),
        #label = content2,
        #label = ~as.character(AvgEcoli),
        label = "Click me!",
        popup = content7,
        weight = 5
      ) 
    # addPopups(
    #   ~Longitude, ~Latitude,
    #   content,
    #   options = popupOptions(closeButton = TRUE)
    # )
    
    output$mymap = renderLeaflet(map2)
    
    ##################################################################################################################
    observeEvent(input$slider2, {
      #bar graph for algorithm results:
      #the slider is in percent, but the model summary data is in decimals, so convert the input to match:
      slider_input <- reactive({ (input$slider2) / 100 })
      
      #reorder the model_summary dataframe so that "tpr" is ascending ("thresholds" breaks ties and is descending)
      model_summary_user <- arrange(model_summary, tpr, desc(thresholds))
      
      #returns the index of the value closest to your input:
      index_user <- findInterval(slider_input(), model_summary_user$tpr) 
      
      #use the input's row index to pull model results:
      hits = model_summary_user[index_user, 10]
      false_alarms = model_summary_user[index_user, 13]
      
      #______________________
      
      #reorder the model_summary dataframe so that "tprUSGS" is ascending ("thresholds" breaks ties and is descending)
      model_summary_USGS <- arrange(model_summary, tprUSGS, desc(thresholds))
      
      #returns the index of the value closest to your input:
      index_USGS <- findInterval(slider_input(), model_summary_USGS$tprUSGS) 
      
      USGS_hits = model_summary_USGS[index_USGS, 14]
      USGS_false_alarms = model_summary_USGS[index_USGS, 17]
      
      Model <- c("Your Model", "Your Model", "USGS Model","USGS Model")
      result <- c("Hits", "False_Alarms", "Hits", "False_Alarms")
      result_count <- unlist(c(hits, false_alarms, USGS_hits, USGS_false_alarms))
      subset2 = data.frame(Model, result, result_count)

      #interactive verbiage for results 
      cost <- round(((2000 * false_alarms) / hits), 2) #COST OF A FALSE ALARM NEEDS TO BE UPDATED
      totalcost <- (2000 * false_alarms) #COST OF A FALSE ALARM NEEDS TO BE UPDATED
      output$results_verbiage <- renderText({ paste("In order to achieve a", format(input$slider2, big.mark=",", trim=TRUE), "% hit rate, your model had to call", 
                                                    format(false_alarms, big.mark=",", trim=TRUE), 
                                                    "false alarms during the summer, costing taxpayers $", format(cost, big.mark=",", trim=TRUE), "per hit, or a total of $", 
                                                    format(totalcost, big.mark=",", trim=TRUE), "over the course of the summer.") }) 
      
      output$graph1 <- renderPlot({ggplot(subset2, aes(x=result, y=result_count, fill=Model)) + geom_bar(position="dodge", stat = "identity")+
          theme_bw() + 
          theme(axis.text.x= element_text(angle=-30, hjust=0.05, vjust=1, size=15, family = "Eras")) +
          theme(axis.text.y = element_text(size=15, family = "Eras")) +
          #ggtitle("Model Results") +
          #theme(plot.title=element_text(size=20)) +
          labs(y="Beach Days", x=NULL) +
          scale_fill_brewer(palette="Paired") +
          theme(legend.title=element_text(family="Eras")) +
          theme(axis.title.y=element_text(size=15, family = "Eras")) +
          guides(fill=guide_legend(title=NULL)) +
          theme(legend.text=element_text(family="Eras"))
      })
      # output$yaccuracy <- renderText({paste("Your Accuracy:", accuracy, "%")})
      # output$oaccuracy <- renderText({paste("USGS Accuracy:", USGS_accuracy, "%")})
    })
    ################################################################################################################## 
    
  })

  
  
  
  
  ##################
  #graph 2 (stacked bar graph):
  observeEvent(input$slider, {
  for (i in data16$Escherichia.coli) {
    overthresh = ifelse(data16$Escherichia.coli > input$slider, 1, 0)}
  data16$overthresh <- overthresh
  
  for (i in data16$Escherichia.coli) {
    underthresh = ifelse(data16$Escherichia.coli <= input$slider, 1, 0)}
  data16$underthresh <- underthresh
  
  #make underthresh a factor:
  data16$underthresh <- as.factor(data16$underthresh)
  
  #reorder the columns of the graph based on number of unsafe days:
  data16$Client.ID <- reorder(data16$Client.ID, data16$overthresh, FUN=sum)
  
  #do a group-wise transform, splitting on "Client.ID" (this allows us to create a proportional bar graph)
  newdata <- ddply(data16, "Client.ID", transform, 
                   percent_days = days / sum(days) * 100)
  

  output$graph2 <- renderPlot({ggplot(newdata, aes(x=Client.ID, y=percent_days, fill=underthresh, alpha=.9)) + geom_bar(stat = "identity")+
    theme_bw() + 
    theme(axis.text.x= element_text(angle=-30, hjust=0.05, vjust=1, size=15, family = "Eras")) +
    theme(axis.text.y = element_text(size=15, family = "Eras")) +
    ggtitle("Swimmable Beach Days in 2016") +
    theme(plot.title=element_text(size=20, family = "Eras")) +
    labs(y="Percent of \nBeach Days", x=NULL, fill = "Swimmable?") +
    theme(axis.title.y=element_text(size=15, family = "Eras")) +
    scale_fill_brewer(labels = c("No", "Yes"), palette="Paired") +
    theme(legend.title=element_text(family="Eras")) +
    theme(legend.text=element_text(family="Eras")) +
    guides(alpha=FALSE)
       })
  })
  
  ###########
  #graph 3 (line graphs):
  observeEvent(input$predictor, {
    plot1 <- ggplot(data= linegraphdata, aes(x=Year, y=E_coli)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15, family = "Eras"), axis.text=element_text(size=13, family = "Eras"))
    
    #note: aes_string allows R to tie the input back to the linegraphdata data frame. Have to put Year in quotes when you use aes_string though
    plot2 <- ggplot(data = linegraphdata, aes_string(x="Year", y=input$predictor)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15, family = "Eras"), axis.text=element_text(size=13, family = "Eras"))
    
    output$graph3 <- renderPlot({grid.arrange(plot1, plot2, ncol=1)})
  })
  
  ###########
  #map 2 (for map tab):
  # Plot a default web map 
  mapA <- leaflet::leaflet(lng_lat_df) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
  #customize your map:
  mapB <- mapA %>%
    #use a third-party tile that looks better:
    #addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    # map location:
    setView(lng=-87.6, lat = 41.85, zoom = 11) %>%                        
    # add some circles:
    addCircles(
      ~Longitude, ~Latitude,
      radius = ~AvgEcoli*3,
      color = ~factpal(Group), #networkD3::JS(ColourScale),  
      #label = content6,
      #label = ~as.character(AvgEcoli),
      label = "Click me!",
      popup = content6,
      weight = 5
    ) 
  # addPopups(
  #   ~Longitude, ~Latitude,
  #   content,
  #   options = popupOptions(closeButton = TRUE)
  # )
  
  output$mymap2 = renderLeaflet(mapB)
  
  ############
  #Network Graph
  
  output$force <- renderForceNetwork({
    networkD3::forceNetwork(Links = beachLinks, Nodes = beachNodes, Source = "source",
                            Target = "target", Value = "value", 
                            NodeID = "name",  colourScale = JS(ColourScale),
                            Nodesize = "size", #radiusCalculation = "d.nodesize", #radiusCalculation = " Math.sqrt(d.nodesize)+6",
                            fontFamily = "Arial", fontSize = 20, opacityNoHover = .99,
                            linkDistance = 225, charge = -120, legend = FALSE, clickAction = NULL,
                            # width = 1500, height = 300,
                            Group = "group", opacity = 1, zoom = F, bounded = T)
  })
  
  

}





# Run the application 
shinyApp(ui = ui, server = server)

