#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in Rstudio.
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
library(formattable)
library(shinyBS)
library(reshape2)
#library(scales)
#library(DT)
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

#load fonts for graphs: [NOTE: this causes the app to crash on Shiny's server]
# windowsFonts(Arial=windowsFont("TT Arial"))
# windowsFonts(Times=windowsFont("TT Times New Roman"))
# windowsFonts(Eras=windowsFont("Eras Light ITC"))
# windowsFonts(PR=windowsFont("Poor Richard"))

# Create a palette that maps factor levels to colors for the interactive map:
#                          blue       red        green       pink       orange     purple
factpal <- colorFactor(c("#0288D1", "#B71C1C",  "#229954", "#EC407A", "#F57C00", "#7B1FA2"), lng_lat_df$Group)

#Create label content for interactive map:
content5 <- paste(sep = "",
                  lng_lat_df$Client.ID,
                  " || ",
                  lng_lat_df$AvgEcoliLabel
)

content6 <- paste0("<strong>Beach: </strong>", 
                   lng_lat_df$Beach, 
                   "<br><strong>Avg E. coli level: </strong>", 
                   lng_lat_df$AvgEcoliLabel
)

#define function to color the beaches based on # of false alarms:
getColor <- function(individual_beach_results_reshaped) {
  sapply(individual_beach_results_reshaped$false_alarms, function(false_alarms) {
    if(false_alarms <= 5) {
      "green"
    } else if(false_alarms <= 10) {
      "orange"
    } else {
      "red"
    } })
}

#define function for reshaping the individual beach data from the algorithm:
my_reshape <- function(predictions,thresh)
{ #make binary columns for individual beach outcomes shown in 'predictions' table outputed from the model:
  predictions$false_alarms <- ifelse(predictions$DNAModel.Prediction>thresh & predictions$Escherichia.coli<=235,1,0)
  predictions$hits <- ifelse(predictions$DNAModel.Prediction>thresh &  predictions$Escherichia.coli > 235,1,0)
  predictions$misses <- ifelse(predictions$DNAModel.Prediction<=thresh &  predictions$Escherichia.coli > 235,1,0)
  
  #aggregate outcomes for each beach:
  false_alarms <- aggregate(predictions$false_alarms, by=list(Beach = predictions$Client.ID), FUN =sum )
  hits <- aggregate(predictions$hits, by=list(Beach = predictions$Client.ID), FUN =sum )
  misses <- aggregate(predictions$misses, by=list(Beach = predictions$Client.ID), FUN =sum )
  
  #rename the aggregated columns (because the aggregate function just names it 'x'):
  colnames(false_alarms) <- c("Beach", "false_alarms")
  colnames(hits) <- c("Beach", "hits")
  colnames(misses) <- c("Beach", "misses")
  
  #merge prediction results with the map dataframe. note that non-predicted beaches will get lost in the merge (which is what we want).
  lng_lat_df_results <- merge(lng_lat_df, false_alarms, by="Beach")
  lng_lat_df_results <- merge(lng_lat_df_results, hits, by="Beach")
  lng_lat_df_results <- merge(lng_lat_df_results, misses, by="Beach")
  return(lng_lat_df_results)
}



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
                                         #width = 326.03125,
                                         src = "Map1.png")),
                      wellPanel(tags$h5("The", tags$strong("Beach Days"), "tab allows you to see how the ", tags$i("E. coli"), " cutoff level affects the number 
                                        of swimmable beach days in the summer."),
                                tags$img(height = 203.1061589,
                                         #width = 281.77326289,
                                         src = "BeachDays.PNG"))
                      ),
               column(4,
                      wellPanel(tags$h5("The ", tags$strong("Predictors"), "tab allows you to explore how different elements of the weather and environment
                                        trend with ", tags$i("E. coli"), " levels."),
                                tags$img(height = 239.994,
                                         #width = 286.39284,
                                         src = "Predictors.PNG"))
                      ),
               column(4,
                      wellPanel(tags$h5("The ", tags$strong("Build a Model"), "tab gives you the tools you need to build a model that predicts the ", tags$i("E. coli"), " 
                                        levels at each beach, and see if you're as smart as you think you are."),
                                tags$img(height = 266.0989356,
                                         #width = 308.3321,
                                         src = "Model.PNG")),
                      wellPanel(tags$h5("The ", tags$strong("Network"), "tab describes the relationships that help us predict the ", tags$i("E. coli"), "  levels."),
                                tags$img(height = 245.767189,
                                         #width = 302.205778,
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
           column(4, align="center",
                  tags$img(height = 110.1333,
                           width = 166.4,
                           src = "SmallerChicagoFlag.PNG")
           ),
           column(4, align="center",
                  tags$h1("Can You Predict Better?")
           ),
           column(4, align="center",
                  tags$img(height = 110.1333,
                           width = 166.4,
                           src = "SmallerChicagoFlag.PNG")
           )
         ),
         
         fixedRow(
           column(7, 
                  bsAlert("alert"),
                  leafletOutput('mymap', width = 650, height = 850)
           ),
           
           column(5,
                  div(style = "height:-400px"),
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
                  wellPanel(
                    uiOutput("Rate"),
                    uiOutput("Box2"),
                    actionButton(inputId = "go", label = "Update beaches (~10 sec)"),
                    selectInput("chosen_beaches", label = tags$h5("Select which beaches will be predictive:"), beach_options, multiple=TRUE, selectize=TRUE),
                    tags$h5(htmlOutput("results_verbiage")),
                    tags$h5("Model Results:"),
                    plotOutput("optimization_graph", height="150px"), #, width = "425px"), 
                    plotOutput("graph1", height="150px"), #, width = "425px"),
                    tags$h6(tags$div(HTML(paste(tags$span(tags$b(style="color:green", "Hits:")), "Unsafe beach days caught by the model.", sep = " ")))),
                    tags$h6(tags$div(HTML(paste(tags$span(tags$b(style="color:red", "False Alarms:")), "Safe beach days incorrectly flagged as unsafe, based upon the model.", sep = " "))))
                  ),
                  #########################################################
                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$div("Loading...",id="loadmessage"))
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
  output$Rate = renderUI(selectInput("rate",h5("What are you interested in?"),c("Hit Rate", "False Alarm Rate"),"False Alarm Rate"))
  output$Box2 = renderUI(
    if(input$rate == "Hit Rate") {sliderInput("slider2", label = NULL, min = 1,
                      max = 100, value = 95, post = "%")
    }
    else if(input$rate == "False Alarm Rate") {sliderInput("slider_FA", label = NULL, min = 1,
                                                   max = 10, value = 5, step= 0.2, post = "%")
    }
  )

  observeEvent(input$go, { #everything inside here is tied to the update button ******************************************************
    
    #create an alert if they select 0 beaches:
    if (length(input$chosen_beaches) == 0){
      closeAlert(session, "exampleAlert")
      createAlert(session, "alert", "exampleAlert", title = "Oops",
                  content = "You must select at least one beach!", append = FALSE)
    }
    #create an alert if they select all beaches:
    else if (length(input$chosen_beaches) == 20){
      closeAlert(session, "exampleAlert")
      createAlert(session, "alert", "exampleAlert", title = "Oops",
                  content = "In order to use the predictive model, some beaches must be left out of the sample (so they can be predicted).", 
                  append = FALSE)
    }
    
    #call the function given the input (as long as they selected at between 1 and 19 beaches):
    else {
      #close the "Oops!" pop-up if it's open:
      closeAlert(session, "exampleAlert")
      
      #call the function:
      return_list <- beach_choose(beaches = as.character(input$chosen_beaches),thresh = as.numeric(235),num_of_folds = 3)
      #save the output:
      model_summary <- return_list$model_summary
      individual_beach_results <- return_list$predictions
      
  #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^    
  observeEvent(input$rate, {   
      
      #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ Hit Rate Input $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    if(input$rate == "Hit Rate") {
      #create data for optimization graph (this is static after they hit the "update" button; no need to tie it to the slider):
      
      #reorder the model_summary dataframe so that "tpr" is ascending ("thresholds" breaks ties and is descending)
      model_summary_tpr <- arrange(model_summary, tpr, desc(thresholds))
      
      #get rid of duplicate tpr (hit rates)
      model_summary_tpr = model_summary_tpr[!duplicated(model_summary_tpr$tpr),]

      #convert hit rate to percent:
      model_summary_tpr$tpr <- (model_summary_tpr$tpr *100)
      
      #create a copy for modification:
      model_summary_tpr2 <- model_summary_tpr[, c(1, 2, 10, 13)]
      names(model_summary_tpr2) <- c('thresholds', 'hit_rate', 'hits', 'false_alarms')
      
      #reshape the data for a 2-line graph:
      model_summary_tpr2 <- melt(model_summary_tpr2, id.vars = c('thresholds', 'hit_rate'), variable.name = 'hit_or_falsealarm', value.name = 'beach_days')
      
      
      ##################################################################################################################
      # EVERYTHING HERE UPDATES WHEN THE HIT RATE SLIDER CHANGES:
      observeEvent(input$slider2, { 
        #optimization graph:
        output$optimization_graph <- renderPlot({
          ggplot(model_summary_tpr2, aes(x=hit_rate, y=beach_days, group = hit_or_falsealarm, colour = hit_or_falsealarm)) + 
            geom_line(size= .75) +
            geom_vline(xintercept = input$slider2) + #****THIS IS THE VARIABLE PART OF THE GRAPH (everything else is static)
            scale_colour_manual(values=c("#229954", "#FF0000"), labels = c("Hits", "False Alarms")) +
            theme_bw() +
            labs(x = "Hit Rate", y = "Beach Days") +
            guides(colour=guide_legend(title=NULL)) +
            theme(axis.text.x= element_text(size=15, family = "Eras")) +
            theme(axis.text.y = element_text(size=15, family = "Eras")) +
            theme(axis.title.y=element_text(size=15, family = "Eras")) +
            theme(axis.title.x=element_text(size=15, family = "Eras")) +
            theme(legend.text=element_text(family="Eras", size=15)) +
            theme(legend.position = c(0.25, 0.75))
        })
        
        
        ######### massage data for use in map and verbiage:
        slider_input <- reactive({ (input$slider2) })
        
        #returns the index of the hit rate closest to and lower than user input. if there isn't a lower hit rate, it will crash
        user_index <- findInterval(slider_input(), model_summary_tpr$tpr) 
        
        #fix issue where a low hit rate crashes the app:
        if (user_index == 0){user_index = 1}
        
        #use the input's row index to pull model results:
        HITS = model_summary_tpr[user_index, 10]
        FALSE_ALARMS = model_summary_tpr[user_index, 13]
        threshold_for_map = model_summary_tpr[user_index, 1]
        
        #______________________
        
        
        ##### MAP:
        #use Callin's function to reshape the data for use in the map:
        individual_beach_results_reshaped <- my_reshape(individual_beach_results, as.numeric(threshold_for_map)) #**threshold varies based on the threshold tied to the hit rate chosen in the slider
        
        #Create label content for interactive map:
        content7 <- paste0("<strong>Beach: </strong>", #** these values change when the hit rate changes (because of the corresponding threshold change)
                           individual_beach_results_reshaped$Beach, 
                           "<br><strong>False Alarms you called: </strong>", 
                           individual_beach_results_reshaped$false_alarms,
                           "<br><strong>Unsafe beach days you caught: </strong>", 
                           individual_beach_results_reshaped$hits,
                           "<br><strong>Unsafe beach days you missed: </strong>", 
                           individual_beach_results_reshaped$misses

        )

        # create a default web map 
        map <- leaflet::leaflet(individual_beach_results_reshaped) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
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
            radius = ~(false_alarms*20),  #NOTE: size of circle is based on number of false alarms now, and only PREDICTED (non-selected beaches) will show up
            color = getColor(individual_beach_results_reshaped),
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
        
        
        ##### reshape USGS data for bar graph:
        #reorder the model_summary dataframe so that "tprUSGS" is ascending ("thresholds" breaks ties and is descending)
        model_summary_USGS <- arrange(model_summary, tprUSGS, desc(thresholds))
        
        #convert tprUSGS (USGS hit rate) to percent:
        model_summary_USGS$tprUSGS = ((model_summary_USGS$tprUSGS)*100)
        
        #returns the index of the hit rate closest to and lower than user input. if there isn't a lower hit rate, it will crash
        user_index_USGS <- findInterval(slider_input(), model_summary_USGS$tprUSGS) 
        
        #fix issue where a low hit rate crashes the app:
        if (user_index_USGS == 0){user_index_USGS = 1}
        
        USGS_hits = model_summary_USGS[user_index_USGS, 14]
        USGS_false_alarms = model_summary_USGS[user_index_USGS, 17]
        
        Model <- c("Your Model", "Your Model", "USGS Model","USGS Model")
        result <- c("Hits", "False_Alarms", "Hits", "False_Alarms")
        result_count <- unlist(c(HITS, FALSE_ALARMS, USGS_hits, USGS_false_alarms))
        subset2 = data.frame(Model, result, result_count)
        
        #interactive verbiage for results 
        if (HITS !=0){
        totalcost <- ((FALSE_ALARMS *1500) + (length(input$chosen_beaches) * 150 *100)) # $150 per test * 100 beach days in the summer
        cost <- (totalcost / HITS) 
        output$results_verbiage <- renderText({ paste("In order to achieve a", tags$b(percent((input$slider2 / 100), digits=0)), "hit rate, your model had to call", 
                                                      tags$b(format(FALSE_ALARMS, big.mark=",", trim=TRUE)), 
                                                      "false alarm(s) during the summer, costing taxpayers", tags$b(currency(cost, digits=0)), "per hit, or a total of",  
                                                      tags$b(currency(totalcost, digits=0)), "over the course of the summer.") }) 
        }
        if (HITS ==0){
          totalcost <- (length(input$chosen_beaches) * 150 *100) # $150 per test * 100 beach days in the summer
          output$results_verbiage <- renderText({ paste("In order to achieve a", tags$b(percent((input$slider2 / 100), digits=0)), "hit rate, your model had to call", 
                                                        tags$b(format(FALSE_ALARMS, big.mark=",", trim=TRUE)), 
                                                        "false alarm(s) during the summer, while achieving no hits and costing taxpayers total of",  
                                                        tags$b(currency(totalcost, digits=0)), "over the course of the summer for all the tests run.") }) 
        }
        ###### grouped bar graph for model:
        output$graph1 <- renderPlot({ggplot(subset2, aes(x=Model, y=result_count, fill=result)) + 
            geom_bar(position="dodge", stat = "identity")+
            theme_bw() + 
            guides(fill=FALSE) +
            theme(axis.text.x= element_text(size=15, family = "Eras")) + #angle=-30, hjust=0.05, vjust=1,
            theme(axis.text.y = element_text(size=15, family = "Eras")) +
            #ggtitle("Model Results") +
            #theme(plot.title=element_text(size=20)) +
            labs(y="Beach Days", x=NULL) +
            scale_fill_manual(values=c("#FF0000", "#229954"), labels = c("False Alarms", "Hits")) + 
            #scale_fill_brewer(palette="Paired") +
            #theme(legend.title=element_text(family="Eras")) +
            theme(axis.title.y=element_text(size=15, family = "Eras")) 
          #guides(fill=guide_legend(title=NULL)) +
          #theme(legend.text=element_text(family="Eras")) 
          
          
        }
        #, height = 200, width = 300
        )
        
      }) #this is for the end of the 'observeEvent' for slider2
    } #this is for the if statement for when they want to look at hit rate
      
      #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ False Alarm Rate Input $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      
    else if(input$rate == "False Alarm Rate") {
      
      #create data for false alarm rate optimization graph (this is static after they hit the "update" button; no need to tie it to the slider):
      #reorder the model_summary dataframe so that "fpr" is ascending ("thresholds" breaks ties and is descending)
      model_summary_fpr <- arrange(model_summary, fpr, desc(thresholds))
      head(model_summary_fpr)
      
      #get rid of duplicate fpr (false alarm rates)
      model_summary_fpr = model_summary_fpr[!duplicated(model_summary_fpr$fpr),]
      head(model_summary_fpr)
      model_summary_fpr
      
      #convert false alarm rate to percent:
      model_summary_fpr$fpr <- (model_summary_fpr$fpr *100)
      head(model_summary_fpr)
      
      #create a copy for modification:
      model_summary_fpr2 <- model_summary_fpr[, c(1, 3, 10, 13)]
      names(model_summary_fpr2) <- c('thresholds', 'false_alarm_rate', 'hits', 'false_alarms')
      head(model_summary_fpr2)
      
      #reshape the data for a 2-line graph:
      library(reshape2)
      model_summary_fpr2 <- melt(model_summary_fpr2, id.vars = c('thresholds', 'false_alarm_rate'), variable.name = 'hit_or_falsealarm', value.name = 'beach_days')
      head(model_summary_fpr2)
      
      ##################################################################################################################
      # EVERYTHING HERE UPDATES WHEN THE FALSE ALARM RATE SLIDER CHANGES:
      observeEvent(input$slider_FA, { 
        #make a new dataset that excludes rows where false alarm rate > 10:
        model_summary_fpr3 <- model_summary_fpr2[!rowSums(model_summary_fpr2[2] > 10),]
        #optimization graph:
        output$optimization_graph <- renderPlot({
          ggplot(model_summary_fpr3, aes(x=false_alarm_rate, y=beach_days, group = hit_or_falsealarm, colour = hit_or_falsealarm)) + 
            geom_line(size= .75) +
            geom_vline(xintercept = input$slider_FA) + #****THIS IS THE VARIABLE PART OF THE GRAPH (everything else is static)
            scale_colour_manual(values=c("#229954", "#FF0000"), labels = c("Hits", "False Alarms")) +
            theme_bw() +
            labs(x = "False Alarm Rate", y = "Beach Days") +
            guides(colour=guide_legend(title=NULL)) +
            theme(axis.text.x= element_text(size=15, family = "Eras")) +
            theme(axis.text.y = element_text(size=15, family = "Eras")) +
            theme(axis.title.y=element_text(size=15, family = "Eras")) +
            theme(axis.title.x=element_text(size=15, family = "Eras")) +
            theme(legend.text=element_text(family="Eras", size=15)) +
            theme(legend.position = c(0.25, 0.75))
        })
        
        
        ######### massage data for use in map and verbiage:
        slider_inputFA <- reactive({ (input$slider_FA) })
        
        #returns the index of the false alarm rate closest to and lower than user input. if there isn't a lower false alarm rate, it will crash
        user_index <- findInterval(slider_inputFA(), model_summary_fpr$fpr) 
        
        #fix issue where a low false alarm rate crashes the app:
        if (user_index == 0){user_index = 1}
        
        #use the input's row index to pull model results:
        HITS = model_summary_fpr[user_index, 10]
        FALSE_ALARMS = model_summary_fpr[user_index, 13]
        MISSES = model_summary_fpr[user_index, 11]
        threshold_for_map = model_summary_fpr[user_index, 1]
        HIT_RATE = model_summary_fpr[user_index, 2]
        
        #______________________
        #interactive verbiage for results 
        if (HITS !=0){
          totalcost <- ((FALSE_ALARMS *1500) + (length(input$chosen_beaches) * 150 *100)) # $150 per test * 100 beach days in the summer
          cost <- (totalcost / HITS) 
          output$results_verbiage <- renderText({ paste("With a", tags$b(percent((input$slider_FA / 100), digits=0)), "false alarm rate, your model had to call", 
                                                        tags$b(format(FALSE_ALARMS, big.mark=",", trim=TRUE)), 
                                                        "false alarm(s) during the summer, costing taxpayers", tags$b(currency(cost, digits=0)), "per hit, or a total of",  
                                                        tags$b(currency(totalcost, digits=0)), "over the course of the summer. With the corresponding",
                                                        tags$b(percent((HIT_RATE), digits=0)), " hit rate, you missed",
                                                        tags$b(format(MISSES, big.mark=",", trim=TRUE)), "unsafe beach days, putting Chicagoans at risk.") }) 
        }
        if (HITS ==0){
          totalcost <- (length(input$chosen_beaches) * 150 *100) # $150 per test * 100 beach days in the summer
          output$results_verbiage <- renderText({ paste("With a", tags$b(percent((input$slider_FA / 100), digits=0)), "false alarm rate, your model had to call", 
                                                        tags$b(format(FALSE_ALARMS, big.mark=",", trim=TRUE)), 
                                                        "false alarm(s) during the summer, while achieving no hits and costing taxpayers total of",  
                                                        tags$b(currency(totalcost, digits=0)), "over the course of the summer for all the tests run. You missed",
                                                        tags$b(format(MISSES, big.mark=",", trim=TRUE)), "unsafe beach days, putting Chicagoans at risk.") }) 
        }
        
        ##### MAP:
        #use Callin's function to reshape the data for use in the map:
        individual_beach_results_reshaped <- my_reshape(individual_beach_results, as.numeric(threshold_for_map)) #**threshold varies based on the threshold tied to the hit rate chosen in the slider
        
        #Create label content for interactive map:
        content7 <- paste0("<strong>Beach: </strong>", #** these values change when the hit rate changes (because of the corresponding threshold change)
                           individual_beach_results_reshaped$Beach, 
                           "<br><strong>False Alarms you called: </strong>", 
                           individual_beach_results_reshaped$false_alarms,
                           "<br><strong>Unsafe beach days you caught: </strong>", 
                           individual_beach_results_reshaped$hits,
                           "<br><strong>Unsafe beach days you missed: </strong>", 
                           individual_beach_results_reshaped$misses
                           
        )
        
        # create a default web map 
        map <- leaflet::leaflet(individual_beach_results_reshaped) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
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
            radius = ~(false_alarms*20),  #NOTE: size of circle is based on number of false alarms now, and only PREDICTED (non-selected beaches) will show up
            color = getColor(individual_beach_results_reshaped),
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
        
        
        ##### reshape USGS data for grouped bar graph:
        #reorder the model_summary dataframe so that "fprUSGS" is ascending ("thresholds" breaks ties and is descending)
        model_summary_USGS <- arrange(model_summary, fprUSGS, desc(thresholds))
        
        #convert fprUSGS (USGS false alarm rate) to percent:
        model_summary_USGS$fprUSGS = ((model_summary_USGS$fprUSGS)*100)
        
        #returns the index of the false alarm rate closest to and lower than user input. if there isn't a lower hit rate, it will crash
        user_index_USGS <- findInterval(slider_inputFA(), model_summary_USGS$fprUSGS) 
        
        #fix issue where a low hit rate crashes the app:
        if (user_index_USGS == 0){user_index_USGS = 1}
        
        USGS_hits = model_summary_USGS[user_index_USGS, 14]
        USGS_false_alarms = model_summary_USGS[user_index_USGS, 17]
        
        Model <- c("Your Model", "Your Model", "USGS Model","USGS Model")
        result <- c("Hits", "False_Alarms", "Hits", "False_Alarms")
        result_count <- unlist(c(HITS, FALSE_ALARMS, USGS_hits, USGS_false_alarms))
        subset2 = data.frame(Model, result, result_count)
        
        ###### grouped bar graph for model:
        output$graph1 <- renderPlot({ggplot(subset2, aes(x=Model, y=result_count, fill=result)) + 
            geom_bar(position="dodge", stat = "identity")+
            theme_bw() + 
            guides(fill=FALSE) +
            theme(axis.text.x= element_text(size=15, family = "Eras")) + #angle=-30, hjust=0.05, vjust=1,
            theme(axis.text.y = element_text(size=15, family = "Eras")) +
            #ggtitle("Model Results") +
            #theme(plot.title=element_text(size=20)) +
            labs(y="Beach Days", x=NULL) +
            scale_fill_manual(values=c("#FF0000", "#229954"), labels = c("False Alarms", "Hits")) + 
            #scale_fill_brewer(palette="Paired") +
            #theme(legend.title=element_text(family="Eras")) +
            theme(axis.title.y=element_text(size=15, family = "Eras")) 
          #guides(fill=guide_legend(title=NULL)) +
          #theme(legend.text=element_text(family="Eras")) 
          
          
        }
        #, height = 200, width = 300
        )
        
      }) #these brackets are for the observeEvent sliderFA
      ################################################################################################################## 
    } #this bracket is for if they selected False Alarm Rate
  }) #these brackets are for the observeEvent input$rate
 } #this bracket is for the 'else' statement when they have selected beaches properly

    
  }) #these brackets are for the observeEvent go button
  
  
  
  
  
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
    
    #stacked bar graph
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

