#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#NOTE:North Avenue and Oak Street have been removed as options because something is wrong with them in the algorithm code

library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)

#if running locally, uncomment the next line and paste in the path to this folder:
#setwd("~/School/DePaul/3. Winter 2017/CSC 465/My Polished App")
source("30_ModelFunc.R")

data16 = read.csv("data16.csv")

linegraphdata = read.csv("linegraphdata.csv")

#list all possible options for the selection menus:
beach_options = c("12th","31st","57th", "63rd", "Albion", "Calumet", "Foster", "Howard", "Jarvis", "Juneway","Leone", "Montrose", "Ohio", "Osterman", "Rainbow", "Rogers", "South Shore", "39th")
predictor_options <- c("Water_Temperature", "Dew_Point", "Humidity", "Rain_Intensity", "Wind_Speed",
                       "Barometric_Pressure", "Visibility", "Cloud_Cover")




# Define UI for application that draws graphs
ui <- fluidPage(tabsetPanel(
  
  tabPanel("Home", 
           fluidRow(
             column(8, offset=1,
                    tags$h1("Welcome!"),
                    tags$h4("This is an interactive site for understanding the ", tags$i("E. coli"), " levels at your beaches in Chicago.
                            Please vist the tabs above to access different interactive features.")
                     )
                  ),

           fluidRow(
             column(12, offset=2, tags$img(height = 413,
                                           width = 624,
                                           src = "SmallerChicagoFlag.PNG")
                                          ),
             column(12, offset=1, tags$h4("__________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("__________________________________________________________________________________________________")
                    )
                   )
  ),
           
           
  
  tabPanel("Build a Model",
  fluidRow(column(12, offset=0, tags$h1("Can you predict better?"))
           ),
  
  fluidRow(
    column(8, offset=0,
                tags$h5("The US Geological Survey (USGS) collects water samples from each beach, and grows the samples in the lab.
                        This is how we know the", tags$i("E. coli"), "levels in the water. However, this test takes 12 hours to run, and can be costly. By the time the
                        test is complete, the beach day is over."),
               tags$h5("In order to inform the public whether the water is safe for swimming, the City of Chicago uses a combination of the", tags$i("E. coli"), "levels
                       from the previous days, as well as environmental factors to", tags$b("predict"), "the current", tags$i("E. coli"), "levels."),
                tags$h5("Another important factor in predicting ", tags$i("E. coli"), " levels is using the beaches that behave similarly to predict each other. 
                        The beaches you select will be entered into an algorithm, which will then be used to 
                       create a predictive model. After you hit the Update button, give the algorithm ", tags$b("30 seconds"), " to run,
                        and your results will populate."),
               tags$h5("After selecting beaches and hitting update, the graph that populates below will show you how well your model performed compared
                       to the model used by the City of Chicago."), 
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
               verbatimTextOutput("yaccuracy"), 
               verbatimTextOutput("oaccuracy"),
               plotOutput("graph1"),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage")),
               tags$h6("Hits: Unsafe beach days caught by the model."), 
               tags$h6("Misses: Unsafe beach days not caught by the model."), 
               tags$h6("False Alarms: Safe beach days incorrectly flagged as unsafe, based upon the model."), 
               tags$h6("Correct Rejections: Safe beach days correctly flagged as safe, based upon the model.")
            ),
             column(3, wellPanel(checkboxGroupInput("chosen_beaches", "Select which beaches will be predictive:", beach_options),
                           actionButton(inputId = "go", label = "Update (~30 sec)"))
                     )
           ),
          fluidRow(
             column(12, offset=0, tags$h4("__________________________________________________________________________________________________"),
                    tags$h5("The algorithm used in this app was built by some very bright folks working for
                            the City of Chicago, including Callin Osborn and his team."),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("__________________________________________________________________________________________________")
                    )
                    )
  ),
  
  
  
  
  
  tabPanel("Predictors",
           fluidRow(
             column(12, offset=0, tags$h1("Pick a Predictor:")
             ),
             column(12, offset=0, tags$h4("Which elements of the environment trend with ", tags$i("E. coli"), " levels? Data scientists use 
                                         elements that trend together to predict each other. Look for 
                                          the predictor whose line graph peaks and valleys with the average", tags$i("E. coli"), "levels.")
             ),
             column(4, offset=3, 
                    wellPanel(selectInput("predictor", "Select a Predictor", choices = predictor_options
                                          )
                              )
             ),
             column(10, 
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
           fluidRow(
             column(12, offset=0, tags$h4("__________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("__________________________________________________________________________________________________")
             )
           )
           ),
  
  
  
  
  
  
  tabPanel("Beach Days",
           fluidRow(
             column(12, offset=0, tags$h1("Set your own limit:")
             ),
             column(12, offset=0, tags$h4("The City of Chicago sets the", tags$i("E. coli"), "cutoff at no greater than 235 Colony Forming Units per 100 mL. This is a level reached 
                                          in collaboration with microbiologists to ensure the safety of the public. Move the slider bar below to see 
                                          how many swimmable beach days there are at different cutoff levels.")
             ),
             column(4, offset=3, 
                    wellPanel(sliderInput("slider", label = h5("", tags$i("E. coli"), "cutoff (in CFU/100mL)"), min = 1, 
                                          max = 2415, value = 235
                    ))
             ),
             column(10, 
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
           fluidRow(
             column(12, offset=0, tags$h4("__________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("__________________________________________________________________________________________________")
                    )
           )
  ),
  
  
  
  
  
  tabPanel("Map",
           fluidRow(
             column(12, offset=0, tags$h1("Which Beaches Behave Similarly?")
             ),
             column(12, offset=0,
                    tags$h4("Below, you can see which beaches have", tags$i("E. coli"), "levels that fluctuate together. By understanding these
                            relationships, data scientists can use only one beach out of a group to predict the ", tags$i("E. coli"), " levels at the other beaches
                            in that group. This means that scientists only need to collect ", tags$i("E. coli"), " samples from one beach in the group, which can eliminate
                            unnecessary spending."),
                    column(12, offset=0,tags$img(height = 751,
                                                 width = 882,
                                                 src = "Map2.png")
                    )
                    )
  ),
  fluidRow(
    column(12, offset=0, tags$h4("__________________________________________________________________________________________________"),
           tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
           #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
           tags$h4("__________________________________________________________________________________________________")
    )
  )
),




    tabPanel("Network",
      fluidRow(
        column(12, offset=0, tags$h1("A Network of Beaches:")
               ),
        column(12, offset=0,
               tags$h4("Below, you can see which beaches have similar", tags$i("E. coli"), "levels. The size of the circle represents the average", 
                       tags$i("E. coli"), "level at that beach. Each color represents a group that fluctuates together.
                       The thickness of each line represents the strength of the connection between the", tags$i("E. coli"), "levels at those 2 beaches.
                       Understanding these relationships is a crucial part of predicting ", tags$i("E. coli"), " levels at each beach."),
        column(12, offset=1,tags$img(height = 468,
                                     width = 877,
                                     src = "Network_Graph.png")
               )
         )
      ),
      fluidRow(
        column(12, offset=0, tags$h4("__________________________________________________________________________________________________"),
               tags$h5("The network graph was built by Norie Kauffman and Don Crowley."),
               tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
               #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
               tags$h4("__________________________________________________________________________________________________")
               )
      )
      )

  
  
  
  )
)
    
  

    




# Define server logic required to draw graphs
server <- function(input, output,session) {

  ####################
  #graph 1:
  observeEvent(input$go, {
    model_summary <- beach_choose(beaches = as.character(input$chosen_beaches)) #calls thhe function given the input, and returns the output as model_summary
    hits = model_summary[235, 10]
    misses = model_summary[235, 11]
    correct_rejections = model_summary[235, 12]
    false_alarms = model_summary[235, 13]
    accuracy = ((hits + correct_rejections) / (hits + misses + correct_rejections + false_alarms)) *100
    accuracy <- as.integer(accuracy)
    accuracy <- round(accuracy, 0)
  
    USGS_hits = model_summary[235, 14]
    USGS_misses = model_summary[235, 15]
    USGS_correct_rejections = model_summary[235, 16]
    USGS_false_alarms = model_summary[235, 17]
    USGS_accuracy = ((USGS_hits + USGS_correct_rejections) / (USGS_hits + USGS_misses + USGS_correct_rejections + USGS_false_alarms)) *100
    USGS_accuracy <- as.integer(USGS_accuracy)
    USGS_accuracy <- round(USGS_accuracy, 0)
    
    Model <- c("Your Model", "Your Model", "Your Model", "Your Model", "USGS Model","USGS Model","USGS Model","USGS Model")
    result <- c("Hits", "Misses", "False_Alarms", "Correct_Rejections", "Hits", "Misses", "False_Alarms", "Correct_Rejections")
    result_count <- unlist(c(hits, misses, false_alarms, correct_rejections, USGS_hits, USGS_misses, USGS_false_alarms, USGS_correct_rejections))
    subset2 = data.frame(Model, result, result_count)
    #subset2$result <- reorder(subset2$result, subset2$result_count, FUN=mean)
  
    output$graph1 <- renderPlot({ggplot(subset2, aes(x=result, y=result_count, fill=Model)) + geom_bar(position="dodge", stat = "identity")+
      theme_bw() + 
      theme(axis.text.x= element_text(angle=-30, hjust=0.05, vjust=1, size=15)) +
      theme(axis.text.y = element_text(size=15)) +
      ggtitle("Your Results vs. USGS Results") +
      theme(plot.title=element_text(size=20)) +
      labs(y=NULL, x=NULL) +
      scale_fill_brewer(palette="Paired")
      })
    output$yaccuracy <- renderText({paste("Your Accuracy:", accuracy, "%")})
    output$oaccuracy <- renderText({paste("USGS Accuracy:", USGS_accuracy, "%")})

  })
  ##################
  #graph 2:
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
  
  output$graph2 <- renderPlot({ggplot(newdata, aes(x=Client.ID, y=percent_days, fill=underthresh)) + geom_bar(stat = "identity")+
    theme_bw() + 
    theme(axis.text.x= element_text(angle=-30, hjust=0.05, vjust=1, size=15)) +
    theme(axis.text.y = element_text(size=15)) +
    ggtitle("Swimmable Beach Days in 2016") +
    theme(plot.title=element_text(size=20)) +
    labs(y="Percent of \nBeach Days", x=NULL, fill = "Swimmable?") +
    theme(axis.title.y=element_text(size=15)) +
    scale_fill_brewer(labels = c("No", "Yes"), palette="Paired")
       })
  })
  
  ###########
  #graph 3:
  observeEvent(input$predictor, {
    plot1 <- ggplot(data= linegraphdata, aes(x=Year, y=E_coli)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15), axis.text=element_text(size=13))
    
    #note: aes_string allows R to tie the input back to the linegraphdata data frame. Have to put Year in quotes when you use aes_string though
    plot2 <- ggplot(data = linegraphdata, aes_string(x="Year", y=input$predictor)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15), axis.text=element_text(size=13))
    
    output$graph3 <- renderPlot({grid.arrange(plot1, plot2, ncol=1)})
  })

}





# Run the application 
shinyApp(ui = ui, server = server)

