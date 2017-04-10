#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(leaflet)
library(shinythemes)

source("30_ModelFunc.R")

#import datasets needed:
data16 = read.csv("data16.csv")
linegraphdata = read.csv("linegraphdata.csv")
lng_lat_df = read.csv("mapdata.csv")

#list all possible options for the selection menus:
beach_options = c("12th","31st","57th", "63rd", "Albion", "Calumet", "Foster", "Howard", "Jarvis", "Juneway","Leone", "Montrose","North Avenue", "Oak Street", "Ohio", "Osterman", "Rainbow", "Rogers", "South Shore", "39th")
predictor_options <- c("Water_Temperature", "Dew_Point", "Humidity", "Rain_Intensity", "Wind_Speed",
                       "Barometric_Pressure", "Visibility", "Cloud_Cover")



# Create a palette that maps factor levels to colors for the interactive map:
pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))
factpal <- colorFactor(c("blue", "red", "green", "purple", "orange", "maroon3" ), lng_lat_df$Group)

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
ui <- fluidPage(
  #to test out themes, uncomment next line and comment out the line below it.
  #shinythemes::themeSelector(), 
  theme = shinytheme("yeti"),
  tabsetPanel(
  
  tabPanel("Home", 
           fluidRow(
             column(2, offset = 1,
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
                    ),
             column(2,
                    tags$h1("Welcome!")
             ),
             column(3,
                    tags$img(height = 110.1333,
                             width = 166.4,
                             src = "SmallerChicagoFlag.PNG")
                   )
             ),
             fluidRow(
               column(9, 
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

           fluidRow(
             column(3,
                    wellPanel(tags$h5("The ", tags$strong("Map"), " tab describes the beach locations and relationships."),
                    tags$img(height = 152.33,
                             width = 208.66,
                             src = "Map1.png")),
                    wellPanel(tags$h5("The", tags$strong("Beach Days"), "tab allows you to see how the ", tags$i("E. coli"), " cutoff level affects the number 
                            of swimmable beach days in the summer."),
                    tags$img(height = 152.33,
                             width = 211.33,
                             src = "BeachDays.PNG"))
                    ),
             column(3,
                    wellPanel(tags$h5("The ", tags$strong("Predictors"), "tab allows you to explore how different elements of the weather and environment
                            trend with ", tags$i("E. coli"), " levels."),
                    tags$img(height = 180,
                             width = 214.8,
                             src = "Predictors.PNG"))
                    ),
             column(3,
                    wellPanel(tags$h5("The ", tags$strong("Build a Model"), "tab gives you the tools you need to build a model that predicts the ", tags$i("E. coli"), " 
                            levels at each beach, and see if you're as smart as you think you are."),
                    tags$img(height = 159.66,
                             width = 185,
                             src = "Model.PNG")),
                    wellPanel(tags$h5("The ", tags$strong("Network"), "tab describes the relationships that help us predict the ", tags$i("E. coli"), "  levels."),
                    tags$img(height = 184.33,
                             width = 226.66,
                             src = "Network.PNG"))
                    )
           ),
           fluidRow(
             column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("______________________________________________________________________________________________________________")
                    )
                   )
  ),
  
  
  
  
  
  
  tabPanel("Map",
           fluidRow(
             column(12, offset=0, tags$h1("Which Beaches Behave Similarly?")
             )
           ),
           fluidRow(
             column(6, offset=0, leafletOutput('mymap2', width = 1700, height = 700)
             ),
             column(4,
                    tags$h5("Each beach is represented on the map as a circle, and they are color-coded to show which beaches have ", tags$i("E. coli"), " 
                         levels that fluctuate together. The size of the circle represents the average ", tags$i("E. coli"), " level at that beach."),
                    tags$h5("By understanding these
                            relationships, data scientists can use only one beach out of a group to predict the ", tags$i("E. coli"), " levels at the other beaches
                            in that group. This means that scientists only need to collect ", tags$i("E. coli"), " samples from one beach in the group, which can eliminate
                            unnecessary spending.")
                    )
                    ),
           fluidRow(
             column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("______________________________________________________________________________________________________________")
             )
           )
  ),
  
  
  
  
  
  
  
  tabPanel("Beach Days",
           fluidRow(
             column(12, offset=0, tags$h1("Set your own limit:")
             )
           ),
           fluidRow(
             column(12,
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
             column(8, 
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
             column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("______________________________________________________________________________________________________________")
             )
           )
           ),
  
  
  
  
  
  
  tabPanel("Predictors",
           fluidRow(
             column(12, offset=0, tags$h1("Pick a Predictor:")
             ),
             column(12, 
                    absolutePanel(
                      bottom = -300, right = 100, width = 300,
                      draggable = TRUE,
                      wellPanel(
                        tags$h5(
                          "Which elements of the environment trend with ", tags$i("E. coli"), " levels? Data scientists use 
                          elements that trend together to predict each other. Look for 
                          the predictor whose line graph peaks and valleys with the average", tags$i("E. coli"), "levels."
                          ),
                        selectInput("predictor", tags$h5("Select a Predictor:"), choices = predictor_options)
                        ),
                      style = "opacity: 0.92"
                      )
             ),
             column(8, 
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
             column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("______________________________________________________________________________________________________________")
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
    column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
           tags$h5("The network graph was built by Norie Kauffman and Don Crowley."),
           tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
           #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
           tags$h4("______________________________________________________________________________________________________________")
    )
  )
),
  
  
  
  
           
           
  
  tabPanel("Build a Model",
  fluidRow(column(12, offset=0, tags$h1("Can you predict better?"))
           ),
  
  fluidRow(
    column(8, offset=0,
                tags$h5("The beaches you select will be entered into an algorithm, which will then be used to 
                       create a predictive model. After you hit the Update button, give the algorithm ", tags$b("10 seconds"), " to run,
                        and your results will populate."),
               tags$h5("After selecting beaches and hitting update, the interactive map that populates below will show you how your model did at 
              each beach. The graph that populates below, on the right, will show you how well your model performed compared
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
               # verbatimTextOutput("yaccuracy"), 
               # verbatimTextOutput("oaccuracy"),
           ###########################################################
           absolutePanel(
             bottom = -475, right = -350, width = 350,
             draggable = TRUE,
             wellPanel(
               sliderInput("slider2", label = h6("", tags$i("E. coli"), "cutoff (in CFU/100mL)"), min = 1, 
                           max = 500, value = 235
               ),
               checkboxGroupInput("chosen_beaches", "Select which beaches will be predictive:", beach_options),
               actionButton(inputId = "go", label = "Update (~10 sec)"),
               tags$h5("Model Results:"),
               plotOutput("graph1")
               ),
             style = "opacity: 0.92"
               ),
           ##########################################################
   
           leafletOutput('mymap'),
               conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                tags$div("Loading...",id="loadmessage")),
               tags$h6("Hits: Unsafe beach days caught by the model."), 
               tags$h6("Misses: Unsafe beach days not caught by the model."), 
               tags$h6("False Alarms: Safe beach days incorrectly flagged as unsafe, based upon the model."), 
               tags$h6("Correct Rejections: Safe beach days correctly flagged as safe, based upon the model.")
            )
           ),
          fluidRow(
             column(12, offset=0, tags$h4("______________________________________________________________________________________________________________"),
                    tags$h5("The algorithm used in this app was built by some very bright folks working for
                            the City of Chicago, including Callin Osborn and his team."),
                    tags$h5("This Shiny app was built by Renel Chesak. For contact information, please visit her profile on LinkedIn:", tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak"), ""),
                    #column(12, offset=4, tags$a(href = "https://www.linkedin.com/in/renel-chesak-541067a1/", "linkedin.com/in/renel-chesak")),
                    tags$h4("______________________________________________________________________________________________________________")
                    )
                    )
  )

  
  )
  
  )

    
  

    




# Define server logic required to draw graphs
server <- function(input, output,session) {

  ####################
  #graph 1 and interactive map:
  observeEvent(input$go, {
    #map:
    # Plot a default web map 
    map <- leaflet(lng_lat_df) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
    #customize your map:
    map2 <- map %>%
      #use a third-party tile that looks better:
      #addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      # map location:
      setView(lng=-87.6, lat = 41.85, zoom = 10) %>%                        
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
    
   
    model_summary <- beach_choose(beaches = as.character(input$chosen_beaches),thresh = as.numeric(input$slider2),num_of_folds = 3) #calls the function given the input, and returns the output as model_summary
    
    
    #bar graph for algorithm results:
    
    hits = model_summary[input$slider2, 10]
    misses = model_summary[input$slider2, 11]
    correct_rejections = model_summary[input$slider2, 12]
    false_alarms = model_summary[input$slider2, 13]
    
    USGS_hits = model_summary[input$slider2, 14]
    USGS_misses = model_summary[input$slider2, 15]
    USGS_correct_rejections = model_summary[input$slider2, 16]
    USGS_false_alarms = model_summary[input$slider2, 17]
    
    #______________________________________________
    
    # hits = model_summary[235, 10]
    # misses = model_summary[235, 11]
    # correct_rejections = model_summary[235, 12]
    # false_alarms = model_summary[235, 13]
    # 
    # USGS_hits = model_summary[235, 14]
    # USGS_misses = model_summary[235, 15]
    # USGS_correct_rejections = model_summary[235, 16]
    # USGS_false_alarms = model_summary[235, 17]
    
    # accuracy = ((hits + correct_rejections) / (hits + misses + correct_rejections + false_alarms)) *100
    # accuracy <- as.integer(accuracy)
    # accuracy <- round(accuracy, 0)
    # USGS_accuracy = ((USGS_hits + USGS_correct_rejections) / (USGS_hits + USGS_misses + USGS_correct_rejections + USGS_false_alarms)) *100
    # USGS_accuracy <- as.integer(USGS_accuracy)
    # USGS_accuracy <- round(USGS_accuracy, 0)
    
    Model <- c("Your Model", "Your Model", "Your Model", "Your Model", "USGS Model","USGS Model","USGS Model","USGS Model")
    result <- c("Hits", "Misses", "False_Alarms", "Correct_Rejections", "Hits", "Misses", "False_Alarms", "Correct_Rejections")
    result_count <- unlist(c(hits, misses, false_alarms, correct_rejections, USGS_hits, USGS_misses, USGS_false_alarms, USGS_correct_rejections))
    subset2 = data.frame(Model, result, result_count)
    #subset2$result <- reorder(subset2$result, subset2$result_count, FUN=mean)
  
    output$graph1 <- renderPlot({ggplot(subset2, aes(x=result, y=result_count, fill=Model)) + geom_bar(position="dodge", stat = "identity")+
      theme_bw() + 
      theme(axis.text.x= element_text(angle=-30, hjust=0.05, vjust=1, size=15)) +
      theme(axis.text.y = element_text(size=15)) +
        
      #ggtitle("Model Results") +
      theme(plot.title=element_text(size=20)) +
      labs(y=NULL, x=NULL) +
      scale_fill_brewer(palette="Paired")
      })
    # output$yaccuracy <- renderText({paste("Your Accuracy:", accuracy, "%")})
    # output$oaccuracy <- renderText({paste("USGS Accuracy:", USGS_accuracy, "%")})

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
  #graph 3 (line graphs):
  observeEvent(input$predictor, {
    plot1 <- ggplot(data= linegraphdata, aes(x=Year, y=E_coli)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15), axis.text=element_text(size=13))
    
    #note: aes_string allows R to tie the input back to the linegraphdata data frame. Have to put Year in quotes when you use aes_string though
    plot2 <- ggplot(data = linegraphdata, aes_string(x="Year", y=input$predictor)) + geom_line() + theme_bw() +
      theme(axis.title=element_text(size=15), axis.text=element_text(size=13))
    
    output$graph3 <- renderPlot({grid.arrange(plot1, plot2, ncol=1)})
  })
  
  ###########
  #map 2 (for map tab):
  # Plot a default web map 
  mapA <- leaflet(lng_lat_df) %>% addTiles() #the add tiles argument breaks the map into tiles so it's not so hard to hold it in memory
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
      color = ~factpal(Group),
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

}





# Run the application 
shinyApp(ui = ui, server = server)

