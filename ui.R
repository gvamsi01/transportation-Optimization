#calling the libraries which are required for analysis
library(shiny)
library(leaflet)
library(ggmap)
library(dplyr)
library(lubridate)
library(shinythemes)
library(plotly)
library(devtools)

shinyUI(
  navbarPage(p(h4(tags$i(strong("Project Leo Visualization"))),style = "color:green"),
             theme = shinytheme("cosmo"),
             tabPanel(p(tags$i(strong("Traveller Travelling Movement"))),
                      #generating the side bar panel for visualization
                      tags$head(
                        tags$style(HTML("
                                        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                        
                                        h1 {
                                        font-family: 'Lobster', cursive;
                                        font-weight: 500;
                                        line-height: 1.1;
                                        color: #48ca3b;
                                        }
                                        
                                        ")),
                        includeScript("spin.min.js")
                        ), #ending for tag heading 
                      
                      #headerPanel("Leaflet Mapping"),
                      sidebarLayout(
                        sidebarPanel(width = 3, p(h5(strong("User Inputs for Mapping"))),
                                     wellPanel(
                                       p(class="intro", "Explore traveller travelling patterns from one place to another using this interactive visualisation"),
                                       tabsetPanel(
                                         tabPanel("Controls",
                                                  dateInput("date","Choose your date",value = "2012-04-15",min = "2012-04-15",max = Sys.time()),
                                                  textInput("loc1","Enter your 1st Location"),
                                                  numericInput("rad_1","Enter zone radius for 1st Location",value=100),
                                                  textInput("loc2","Enter your 2nd Location"),
                                                  numericInput("rad_2","Enter zone radius for 2nd Location",value=200,min = 0,max = 3000),
                                                  sliderInput("hour","Choose your Hour",min = 0,max = 23,value = 12,step = 1),
                                                  #sliderInput("slide","Choose Time Interval",min = 0,max = 59,value = c(10,30),step = 1),
                                                  submitButton("Submit"),
                                                  hr(class="thin"),
                                                  p("Development by",a("@Vamsi Grandhi", href="https://sg.linkedin.com/in/vamsi-grandhi-6ba95020",target="_blank"),
                                                    "from",a("@National University of Singapore",href="http://www.nus.edu.sg/",target="_blank"))
                                                  ), #ending of control tabpanel
                                         
                                         #Generating Visual Info tab
                                         tabPanel("Visualization Info",br(),
                                                  p(strong("All Status Tab")),
                                                  p("The clustered number represents no of cabs starting and passing through that locations.",
                                                    "Markers with different shapes represent different status of cabs"), br(),
                                                  p(strong("Source & Destination tab")),
                                                  p("This tab shows a granular level analysis between two locations. The user specified first location",
                                                    "is considered as a source and reflects a unique number representing number of travellers starting from",
                                                    "that location and reaching their destination in the designated hour"), br(),
                                                  p(strong("Marker Information")),
                                                  p(img(src="home_red.jpg",width=50,height=50),
                                                    img(src="home_blue.jpg",width=50,height=50),align="center"),
                                                  p("represents",strong(span("destination",style="color:blue")),"of commuter and status",
                                                    strong("PAYMENT",style="color:red")),
                                                  br(),
                                                  p(img(src="flag.jpg",width=50,height=50),"represents",strong(span("source",style="color:blue")),
                                                    "of commuter and status",strong("POB",style="color:blue"),align="center"),
                                                  br(),
                                                  p(img(src="Busy.jpg",width=50,height=50),"represents the other status of the cab like",span("BUSY, FREE, 
                                                  ONCALL, SHIFT CHANGE",style="color:blue"),align="center")
                                                  ),
                                         
                                         #generating the About tabpanel
                                         tabPanel("Software Info", br(),
                                                  p("Project Leo is written in ",a("Shiny,", href="http://shiny.rstudio.com/", target="_blank"),
                                                    "a web application framework using R language.","Maps are built with ",
                                                    a("leaflet.js", href="http://leafletjs.com/", target="_blank"),"via the",
                                                    a("R language bindings,", href="https://rstudio.github.io/leaflet/",
                                                      target="_blank"),"and using map data from",
                                                    a("Open Street Map.", href="http://www.openstreetmap.org/copyright",
                                                      target="_blank"),"and",
                                                    a("Carto DB",href="https://cartodb.com/",target="_blank")
                                                  ), br(),
                                                  p(strong("R Language"),img(src="R.jpg",width=50,height=50)),
                                                  p(a("R Language"),"is an open source software currently being used by statisticans, GIS Analyts",
                                                    "Data scientits to handle astronomical data for analysis"),br(),
                                                  p(strong("Leaflet"), img(src="Leaflet.jpg",width=150,height=50)),
                                                  p(a("Leaflet for R", href="https://rstudio.github.io/leaflet/",target="_blank"),"&",
                                                    a("Leaflet.js", href="http://leafletjs.com/",target="_blank"),"are built on",strong("Java Script"),
                                                    "to handle and process",strong("GPS data",style="color:red"),"(Latitude & Longitude)"), br(),
                                                  p(strong("Interactive Maps")),
                                                  p("Open sources like",a("Open Street Maps"),"&",a("Carto DB"),"are used as base maps to plot the processed GPS Data",
                                                    "and features are provided to toggle between the base maps to have higher granularity")
                                                  
                                                  ) #ending of about software information tabPanel
                                       )#ending of tabset Panel
                                       
                                     ) #ending of well panel
                                     
                        ), #ending for side bar Panel
                        mainPanel(
                          tabsetPanel(types = "pills", position = "left",
                                      tabPanel("All status",leafletOutput("map",width = 1400,height = 820)),
                                      tabPanel("Source & Destination",leafletOutput("map_1",width = 1400, height = 820)))
                          
                        )) #ending for side bar Layout 
                      ),# ending for commuter tabPanel

             #Generating tab for Region
             tabPanel(p(tags$i(strong("Exploratory Analysis"))),
                      sidebarLayout(
                        sidebarPanel(h4(strong("User Input for Visualization")),width = 3,
                                     # inserting the loading message
                                     tags$head(tags$style(type="text/css", "
                                                          #loadmessage {
                                                          position: fixed;
                                                          top: 400px;
                                                          left: 500px;
                                                          width: 40%;
                                                          padding: 5px 0px 5px 0px;
                                                          text-align: center;
                                                          font-weight: bold;
                                                          font-size: 100%;
                                                          color: #000000;
                                                          background-color: #FFFFFF;
                                                          z-index: 105;
                                                          }
                                                          ")),
                                     #creating the side bar panel for visualization
                                     wellPanel(
                                       tabsetPanel(
                                         tabPanel("Controls", br(),
                                                  sliderInput("hour_", label="Choose your hour",min=0, max=23, value=12, step=1),
                                                  helpText("Move the slider back and forth to choose a delighted hour and press",strong("Submit")),
                                                  br(),
                                                  selectInput("sour","Choose your source region",c("East","West","Central","North","North-East"),selected="West"),
                                                  selectInput("dest","Choose your destination region",c("East","West","Central","North","North-East"),selected="West"),
                                                  #radioButtons("source","Choose your source region",c("East","West","Central","North","North-East")),
                                                  #radioButtons("destination","Choose your destination region",c("East","West","Central","North","North-East")),
                                                  submitButton("Submit"),
                                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                   tags$div(h3("Generating visualization...."),id="loadmessage")) #end of conditional panel
                                         ), #ending of control tab Panel
                                         
                                         # Generating tabpanel "About"
                                         tabPanel("About",
                                                  br(),
                                                  p("This page is designed to visualize
                                                    number of travellers in a particular region
                                                    and to see movement from one region to another at any particular point of time."),
                                                  p("The graphs and bar chart are developed using",
                                                    a("Plotly in R.",href="https://plot.ly/",target="_blank")),
                                                  p(h4(strong("Bar Chart"))),
                                                  p("The",strong("POB",style = "color:red"),"shows no.of travellers starting & passing through",
                                                    "that region where as", strong("PAYMENT",style = "color:green"),"represents",
                                                    "no. of travellers alighing in that region"), br(),
                                                  p(h4(strong("Pie Charts - Travellers from source region"))),
                                                  p("Represents no of travellers starting from one particular region and",
                                                    "alighting at another region"),
                                                  p(strong("Eg."), " The pie chart for",strong("West Region"),"shows the proportion of",
                                                    "travellers starting at",strong("West"),"and alighting at different regions"), br(),
                                                  p(h4(strong("Pie Charts - Travellers to destination region"))),
                                                  p("Represents no of travellers starting from one particular region and",
                                                    "alighting at another region"),
                                                  p(strong("Eg."), " The pie chart for",strong("West Region"),"shows the proportion of",
                                                    "travellers starting from different regions and alighting at",strong("West"))
                                                  ) #ending of tab panel
                                       )#ending of tabset Panel
                                       
                                     )#ending of well panel
                                     ),# ending of side bar panel
                        mainPanel (
                          fluidRow(column(12,plotlyOutput("trend"))),
                          br(),
                          fluidRow(column(5,plotlyOutput("North"),offset=1), 
                                   column(5,plotlyOutput("North_East"),offset=1))
                        )#ending of the main panel
                        )#end of side bar Layout
                      #fluidRow(column(4,plotlyOutput("East")))

                      ), #ending of region tabpanel

             #inserting a tab for Trip Sheet
             tabPanel(p(tags$i(strong("Potential Routes"))),
                       fluidRow(dataTableOutput("mytable"))
                      ) #ending of Trip Sheet Panel
  ) #loop ending for navbar page layout
) #loop ending for UI entry