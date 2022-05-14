#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("data.table")
#install.packages("RCurl")
#install.packages("randomForest")
#install.packages("markdown")
#install.packages("torch")
#install.packages("keras")
#install.packages("tensorflow")
#install_tensorflow()
#intsall_keras()

####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################


# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(rvest)
library(randomForest)


webpage = read_html("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv")
heading = html_node(webpage, 'body')
# Read data
weather <- read.csv(text = html_text(heading))
weather$outlook <- factor(weather$outlook, levels = c("overcast", "rainy", "sunny"))
weather$play <- factor(weather$play, levels = c("no", "yes"))
                          
# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
#model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme =  shinytheme("united"),
                navbarPage("MathX Recognizer:",
                           tabPanel("Main",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Image to Math Expression</h3>"),
                                      
                                      fileInput("image", label = "Please Select an Image:", accept = "image/*"),
                                     
                                      actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      uiOutput('uploadedImage'), #Show Uploaded Image
                                      textOutput("predictedExpression")
                                    )
                                    
                           ),
                           tabPanel("Data Views",
                                    navbarPage("",
                                              tabPanel("Data Table", 
                                                       sidebarPanel(
                                                         HTML("<h3>Peek the data</h3>"),
                                                         
                                                         sliderInput("numberHead", label = "Display First n rows:",
                                                                     min = 5, max = 17016,
                                                                     value = 5),
                                                         
                                                         actionButton("headButton", "Submit", class = "btn btn-primary"),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$label(h3("Data Table")),
                                                         dataTableOutput("dataView")
                                                       )),
                                              
                                              tabPanel("Image View",
                                                       sidebarPanel(
                                                         HTML("<h3>Wonder How Image Data Looks Like?</h3>"),
                                                         
                                                         numericInput("dataRow", label = "Visualize which row?", 
                                                                      min = 1, max = 17016,
                                                                      value = 1),
                                                         
                                                         actionButton("imageButton", "Submit", class = "btn btn-primary"),
                                                         
                                                         HTML("<br /> <h5>Notes:  <br />
                                                              \"-\": 1-1000  <br />
                                                              \"(\": 1001-2001  <br />
                                                              \")\": 2002-3002  <br />
                                                              \"+\": 3003-4003  <br />
                                                              \"=\": 4004-5004  <br />
                                                              \"0\": 5005-6005  <br />
                                                              \"/\": 6006-6873  <br />
                                                              \"x\": 6874-8007  <br />
                                                              \"1\": 8008-9008  <br />
                                                              \"2\": 9009-10009  <br />
                                                              \"3\": 10010-11010  <br />
                                                              \"4\": 11011-12011  <br />
                                                              \"5\": 12012-13012  <br />
                                                              \"6\": 13013-14013  <br />
                                                              \"7\": 14014-15014  <br />
                                                              \"8\": 15015-16015  <br />
                                                              \"9\": 16016-17016    <br />
                                                              </h5>")
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$label(h2("Raw Data to Image")),
                                                         h3("Image"),
                                                         plotOutput("individualImage"),
                                                         h3("Raw Data"),
                                                         dataTableOutput("rawData")
                                                       )),
                                              
                                              tabPanel("Data Summary",
                                                       sidebarPanel(
                                                         HTML("<h3>What Plot</h3>"),
                                                         selectInput("plot", "Your Chosen Plot", choices = list("Plot A" = "plotA",
                                                                                                                "Plot B" = "plotB", 
                                                                                                                "Plot C" = "plotC"))
                                                       ),
                                                       
                                                       mainPanel(
                                                         
                                                         h2("Summary Plot"),
                                                         plotOutput("summaryPlot"),
                                                         h2("Dimension"),
                                                         verbatimTextOutput("dimension"),
                                                         h2("6 Summary Value of First Image"),
                                                         h4("Summary value of the first sample of the data table using summary()"),
                                                         verbatimTextOutput("summary"),
                                                         h2("Structure"),
                                                         verbatimTextOutput("str"),
                                                       )),
                                    ),
                           ),
                           
                           tabPanel("Data Analysis",
                                    titlePanel("Relationship between Symbol Cluster & their pixel"),
                                    sidebarLayout(
                                      sidebarPanel = sidebarPanel("Observe the symbols and their pixel points on graph",
                                                                  numericInput('point1', "Point 1", 
                                                                               min = 1, max = 2025,
                                                                               value = 1),
                                                                  
                                                                  numericInput("point2", "Point 2", 
                                                                               min = 1, max = 2025,
                                                                               value = 2),
                                                                  
                                                                  selectInput("character", label = "Math Symbol", choices = list("-" = "-", "(" = "(", ")" = ")",
                                                                                                                                 "+"= "+", "=" = "=", "x" = "x", "/" = "/", 
                                                                                                                                 "0" = "0", "1" = "1",
                                                                                                                                 "2" = "2", "3" = "3", "4" = "4", "5" = "5",
                                                                                                                                 "6" = "6", "7" = "7", "8" = "8", "9" = "9")),
                                                                  
                                                                  textInput("pointColour", label = "Colour of Point", value = "violetred2"),
                                                                  
                                                                  actionButton("pcaPlotButton", "Submit", class = "btn btn-primary")
                                                                  ),
                                      
                                      mainPanel = mainPanel(
                                        tags$label(h2("PCA Scatter Plot")),
                                        h4("Principal Component Analysis (PCA) is transforming the values of each pixel using standard deviation, mean and rotation and gives meaning to each pixel with correlation to each other."),
                                        plotOutput("pcaPlot")
                                      )
                                    )),
                           
                           tabPanel("About",
                                    titlePanel("About"),
                                    div(includeMarkdown("about.md"),
                                        align="justify")         
                           )
                           
                  )
                
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # First tabPanel - Main
  observeEvent(input$image, {
    inFile <- input$image
    if (!is.null(inFile)){
      
    }
  })
  
  # Second tabPanel - Data Table
  datatableInput <- reactive({
    Output_table <- data.frame(view_matrix(input$numberHead))
    print(Output_table)
  })
  
  output$dataView <- renderDataTable({
    if(input$headButton >0){
      isolate(datatableInput())
    }
  }, options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  
  # Second tabPanel - Image Views
  imageViewInput <- reactive({
    image <- visualize_image(input$dataRow)
  })
  
  
  datatableRaw <- reactive({
    raw <- data.frame(get_visualize_image(input$dataRow))
    print(raw)
  })
  
  output$individualImage <- renderPlot({
    if(input$imageButton>0){
      isolate(imageViewInput())
    }
  })
  
  output$rawData <- renderDataTable({
    if(input$imageButton>0){
      isolate(datatableRaw())
    }
  },options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2)))

  # Second tabPanel - Data Summary
  dimPlot <- reactive({
    cbind(c("Rows", "Columns"), dim(frame_small_matrix))
  })
  
  strPlot <- reactive({
    str(frame_small_matrix)
  })
  
  sumPlot <- reactive({
    sum_small_matrix[,1]
  })
  
  output$dimension <- renderPrint({
    isolate(dimPlot())
  })
  
  output$str <- renderPrint({
    isolate(strPlot())
  })
  
  output$summary <- renderPrint({
    isolate(sumPlot())
  })
  
  # Third tabPanel - Data Analysis
  pcaPlot <- reactive({
    colour(character_list[[input$character]], input$pointColour, input$point1, input$point2)
  })

  output$pcaPlot <- renderPlot({
    if(input$pcaPlotButton>0){
      isolate(pcaPlot())
    }
  })
}

# h2("Summary Plot"),
# plotOutput("summaryPlot"),
# h2("Dimension"),
# tableOutput("dimension"),
# h2("Structure"),
# tableOutput("str"),
# h2("6 Summary Value of First Image"),
# tableOutput("summary")

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)


#
#fluidRow(
#box(title = "", solidHeader = T, width = 8, collapsible = T, plotlyOutput("macro_plot"))
#)