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


# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(rvest)
library(randomForest)
library(factoextra)
library(base64enc)
options(shiny.maxRequestSize = 30*1024^2)
library(ggplot2)
library(tidyr)

print("Please download mainData.R and load it to your environment before running this Shiny Apps.")
load(file = "mainData.RData", envir = .GlobalEnv)
####################################
# User interface                   #
####################################

ui <- fluidPage(theme =  shinytheme("united"),
                navbarPage("MathX Recognizer:",
                           tabPanel("Main",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Image to Math Expression</h3>"),
                                      
                                      fileInput("upload", label = "Please Select an Image:", accept = "image/*"),
                                     
                                      actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      h4("The uploaded image :"),
                                      uiOutput("image"), #Show Uploaded Image
                                      h4("predicted Expression : ")
                                    )
                                    
                           ),
                           tabPanel("Data Views",
                                    navbarPage("",
                                              tabPanel("Data Table", 
                                                       sidebarPanel(
                                                         HTML("<h3>Peek the data</h3>"),
                                                         
                                                         numericInput("numberHead", label = "Display First n rows:",
                                                                     min = 1, max = 17016,
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
                                                                      min = 1, max = 17,
                                                                      value = 1),
                                                         
                                                         actionButton("imageButton", "Submit", class = "btn btn-primary"),
                                                         
                                                         HTML("<br /> <h5>Notes:  <br />
                                                              \"-\": 1  <br />
                                                              \"(\": 2  <br />
                                                              \")\": 3  <br />
                                                              \"+\": 3  <br />
                                                              \"=\": 5  <br />
                                                              \"0\": 6  <br />
                                                              \"/\": 7  <br />
                                                              \"x\": 8  <br />
                                                              \"1\": 9  <br />
                                                              \"2\": 10  <br />
                                                              \"3\": 11  <br />
                                                              \"4\": 12  <br />
                                                              \"5\": 13  <br />
                                                              \"6\": 14  <br />
                                                              \"7\": 15  <br />
                                                              \"8\": 16  <br />
                                                              \"9\": 17    <br />
                                                              </h5>"),
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$label(h2("Raw Data to Image")),
                                                         h3("Image"),
                                                         plotOutput("individualImage"),
                                                         h3("Raw Data"),
                                                         dataTableOutput("rawData"),
                                                       )),
                                              
                                              tabPanel("All Symbols Image",
                                                       sidebarPanel(
                                                         HTML("<h3>Wonder How Image Data Looks Like?</h3>"),
                                                         
                                                         HTML("<h4>Show All Randomized Symbols</h4>"),
                                                         
                                                         actionButton("allButton", "Submit", class = "btn btn-primary"),
                                                       ),
                                                       
                                                       mainPanel(
                                                         h3("All Randomized Symbols"),
                                                         plotOutput("allImage")
                                                       )),
                                              
                                              tabPanel("Data Summary",
                                                       sidebarPanel(
                                                         HTML("<h3>Data Summary  <br /></h3>"),
                                                         HTML("<h4>In this section, we will display descriptive and exploratory analysis on our image dataset.  <br /></h4>"),
                                                         
                                                         HTML("<h3>EDA - Bar Plot   <br /></h3>"),
                                                         HTML("<h4>This first analysis is EDA, using Bar Chart  <br /></h4>"),
                                                         
                                                         HTML("<h3>Descriptive Analysis - Dimension   <br /></h3>"),
                                                         HTML("<h4>The second analysis is descriptive, using dim() function. This section tells you about the dimension of our dataset  <br /></h4>"),
                                                         
                                                         HTML("<h3>Descriptive Analysis - Summary   <br /></h3>"),
                                                         HTML("<h4>The third analysis is descriptive, using summary() function. This section obtain the value of min, max, Q1, Q2, Q3 and Q4  <br /></h4>"),
                                                         
                                                         HTML("<h3>Descriptive Analysis - Structure   <br /></h3>"),
                                                         HTML("<h4>The last analysis is descriptive, using str() function. This section show you the structure of our dartaset.  <br /></h4>"),
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
                                      sidebarPanel = sidebarPanel(HTML("<h3>PCA Scatter Plot</h3>"),
                                                                  
                                                                  HTML("<h5>Observe the symbols and their pixel points on graph</h5>"),
                                                                  
                                                                  
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
                                                                  
                                                                  checkboxInput("scaled", "Scaled PCA Value?", value = TRUE),
                                                                  
                                                                  actionButton("pcaPlotButton", "Submit", class = "btn btn-primary"),
                                                                  
                                                                  HTML("<h3>Scree Plot</h3>"),
                                                                  
                                                                  numericInput("pcaScree", "How many points of PCA to analyse?", 
                                                                               min = 10, max = 2025,
                                                                               value = 100),
                                                                  
                                                                  checkboxInput("scaledScree", "Scaled PCA Value?", value = TRUE),
                                                                  
                                                                  actionButton("screePlotButton", "Submit", class = "btn btn-primary"),
                                                                  ),
                                      
                                      mainPanel = mainPanel(
                                        tags$label(h2("PCA Scatter Plot")),
                                        h4("Principal Component Analysis (PCA) is transforming the values of each pixel using standard deviation, mean and rotation and gives meaning to each pixel with correlation to each other."),
                                        plotOutput("pcaPlot"),
                                        h2("Scree Plot"),
                                        h4("Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component."),
                                        plotOutput("screePlot")
                                      )
                                    )),
                           
                           tabPanel("About",
                                    HTML("<h1>About</h1>"),
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
  base64 <- reactive({
    inFile <- input[["upload"]]
    if(!is.null(inFile)){
      dataURI(file = inFile$datapath, mime = "image/*")
    }
  })
  
  output[["image"]] <- renderUI({
    if(!is.null(base64())){
      tags$div(
        tags$img(src= base64(), width="100%"),
        style = "width: 300px;"
        )
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
  a = reactiveVal(0);
  
  get_rand <- function(){
    a(get_random(input$dataRow))
  }
  
  imageViewInput <- function(){
    randomized_image(a())
  }
  
  
  datatableRaw <- reactive({
    raw <- get_randomized_image(a())
  })
  
  allImage <- function(){
    all_randomized_image()
  }
  
  output$individualImage <- renderPlot({
    if(input$imageButton>0){
      get_rand()
      isolate(imageViewInput())
    }
  })
  
  output$rawData <- renderDataTable({
    if(input$imageButton>0){
      isolate(datatableRaw())
    }
  },options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2)))
  
  output$allImage <- renderPlot({
    if(input$allButton>0){
      allImage()
    }
  }, height = 500, width = 500  )
  

  # Second tabPanel - Data Summary
  barShinyPlot <- reactive({
    y_summary()
  })
  
  dimPlot <- reactive({
    cbind(c("Rows", "Columns"), dim(frame_small_matrix))
  })
  
  strPlot <- reactive({
    str(frame_small_matrix)
  })
  
  sumPlot <- reactive({
    sum_small_matrix[,1]
  })
  
  output$summaryPlot <- renderPlot({
    isolate(barShinyPlot())
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
  pcaPlot <- function(){
    colour(character_list[[input$character]], input$pointColour, input$point1, input$point2, input$scaled)
  }

  output$pcaPlot <- renderPlot({
    if(input$pcaPlotButton>0){
      isolate(pcaPlot())
    }
  })
  
  screePlot <- function(){
    eig_visualizer(input$pcaScree, input$scaledScree)
  }
  
  output$screePlot <- renderPlot({
    if(input$screePlotButton>0){
      isolate(screePlot())
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