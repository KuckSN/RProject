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

ui <- fluidPage(#theme =  shinytheme("united"),
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
                                                         tableOutput("dataView")
                                                       )),
                                              
                                              tabPanel("Image View",
                                                       sidebarPanel(
                                                         HTML("<h3>Wonder How Image Data Looks Like?</h3>"),
                                                         
                                                         numericInput("dataRow", label = "Visualize which row?", 
                                                                      min = 1, max = 17016,
                                                                      value = 1),
                                                         
                                                         actionButton("imageButton", "Submit", class = "btn btn-primary"),
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$label(h2("Raw Data to Image")),
                                                         h3("Image"),
                                                         imageOutput("individualImage"),
                                                         h3("Raw Data"),
                                                         tableOutput("rawData")
                                                       )),
                                              
                                              tabPanel("Data Summary",
                                                       sidebarPanel(
                                                         HTML("<h3>What Plot</h3>"),
                                                         selectInput("plot", "Your Chosen Plot", choices = list("Plot A" = "plotA",
                                                                                                                "Plot B" = "plotB", 
                                                                                                                "Plot C" = "plotC"))
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$label(h2("Summary Plot")),
                                                         plotOutput("plot")
                                                       )),
                                    ),
                           ),
                           
                           tabPanel(),
                           
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
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    print(test)
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)


#
#fluidRow(
#box(title = "", solidHeader = T, width = 8, collapsible = T, plotlyOutput("macro_plot"))
#)