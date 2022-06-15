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
library(markdown)
library(factoextra)
library(base64enc)
library(ggplot2)
library(tidyr)
library(httr)

print("Please download mainData.R and load it to your environment before running this Shiny Apps.")
load(file = "mainData.RData", envir = .GlobalEnv)
################################
## Loading Necessary Variable ##
################################
subtract_indexes <- grep(rownames(pca$x), pattern = "-")
leftP_indexes <- grep(rownames(pca$x), pattern = "\\(")
rightP_indexes <- grep(rownames(pca$x), pattern = ")")
plus_indexes <- grep(rownames(pca$x), pattern = "\\+")
equal_indexes <- grep(rownames(pca$x), pattern = "=")
zero_indexes <- grep(rownames(pca$x), pattern = "0")
one_indexes <- grep(rownames(pca$x), pattern = "1")
two_indexes <- grep(rownames(pca$x), pattern = "2")
three_indexes <- grep(rownames(pca$x), pattern = "3")
four_indexes <- grep(rownames(pca$x), pattern = "4")
five_indexes <- grep(rownames(pca$x), pattern = "5")
six_indexes <- grep(rownames(pca$x), pattern = "6")
seven_indexes <- grep(rownames(pca$x), pattern = "7")
eight_indexes <- grep(rownames(pca$x), pattern = "8")
nine_indexes <- grep(rownames(pca$x), pattern = "9")
div_indexes <- grep(rownames(pca$x), pattern = "div")
mul_indexes <- grep(rownames(pca$x), pattern = "X")

character_list <- list("-" = subtract_indexes, "(" = leftP_indexes, ")" = rightP_indexes,
                       "+"= plus_indexes, "=" = equal_indexes, "0" = zero_indexes, "1" = one_indexes,
                       "2" = two_indexes, "3" = three_indexes, "4" = four_indexes, "5" = five_indexes,
                       "6" = six_indexes, "7" = seven_indexes, "8" = eight_indexes, "9" = nine_indexes,
                       "x" = mul_indexes, "/" = div_indexes)
####################
## Matrix Display ##
## Tabulated Data ##
####################
view_matrix <- function(x){
  temp <- cbind(head(small_y, x), head(frame_small_matrix, x))
  colnames(temp) <- c("Symbol", paste("Pixel", 1:2025, sep = ""))
  temp
}

################
## Visualizer ##
############3###
get_random <- function(x){
  exp = x
  idxListPicked = switch(exp, subtract_indexes, leftP_indexes, rightP_indexes, plus_indexes, equal_indexes,zero_indexes, one_indexes, two_indexes,three_indexes, four_indexes, five_indexes, six_indexes, seven_indexes, eight_indexes, nine_indexes, mul_indexes, div_indexes)
  random = sample(idxListPicked, 1)
  random
}

randomized_image <- function(random_pick){
  # first plot
  random = random_pick
  
  image_1 = matrix(unlist(frame_small_matrix[random,]), nrow=45, ncol=45)
  image_1 = as.data.frame(t(image_1))
  colnames(image_1) <- seq_len(ncol(image_1))
  image_1$y <- seq_len(nrow(image_1))
  image_1 <- gather(image_1, "x", "value", -y)
  image_1$x <- as.integer(image_1$x)
  
  ggplot(image_1, aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = NA) +
    scale_y_reverse() +
    theme_minimal() +
    theme(panel.grid = element_blank())   +
    theme(aspect.ratio = 1) +
    xlab("") +
    ylab("")
}

get_randomized_image <- function(random_pick){
  random = random_pick
  image_1 = matrix(unlist(frame_small_matrix[random,]), nrow=45, ncol=45)
  image_1 = as.data.frame(t(image_1))
  print(image_1)
}

all_randomized_image <- function(){
  ## second plot
  ## press button to show again, will pick diff image every time
  par(mfcol=c(5,4))
  par(mar=c(0,0,1.5,0), xaxs='i', yaxs='i')
  for (i in 1:17){
    idxListPicked = switch(i, subtract_indexes, leftP_indexes, rightP_indexes, plus_indexes, equal_indexes,zero_indexes, one_indexes, two_indexes,three_indexes, four_indexes, five_indexes, six_indexes, seven_indexes, eight_indexes, nine_indexes, mul_indexes, div_indexes)
    rand = sample(idxListPicked, 1)
    img = frame_small_matrix[rand, ]
    img = matrix(unlist(img), nrow=45, ncol=45)
    img = img[, ncol(img):1]
    image(1:45, 1:45, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
          main = paste(small_y[rand]))
  }
}

###############
##  Summary  ##
###############

y_summary <- function(){
  df = sort(table(small_y))
  df = as.data.frame(df)
  names(df)[1] = 'symbols'
  barplot(height=df$Freq, names=df$symbols,
          col='#AC92EC', main="Distribution of Symbols in Test Set",
          xlab="Symbols", ylab="Frequency")
}

######################
##  PCA Visualizer  ##
######################
#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
eig_visualizer <- function(ncp_num){
  fviz_eig(pca, ncp = ncp_num)
}

#################
##  PCA Color  ##
#################
colour <- function(index, cols, x, y){
  plot(pca$x[, c(x, y)])
  points(pca$x[index, c(x, y)], col = cols)
}

#load(file = "mainData.RData", envir = .GlobalEnv)
####################################
# User interface                   #
####################################

ui <- fluidPage(#theme =  shinytheme("united"),
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
                                      h4("Uploaded image :"),
                                      uiOutput("image"), #Show Uploaded Image
                                      h5("Predicted Expression : "),
                                      textOutput("expression"),
                                      h5("Character Confidence : "),
                                      verbatimTextOutput("confidence"),
                                      h4("What the AI Saw:"),
                                      uiOutput("bbox_image")
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
                                                         
                                                         numericInput("dataRow", label = "Visualize which class of mathematical expression? Choose a number with guidance from hint.", 
                                                                      min = 1, max = 17,
                                                                      value = 1),
                                                         
                                                         actionButton("imageButton", "Submit", class = "btn btn-primary"),
                                                         
                                                         HTML("<br /> <h5>Hints:  <br />
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
                                                         
                                                         actionButton("allButton", "Generate", class = "btn btn-primary"),
                                                       ),
                                                       
                                                       mainPanel(
                                                         h3("All Randomized Symbols"),
                                                         h4("p/s Random sample image will be chosen"),
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
                                                                  
                                                                  actionButton("pcaPlotButton", "Submit", class = "btn btn-primary"),
                                                                  
                                                                  HTML("<h3>Scree Plot</h3>"),
                                                                  
                                                                  numericInput("pcaScree", "How many points of PCA to analyse?", 
                                                                               min = 10, max = 2025,
                                                                               value = 100),
                                                                  
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
  
  getImg <- function(txt) {
    raw <- base64Decode(txt, mode="raw")
    if (all(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))==raw[1:8])) { # it's a png...
      img <- png::readPNG(raw)
      transparent <- img[,,4] == 0
      img <- as.raster(img[,,1:3])
      img[transparent] <- NA
    } else if (all(as.raw(c(0xff, 0xd8, 0xff, 0xd9))==raw[c(1:2, length(raw)-(1:0))])) { # it's a jpeg...
      img <- jpeg::readJPEG(raw)
    } else stop("No Image!")
    return(img)
  }
  
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
        style = "width: 60vw;"
        )
      }
    })
  
   pressbtn <- observeEvent(input$submitbutton,{
      url = "https://wie2003handwriting-recognition-o2f2jvjewq-de.a.run.app/api/predict"
   post_zip = httr::POST(
       url = url,
       body = list(
         file=dataURI(file = input[["upload"]]$datapath, mime = "image/jpeg")
       ),
      httr::add_headers("Content-Type"="image/jpeg")
    )
    post_zip
    exp = content(post_zip, "parsed")
    output$expression <- renderText({exp$prediction})
    output$confidence <- renderText({exp$confidence})
    output[["bbox_image"]] <- renderUI({
      if(!is.null(exp$image)){
        tags$div(
          tags$img(src= paste("data:image/png;base64,",exp$image,sep=""), width="100%"),
          style = "width: 60vw;margin-bottom:50px;"
        )
      }
    })

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
      isolate(get_rand())
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
    sum_small_matrix <- summary(frame_small_matrix)
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
    colour(character_list[[input$character]], input$pointColour, input$point1, input$point2)
  }

  output$pcaPlot <- renderPlot({
    if(input$pcaPlotButton>0){
      isolate(pcaPlot())
    }
  })
  
  screePlot <- function(){
    eig_visualizer(input$pcaScree)
  }
  
  output$screePlot <- renderPlot({
    if(input$screePlotButton>0){
      isolate(screePlot())
    }
  })
}


####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
