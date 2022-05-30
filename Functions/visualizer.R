#########################################################################################################################
#Display PNG ....."Image Data with pixel 45x45 is vectorized into large array with 2025 (45*45) columns with 17016 rows"#
#########################################################################################################################

####################
## Matrix Display ##
## Tabulated Data ##
####################
view_matrix <- function(x){
  temp <- cbind(head(small_y, x), head(small_matrix, x))
  colnames(temp) <- c("Symbol", paste("Pixel", 1:2025, sep = ""))
  temp
}



################
## Visualizer ##
############3###
visualize_image <- function(x){
  image_matrix <- matrix(0, ncol = 45, nrow = 45)
  for(i in 0:44){
    #Debug
    #print(paste("Pass:", i))
    
    image_matrix[i+1, 1:45] <- small_matrix[(x), (45*i+1) : (45*(i+1))]
  }
  
  plot(as.raster(image_matrix, max=255))
}

get_visualize_image <- function(x){
  image_matrix <- matrix(0, ncol = 45, nrow = 45)
  for(i in 0:44){
    #Debug
    #print(paste("Pass:", i))
    
    image_matrix[i+1, 1:45] <- small_matrix[(x), (45*i+1) : (45*(i+1))]
  }
  image_matrix <- cbind(c(paste("Pixel", 1:45, sep = "")), image_matrix)
  colnames(image_matrix) <- c("V/H Pixel", paste("Pixel", 1:45, sep = ""))
  image_matrix
}

randomized_image <- function(x){
  # first plot
  exp = x # change this to show diff img
  idxListPicked = switch(exp, subtract_indexes, leftP_indexes, rightP_indexes, plus_indexes, equal_indexes,zero_indexes, one_indexes, two_indexes,three_indexes, four_indexes, five_indexes, six_indexes, seven_indexes, eight_indexes, nine_indexes, mul_indexes, div_indexes)
  random = sample(idxListPicked, 1)
  
  image_1 = matrix(unlist(small_matrix[random,]), nrow=45, ncol=45)
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

get_randomized_image <- function(x){
  exp = x # change this to show diff img
  idxListPicked = switch(exp, subtract_indexes, leftP_indexes, rightP_indexes, plus_indexes, equal_indexes,zero_indexes, one_indexes, two_indexes,three_indexes, four_indexes, five_indexes, six_indexes, seven_indexes, eight_indexes, nine_indexes, mul_indexes, div_indexes)
  random = sample(idxListPicked, 1)
  image_1 = matrix(unlist(small_matrix[random,]), nrow=45, ncol=45)
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
    img = small_matrix[rand, ]
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
eig_visualizer <- function(ncp_num, scaled){
  if(scaled){
    fviz_eig(scaled_pca, ncp = ncp_num)
  } else {
    fviz_eig(pca, ncp = ncp_num)
  }
}

#Graph of individuals. Individuals with a similar profile are grouped together.
# fviz_pca_ind(pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
# fviz_pca_var(pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

#################
##  PCA Color  ##
#################
colour <- function(index, cols, x, y, truth){
  if(truth){
    plot(scaled_pca$x[, c(x, y)])
    points(scaled_pca$x[index, c(x, y)], col = cols)
  } else {
    plot(pca$x[, c(x, y)])
    points(pca$x[index, c(x, y)], col = cols)
  }
}

################
##  ML Model  ##
################

#model
#model$finalModel

##################
##  Clustering  ##
##################
