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

###############
##  Summary  ##
############3##
factor_small_y <- as.factor(small_y)
summary(factor_small_y)
summary(small_y)
dim(frame_small_matrix)
sum_small_matrix <- summary(frame_small_matrix)
str_small_matrix <- str(frame_small_matrix)


######################
##  PCA Visualizer  ##
######################
##Havent tried these visualization

# install.packages("factoextra")
# library(factoextra)

#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
#fviz_eig(pca)

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
colour <- function(index, cols, x, y){
  plot(pca$x[, c(x, y)])
  points(pca$x[index, c(x, y)], col = cols)
}

################
##  ML Model  ##
################
#havent trained yet

#model
#model$finalModel

##################
##  Clustering  ##
##################
