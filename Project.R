library(magick)
# data_matrix <- NULL
# y_matrix <- NULL

character <- c("subtract_indexes", 'leftP_indexes', "rightP_indexes", "plus_indexes", "equal_indexes", "zero_indexes", "one_indexes",
               "two_indexes", "three_indexes", "four_indexes", "five_indexes", "six_indexes", 'seven_indexes', "eight_indexes", 
               "nine_indexes", "div_indexes", "mul_indexes")
regex <- c("-", "\\(", ")", "\\+", "=", "0", "1", "2", "3", "4", "5","6", "7","8","9", "div", "X")

character_list <- list("-" = subtract_indexes, "(" = leftP_indexes, ")" = rightP_indexes,
                       "+"= plus_indexes, "=" = equal_indexes, "0" = zero_indexes, "1" = one_indexes,
                       "2" = two_indexes, "3" = three_indexes, "4" = four_indexes, "5" = five_indexes,
                       "6" = six_indexes, "7" = seven_indexes, "8" = eight_indexes, "9" = nine_indexes,
                       "x" = mul_indexes, "/" = div_indexes)

#Process image into vector
for(file in list.files(path = "C:\\Users\\kuckn\\Desktop\\Usable Data")) {
  current_file <- file
  print(paste("Processing:", file, sep=" "))
  for(images in list.files(paste("C:\\Users\\kuckn\\Desktop\\Usable Data", "\\", current_file, sep = ""), full.names = TRUE)) {
    current_image <- image_read(images)
    current_image_data <- as.numeric(current_image[[1]][1, , ])
    data_matrix <- rbind(data_matrix, current_image_data)
    y_matrix <- rbind(y_matrix, file)
    row.names(data_matrix)[nrow(data_matrix)] <- file
  }
}

#Analysis
pca <- prcomp(small_matrix)
scaled_pca <- prcomp(small_matrix, scale. = TRUE)

# PCA Scatter Plot
plot(pca$x[, c(1, 2)])

# Group each character 
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

# Colour each character on the plot
points(pca$x[subtract_indexes, 1:2], col = "violetred2")
points(pca$x[leftP_indexes, 1:2], col = "gray")
points(pca$x[rightP_indexes, 1:2], col = "brown")
points(pca$x[plus_indexes, 1:2], col = "violet")
points(pca$x[equal_indexes, 1:2], col = "slategray2")
points(pca$x[zero_indexes, 1:2], col = "red")
points(pca$x[one_indexes, 1:2], col = "red4")
points(pca$x[two_indexes, 1:2], col = "orange")
points(pca$x[three_indexes, 1:2], col = "orange3")
points(pca$x[four_indexes, 1:2], col = "yellow")
points(pca$x[five_indexes, 1:2], col = "yellow4")
points(pca$x[six_indexes, 1:2], col = "blue")
points(pca$x[seven_indexes, 1:2], col = "blue4")
points(pca$x[eight_indexes, 1:2], col = "purple")
points(pca$x[nine_indexes, 1:2], col = "mediumpurple")
points(pca$x[div_indexes, 1:2], col = "green")
points(pca$x[mul_indexes, 1:2], col = "green4")


# Prediction
# Multiclassing
symbols <- rep(0, times = nrow(pca$x))
symbols[zero_indexes] <- 0
symbols[one_indexes] <- 1
symbols[two_indexes] <- 2
symbols[three_indexes] <- 3
symbols[four_indexes] <- 4
symbols[five_indexes] <- 5
symbols[six_indexes] <- 6
symbols[seven_indexes] <- 7
symbols[eight_indexes] <- 8
symbols[nine_indexes] <- 9
symbols[subtract_indexes] <- 10
symbols[plus_indexes] <- 11
symbols[mul_indexes] <- 12
symbols[div_indexes] <- 13
symbols[leftP_indexes] <- 14
symbols[rightP_indexes] <- 15
symbols[equal_indexes] <- 16
predictive_matrix <- cbind(symbols, pca$x) 


# Model
library(caret)
model <- caret::train(y = as.vector(symbols), x=pca$x, method = "rf", trControl = trainControl(method = 'LOOCV', verboseIter = TRUE))

# This is for testing
small_model <- caret::train(y = as.vector(small_symbols), x = small, method = "rf", trControl = trainControl(method = "LOOCV", verboseIter = TRUE))

#See Model Summary
#model
#model$finalModel



##### WJ
library(ggplot2)
library(tidyr)

# first plot
exp = 7 # change this to show diff img
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

## second plot
## press button to show again, will pick diff image every time
par(mfcol=c(5,4))
par(mar=c(0,0,1.5,0), xaxs='i', yaxs='i')
for (i in 1:17){
  idxListPicked = switch(i, subtract_indexes, leftP_indexes, rightP_indexes, plus_indexes, equal_indexes,zero_indexes, one_indexes, two_indexes,three_indexes, four_indexes, five_indexes, six_indexes, seven_indexes, eight_indexes, nine_indexes, mul_indexes, div_indexes)
  rand = sample(idxListPicked, 1)
  img = matrix(unlist(img), nrow=45, ncol=45)
  img = img[, ncol(img):1]
  image(1:45, 1:45, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste(small_y[rand]))
}

# Data Summary
factor_small_y <- as.factor(small_y)
summary(factor_small_y)

summary(small_y)
dim(frame_small_matrix)
sum_small_matrix <- summary(frame_small_matrix)
str_small_matrix <- str(frame_small_matrix)
