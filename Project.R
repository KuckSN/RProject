#library(magick)
#data_matrix <- NULL
#y_matrix <- NULL
library(magick)


# for(file in list.files(path = "C:\\Users\\kuckn\\Desktop\\Usable Data")) {
#   current_file <- file
#   print(paste("Processing:", file, sep=" "))
#   for(images in list.files(paste("C:\\Users\\kuckn\\Desktop\\Usable Data", "\\", current_file, sep = ""), full.names = TRUE)) {
#     current_image <- image_read(images)
#     current_image_data <- as.numeric(current_image[[1]][1, , ])
#     data_matrix <- rbind(data_matrix, current_image_data)
#     y_matrix <- rbind(y_matrix, file)
#     row.names(data_matrix)[nrow(data_matrix)] <- file
#   }
# }

#Analysis
pca <- prcomp(data_matrix)
plot(pca$x[, 1:2])

subtract_indexes <- grep(rownames(pca$x), pattern = "-")
leftP_indexes <- grep(rownames(pca$x), pattern = "(")
rightP_indexes <- grep(rownames(pca$x), pattern = ")")
plus_indexes <- grep(rownames(pca$x), pattern = "+")
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
points(pca$x[subtract_indexes, 1:2], col = "violetred2")
points(pca$x[leftP_indexes, 1:2], col = "black")
points(pca$x[rightP_indexes, 1:2], col = "orange")
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


#Prediction
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

#Model
library(caret)
model <- train(as.factor(symbols)~., data = predictive_matrix, trControl = trainControl(method = "LOOCV"), method = "rf")
