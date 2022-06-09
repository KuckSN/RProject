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

# Data Summary
factor_small_y <- as.factor(small_y)
sum_small_matrix <- summary(frame_small_matrix)
str_small_matrix <- str(frame_small_matrix)
