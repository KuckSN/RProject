# Small Dataset For R Shiny Usage
small_matrix <- data_matrix[1:1000, 1:2025]
small_matrix <- rbind(small_matrix, data_matrix[33998:34998, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[48292:49292, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[61905:62905, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[74761:75761, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[87905:88905, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[94819:95819, 1:2025])
small_matrix <- rbind(small_matrix, data_matrix[95820:96820, 1:2025])

small_y <- y_matrix[1:1000]
small_y <- append(small_y, y_matrix[33998:34998])
small_y <- append(small_y, y_matrix[48292:49292])
small_y <- append(small_y, y_matrix[61905:62905])
small_y <- append(small_y, y_matrix[74761:75761])
small_y <- append(small_y, y_matrix[87905:88905])
small_y <- append(small_y, y_matrix[94819:95819])
small_y <- append(small_y, y_matrix[95820:96820])
small_y <- as.matrix(small_y)