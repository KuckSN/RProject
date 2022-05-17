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



# Smallest Dataset for Machine Learning Model Training
small_predictive_matrix <- predictive_matrix[1:10, 1:2026]
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[1001:1010, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[2002:2011, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[3003:3012, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[4004:4013, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[5005:5014, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[6006:6015, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[7007:7016, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[8008:8017, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[9009:9018, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[10010:10019, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[11011:11020, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[12012:12021, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[13013:13022, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[14014:14023, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[15015:15024, 1:2026])
small_predictive_matrix <- rbind(small_predictive_matrix, predictive_matrix[16016:16025, 1:2026])

small_symbols <- symbols[1:10]
small_symbols <- append(small_symbols, symbols[1001:1010])
small_symbols <- append(small_symbols, symbols[2002:2011])
small_symbols <- append(small_symbols, symbols[3003:3012])
small_symbols <- append(small_symbols, symbols[4004:4013])
small_symbols <- append(small_symbols, symbols[5005:5014])
small_symbols <- append(small_symbols, symbols[6006:6015])
small_symbols <- append(small_symbols, symbols[7007:7016])
small_symbols <- append(small_symbols, symbols[8008:8017])
small_symbols <- append(small_symbols, symbols[9009:9018])
small_symbols <- append(small_symbols, symbols[10010:10019])
small_symbols <- append(small_symbols, symbols[11011:11020])
small_symbols <- append(small_symbols, symbols[12012:12021])
small_symbols <- append(small_symbols, symbols[13013:13022])
small_symbols <- append(small_symbols, symbols[14014:14023])
small_symbols <- append(small_symbols, symbols[15015:15024])
small_symbols <- append(small_symbols, symbols[16016:16025])

small <- small_predictive_matrix[, 2:2026]
