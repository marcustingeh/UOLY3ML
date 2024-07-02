# Load required libraries
library(caret)
library(e1071)

# Set the working directory
setwd("C:/Users/xavie/OneDrive/Desktop/MLCW/Data")

# Load the data
data <- read.csv("liverdisorder.csv")

# Create the data partition for cross-validation
set.seed(123)
folds <- createFolds(data$X.drinks., k = 10)

# Place to store RMSE values before and after optimization
rmse_values_before <- numeric()
rmse_values_after <- numeric()

# Range of values for C
c_values <- c(0.01, 0.1, 1, 10, 100)

# Perform grid search with cross-validation for each value of C
for (c in c_values) {
  svr_results <- numeric()
  for (i in 1:10) {
    # Split data into training and test sets
    train_data <- data[-folds[[i]], ]
    test_data <- data[folds[[i]], ]
    
    # Support vector regression
    svr_model <- svm(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., 
                     data = train_data, kernel = "linear", cost = c)
    svr_pred <- predict(svr_model, newdata = test_data)
    svr_results <- c(svr_results, sqrt(mean((test_data$X.drinks. - svr_pred)^2)))
  }
  # Average RMSE across folds before optimization
  rmse_values_before <- c(rmse_values_before, mean(svr_results))
}

# Find the best C based on the minimum RMSE
best_c <- c_values[which.min(rmse_values_before)]

# Train the final SVR model using the best C
final_svr_model <- svm(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., 
                       data = data, kernel = "linear", cost = best_c)

# Make predictions on the entire dataset
final_svr_pred <- predict(final_svr_model, newdata = data)

# Calculate the RMSE for the final model
final_svr_rmse <- sqrt(mean((data$X.drinks. - final_svr_pred)^2))

# Print the optimal regularization parameter and the RMSE after optimization
print(paste("Optimal Regularization Parameter (C):", best_c))
print(paste("Final Support Vector Regression RMSE after Optimization:", final_svr_rmse))

# Print the mean RMSE before optimization
print(paste("Mean RMSE before optimization:", mean(rmse_values_before)))


