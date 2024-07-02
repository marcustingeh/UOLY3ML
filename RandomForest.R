# Load required libraries
library(caret)
library(randomForest)

# Set the working directory
setwd("C:/Users/xavie/OneDrive/Desktop/MLCW/Data")

# Load the data
data <- read.csv("liverdisorder.csv")

# Create the data partition for cross-validation
set.seed(123)
folds <- createFolds(data$X.drinks., k = 10)

#Place to store results
rf_reg_results <- numeric()

# Place range of number of trees to evaluate
num_trees <- seq(50, 500, by = 50)

# Place to store RMSE values before and after optimization
rmse_values_before <- numeric(length(num_trees))
rmse_values_after <- numeric(length(num_trees))

# Perform 10-fold cross-validation for each number of trees
for (i in 1:length(num_trees)) {
  for (j in 1:10) {
    # Split data into training and test sets
    train_data <- data[-folds[[j]], ]
    test_data <- data[folds[[j]], ]
    
    # Random forest regression
    rf_reg_model <- randomForest(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., 
                                 data = train_data, ntree = num_trees[i])
    rf_reg_pred <- predict(rf_reg_model, newdata = test_data)
    rmse_values_before[i] <- rmse_values_before[i] + sqrt(mean((test_data$X.drinks. - rf_reg_pred)^2))
  }
  # Average RMSE across folds before optimization
  rmse_values_before[i] <- rmse_values_before[i] / 10
}

# Find the minimum RMSE and its corresponding number of trees before optimization
min_rmse_before <- min(rmse_values_before)
min_rmse_num_trees_before <- num_trees[which.min(rmse_values_before)]

# Print the minimum RMSE and its corresponding number of trees before optimization
print(paste("Minimum RMSE before optimization:", min_rmse_before))
print(paste("Number of trees corresponding to minimum RMSE (before optimization):", min_rmse_num_trees_before))

# Plot RMSE vs. number of trees before optimization
plot(num_trees, rmse_values_before, type = "b", xlab = "Number of Trees", ylab = "RMSE before optimization")

# Train the final model with the optimal number of trees
final_rf_model <- randomForest(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., 
                               data = data, ntree = min_rmse_num_trees_before)

# Make predictions on the entire dataset
final_rf_pred <- predict(final_rf_model, newdata = data)

# Calculate the RMSE for the final model
final_rf_rmse <- sqrt(mean((data$X.drinks - final_rf_pred)^2))

# Print the RMSE for the final model after optimization
print(paste("Final Random Forest Regression RMSE after optimization:", final_rf_rmse))
