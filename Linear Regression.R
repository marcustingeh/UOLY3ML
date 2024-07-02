# Load required libraries
library(caret)

# Set the working directory
setwd("C:/Users/xavie/OneDrive/Desktop/MLCW/Data")

# Load the data
data <- read.csv("liverdisorder.csv")

# Create the data partition for cross-validation
set.seed(123)
folds <- createFolds(data$X.drinks, k = 10)

# Initialize vector to store results
linear_reg_results <- numeric()

# Perform 10-fold cross-validation
for (i in 1:10) {
  # Split data into training and test sets
  train_data <- data[-folds[[i]], ]
  test_data <- data[folds[[i]], ]
  
  # Linear regression
  linear_reg_model <- lm(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., data = train_data)
  linear_reg_pred <- predict(linear_reg_model, newdata = test_data)
  linear_reg_results <- c(linear_reg_results, sqrt(mean((test_data$X.drinks - linear_reg_pred)^2)))
}

# Print the mean RMSE for linear regression
print(paste("Linear Regression RMSE:", mean(linear_reg_results)))

glm.fit=glm(X.drinks. ~ X.mcv. + X.selector. + X.alkphos. + X.sgpt. + X.sgot. + X.gammagt., data = train_data)
summary(glm.fit)

# Initialize vector to store results
linear_reg_results2 <- numeric()

# Perform 10-fold cross-validation
for (i in 1:10) {
  # Split data into training and test sets
  train_data <- data[-folds[[i]], ]
  test_data <- data[folds[[i]], ]
  
  # Linear regression
  linear_reg_model2 <- lm(X.drinks. ~ X.mcv. + X.gammagt., data = train_data)
  linear_reg_pred2 <- predict(linear_reg_model2, newdata = test_data)
  linear_reg_results2 <- c(linear_reg_results2, sqrt(mean((test_data$X.drinks. - linear_reg_pred2)^2)))
}

# Print the mean RMSE for linear regression
print(paste("Linear Regression RMSE:", mean(linear_reg_results2)))


