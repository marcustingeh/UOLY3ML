# Load required libraries
library(caret)
library(gbm)
library(rpart)
library(ggplot2)
library(pROC)

# Set the working directory
setwd("C:/Users/xavie/OneDrive/Desktop/MLCW/Data")

# Load the data
data <- read.csv("php0iVrYT.csv")

# Set seed for reproducibility
set.seed(123)

# Convert the 'class' column to a factor
data$Class <- as.factor(data$Class)

# Split the data into training (70%) and test (30%) sets
trainIndex <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Define cross-validation control
ctrl <- trainControl(method = "cv", number = 10)

# Logistic Regression
log_reg_model <- train(Class ~ V1 + V2 + V3 + V4, data = train_data, method = "glm", trControl = ctrl)

# Make predictions on the test set
predictions1 <- predict(log_reg_model, newdata = test_data)

# Confusion matrices
conf_matrix1 <- confusionMatrix(predictions1, test_data$Class)

# Extract precision, recall, and F1-score from confusion matrices
precision1 <- conf_matrix1$byClass["Pos Pred Value"]
recall1 <- conf_matrix1$byClass["Sensitivity"]
f1_score1 <- conf_matrix1$byClass["F1"]

# Print precision, recall, and F1-score for each model
cat("Logistic Regression:\n")
cat("Precision:", precision1, "\n")
cat("Recall:", recall1, "\n")
cat("F1-score:", f1_score1, "\n\n")

# Define the control parameters for RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(x = data[, -5],    # Features
              y = data$Class, # Target
              sizes = c(1:4),                    # Number of features to select
              rfeControl = control,              # Training control
              method = "glm")                    # Model to use
print(result)

# Logistic Regression2
log_reg_model2 <- train(Class ~ V1 + V3 + V2, data = train_data, method = "glm", trControl = ctrl)
predictions4 <- predict(log_reg_model2, newdata = test_data)
conf_matrix4 <- confusionMatrix(predictions4, test_data$Class)

precision4 <- conf_matrix4$byClass["Pos Pred Value"]
recall4 <- conf_matrix4$byClass["Sensitivity"]
f1_score4 <- conf_matrix4$byClass["F1"]

cat("Precision:", precision4, "\n")
cat("Recall:", recall4, "\n")
cat("F1-score:", f1_score4, "\n\n")
