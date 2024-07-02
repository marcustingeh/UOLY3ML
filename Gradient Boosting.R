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

# Gradient Boosting
gbm_model <- train(Class ~ V1 + V2 + V3 + V4, data = train_data, method = "gbm", trControl = ctrl, verbose = FALSE)

predictions <- predict(gbm_model, newdata = test_data)

conf_matrix <- confusionMatrix(predictions, test_data$Class)

precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- conf_matrix$byClass["F1"]

cat("Gradient Boosting:\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-score:", f1_score, "\n\n")


# Define the control parameters for RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(x = data[, -5],    # Features
              y = data$Class, # Target
              sizes = c(1:4),                    # Number of features to select
              rfeControl = control,              # Training control
              method = "glm")                    # Model to use
print(result)

gbm_model2 <- train(Class ~ V1 + V3, data = train_data, method = "gbm", trControl = ctrl, verbose = FALSE)

predictions2 <- predict(gbm_model2, newdata = test_data)

conf_matrix2 <- confusionMatrix(predictions2, test_data$Class)

precision2 <- conf_matrix2$byClass["Pos Pred Value"]
recall2 <- conf_matrix2$byClass["Sensitivity"]
f1_score2 <- conf_matrix2$byClass["F1"]

cat("Gradient Boosting:\n")
cat("Precision:", precision2, "\n")
cat("Recall:", recall2, "\n")
cat("F1-score:", f1_score2, "\n\n")


