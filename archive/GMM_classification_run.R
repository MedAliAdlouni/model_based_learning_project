#rm(list=ls())

# Load required libraries
library(caret)

# Source the GMM class from the external file
source("C:/projects/model_based_learning/td1/GMM_classification_algo.R")

# Assign X and y from the iris dataset
X <- as.matrix(iris[, -5])  # Features (columns 1 to 4)
y <- iris[, 5]  # Target variable (Species)

# Split data into train and test sets
set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Create an instance of the GMM class and fit the model
gmm_model <- GMM_classification$new()
gmm_model$fit(X_train, y_train)

# Predict on the test set
y_pred <- gmm_model$predict(X_test)

# Convert predicted indices to class labels
unique_classes <- unique(y_train)
y_pred_labels <- unique_classes[y_pred]

# Check accuracy
accuracy <- sum(y_pred_labels == y_test) / length(y_test)
cat("Test Accuracy: ", accuracy, "\n")
