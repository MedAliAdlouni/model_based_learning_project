# Load required libraries
library(R6)
library(caret)
library(MyGMMPackage)


# Source the GMM class
# source("EM_algorithm_.R")

# Assign X and y from the iris dataset
X <- as.matrix(iris[, -5])  # Features (columns 1 to 4)
y <- iris[, 5]  # Target variable (Species)
true_labels <- as.numeric(y)  # Convert factor levels to numeric

# Standardize the features before passing to the EM algorithm
preprocess_params <- preProcess(X, method = c("center", "scale"))
X <- predict(preprocess_params, X)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)  # 80% training, 20% testing
X_train <- X[train_index, ]
y_train <- true_labels[train_index]
X_test <- X[-train_index, ]
y_test <- true_labels[-train_index]


# Set parameters for the EM algorithm
K <- 3
model_types <- c("basic", "homoscedastic", "CEM", "SEM")


# Call EM algorithm for the training set
results <- em_algorithm(X_train, K, model = "basic", max_iterations = 50, nb_initializations = 5,
                        loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step = 10, init_method = "random")

# Extract results
losses <- results$losses
proportions <- results$proportions
mu <- results$mu
sigma <- results$sigma
last_t_i_k <- results$last_t_i_k
bic_score <- results$bic
maximum_a_posteriori <- results$maximum_a_posteriori

# Print results
print(last_t_i_k)
print(losses)
print(maximum_a_posteriori)
cat("BIC score: ", bic_score, "\n")  # Print BIC score

# Evaluate training performance scores
train_perf <- evaluate_gmm(y_train, maximum_a_posteriori)

train_confusion_matrix <- train_perf$confusion_matrix
train_accuracy <- train_perf$accuracy

cat("Training Confusion Matrix: \n")
print(train_confusion_matrix)
cat("Training Accuracy: ", train_accuracy * 100, "%\n")

# Evaluate test performance scores
# Predict cluster assignments for test data
test_clusters <- predict_gmm(X_test, proportions, mu, sigma, K)  # Ensure you have a function to predict clusters
test_perf <- evaluate_gmm(y_test, test_clusters)

test_confusion_matrix <- test_perf$confusion_matrix
test_accuracy <- test_perf$accuracy

cat("Testing Confusion Matrix: \n")
print(test_confusion_matrix)
cat("Testing Accuracy: ", test_accuracy * 100, "%\n")

