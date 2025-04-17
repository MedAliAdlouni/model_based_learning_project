# Load required libraries
library(caret)   # For standardization
library(Rtsne)   # For t-SNE
library(R6)
library(MyGMMPackage)

load("data/mnist.RData")

# Split data into training and testing sets
set.seed(123)

mnist_ytrain <-  as.numeric(mnist_ytrain)

# Standardize the data
# X_scaled <- scale(mnist_Xtrain[1:100, ], center = TRUE, scale = FALSE)

# Set parameters for the EM algorithm and the number of components
K <- 10  # Number of clusters

# Apply PCA
pca_result <- prcomp(mnist_Xtrain)
reduced_train <- pca_result$x[,1:50]
X_pca <- scale(reduced_train)
explained_variance_ratio <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
total_variance_explained <- sum(explained_variance_ratio[1:50])
cat("Total variance explained by the first 50 components:", total_variance_explained, "\n")

# Apply t-SNE
X_tsne <- Rtsne(mnist_Xtrain, dims = 3, perplexity = 30, theta = 0.5)
X_tsne <- scale(X_tsne$Y)


# Assuming em_tsne$losses is the loss over iterations for each K
bics <- numeric(15)  # Assuming you test from K = 1 to K = 10

for (cluster in 2:16) {
  # Run your EM algorithm for each K (this part depends on your specific EM implementation)
  em_tsne_result <- em_algorithm(X_tsne, cluster, model = "basic", max_iterations = 100, nb_initializations = 5,
                                 loss_treshold = 1e-6, plot_losses = FALSE, nb_iter_init_step = 10, init_method = "kmeans") # Example call to EM algorithm
  bics[cluster-1] <- em_tsne_result$bic  # Get the bic
}

# Plot final losses vs K
plot(2:16, bics, type = "b", main = "BIC score for Different K Values",
     xlab = "Number of Clusters (K)", ylab = "BIC score", pch = 19)






# Run EM algorithm on data
#em <- em_algorithm(X_scaled, K, model = "homoscedastic", max_iterations = 100, nb_initializations = 5,
#                       loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step = 10, init_method = "kmeans")

# Run EM algorithm on PCA-reduced data
cat("Running EM on PCA-reduced data...\n")
em_pca <- em_algorithm(X_pca, K, model = "basic", max_iterations = 100, nb_initializations = 5,
                       loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step = 10, init_method = "kmeans")

# Run EM algorithm on t-SNE-reduced data
cat("Running EM on t-SNE-reduced data...\n")
em_tsne <- em_algorithm(X_tsne, K, model = "basic", max_iterations = 100, nb_initializations = 5,
                        loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step = 10, init_method = "kmeans")

cat("BIC score for EM_PCA: ", em_pca$bic, "\n")
cat("BIC score for EM_tSNE: ", em_tsne$bic, "\n")




# Extract the losses from the EM algorithm results
losses_pca <- em_pca$losses
losses_tsne <- em_tsne$losses

# Determine the range of iterations for plotting
iterations_pca <- seq_along(losses_pca)
iterations_tsne <- seq_along(losses_tsne)

# Set up a 1-row, 2-column plotting layout
par(mfrow = c(1, 2))

# Plot the losses for EM on PCA-reduced data
plot(iterations_pca, losses_pca, type = "l", col = "blue", lwd = 2,
     xlab = "Iterations", ylab = "Loss", main = "EM on PCA-Reduced Data")

# Plot the losses for EM on t-SNE-reduced data
plot(iterations_tsne, losses_tsne, type = "l", col = "red", lwd = 2,
     xlab = "Iterations", ylab = "Loss", main = "EM on t-SNE-Reduced Data")

# Reset the plotting layout to default
par(mfrow = c(1, 1))










# Evaluate training performance scores
# TSNE
train_perf_tsne <- evaluate_gmm(mnist_ytrain, em_tsne$maximum_a_posteriori)
train_confusion_matrix_tsne <- train_perf_tsne$confusion_matrix
train_accuracy_tsne <- train_perf_tsne$accuracy
cat("Training Confusion Matrix: \n")
print(train_confusion_matrix_tsne)
cat("Training Accuracy: ", train_accuracy_tsne * 100, "%\n")

#PCA
train_perf_pca <- evaluate_gmm(mnist_ytrain, em_pca$maximum_a_posteriori)
train_confusion_matrix_pca <- train_perf_pca$confusion_matrix
train_accuracy_pca <- train_perf_pca$accuracy
cat("Training Confusion Matrix: \n")
print(train_confusion_matrix_pca)
cat("Training Accuracy: ", train_accuracy_pca * 100, "%\n")



# Evaluate test performance scores
# Predict cluster assignments for test data
#test_clusters <- predict_gmm(X_pca_test, em_tsne$proportions, em_tsne$mu, em_tsne$sigma, K)  # Ensure you have a function to predict clusters
#test_perf <- evaluate_gmm(ytest, test_clusters)




# Parameters to test
models <- c("basic", "homoscedastic", "CEM", "SEM")
nb_initializations_list <- c(5, 10, 20)
init_methods <- c("kmeans", "random")

# Initialize an empty list to store the loss values for each model
losses_list <- list()

# Run the EM algorithm for each model
for (init in nb_initializations_list) {
  result <- em_algorithm(
    X = X_tsne,             # Your input data
    K = 3,                   # Number of clusters
    model = "basic",      # Set the model type (basic, homoscedastic, CEM, SEM)
    nb_initializations = init, # Number of initializations
    max_iterations = 100,    # Max iterations
    loss_treshold = 1e-6,    # Loss threshold for convergence
    plot_losses = FALSE,     # We don't need to plot the loss inside the function
    nb_iter_init_step = 20,  # Number of iterations for the initialization step
    init_method = "random"   # Initialization method
  )

  # Store the loss values for this model
  losses_list[[model_type]] <- result$losses
}

# Plot the loss evolution for each model
plot(1:length(losses_list[[1]]), losses_list[[1]], type = "l", col = "blue",
     xlab = "Iterations", ylab = "Loss", lwd = 2,
     main = "Loss Evolution for Different GMM Models")
for (i in 2:length(models)) {
  lines(1:length(losses_list[[i]]), losses_list[[i]], col = i, lwd = 2)
}

# Add a legend
legend("topright", legend = models, col = 1:length(models), lwd = 2)
