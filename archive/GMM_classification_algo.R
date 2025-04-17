# Load the required library for R6
library(R6)

# Define the Gaussian Mixture Model (GMM) class
GMM_classification <- R6Class("GMM",
               
  public = list(
   p = NULL,
   mu = NULL,
   covar_mtrx = NULL,
   
   # Constructor
   initialize = function() {
     self$p <- list()
     self$mu <- list()
     self$covar_mtrx <- list()
   },
   
   # Function to fit the GMM model
   fit = function(X, y) {
     unique_classes <- unique(y)
     counts <- table(y)
     K <- length(unique_classes)
     n <- nrow(X)
     
     n_k <- numeric(K)
     self$p <- numeric(K)
     self$mu <- list()
     self$covar_mtrx <- list()
     
     for (k in seq_len(K)) {
       class_k <- unique_classes[k]
       n_k[k] <- counts[class_k]
       self$p[k] <- n_k[k] / n
       
       # Compute mean (mu) for class k
       X_k <- X[y == class_k, , drop = FALSE]
       self$mu[[k]] <- colMeans(X_k)
       
       # Compute covariance matrix for class k
       centered_X_k <- sweep(X_k, 2, self$mu[[k]])
       self$covar_mtrx[[k]] <- (t(centered_X_k) %*% centered_X_k) / n_k[k]
     }
   },
   
   # Function to make predictions based on the fitted GMM model
   predict = function(X_test) {
     K <- length(self$p)
     n <- nrow(X_test)
     likelihoods <- matrix(0, n, K)
     
     for (k in seq_len(K)) {
       p_k <- self$p[k]
       mu_k <- self$mu[[k]]
       covar_k <- self$covar_mtrx[[k]]
       inv_covar_k <- solve(covar_k)
       det_covar_k <- det(covar_k)
       
       for (i in seq_len(n)) {
         x_i <- X_test[i, , drop = FALSE]
         diff <- x_i - mu_k
         exponent <- -0.5 * diff %*% inv_covar_k %*% t(diff)
         likelihoods[i, k] <- log(p_k) - p_k * 0.5 * log(2 * pi) - 0.5 * log(det_covar_k) + exponent
       }
     }
     
     # Assign each point to the class with the highest likelihood
     predicted_class <- apply(likelihoods, 1, which.max)
     return(predicted_class)
   }
  )
)
