# Load the required library for R6
library(R6)
library(mvtnorm)
# Define the Gaussian Mixture Model (GMM) class
GMM_clustering <- R6Class("GMM",
                          
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
  fit = function(X, K, epsilon = 0.001, max_iter = 10000) {
    n <- nrow(X)
    p <- ncol(X)
    
    # EM Algorithm
    # Initialization
    self$p <- rep(1/K, K)
    self$mu <- replicate(K, colMeans(X) + rnorm(p, 0, 0.01), simplify = FALSE)
    self$covar_mtrx[[k]] <- (t(centered_X) %*% (centered_X * responsibilities[, k])) / n_k[k] + diag(epsilon, p)
    
    # Initial E-step using matrix operations
    likelihoods_q0 <- sapply(seq_len(K), function(k) {
      p_k <- self$p[k]
      mu_k <- self$mu[[k]]
      covar_k <- self$covar_mtrx[[k]]
      inv_covar_k <- solve(covar_k)
      det_covar_k <- det(covar_k)
      
      diff <- t(t(X) - mu_k)
      mahalanobis_dist <- rowSums((diff %*% inv_covar_k) * diff)
      log(p_k) - 0.5 * (p * log(2 * pi) + log(det_covar_k) + mahalanobis_dist)
    })
    
    likelihoods_q1 <- likelihoods_q0
    iter <- 0
    
    # EM loop
    repeat {
      iter <- iter + 1
      likelihoods_q0 <- likelihoods_q1
      
      # Expectation step (E-step)
      responsibilities <- exp(likelihoods_q0)
      responsibilities <- responsibilities / rowSums(responsibilities)
      
      # Maximization step (M-step)
      n_k <- colSums(responsibilities)
      
      for (k in seq_len(K)) {
        self$p[k] <- n_k[k] / n
        self$mu[[k]] <- colSums(responsibilities[, k] * X) / n_k[k]
        centered_X <- sweep(X, 2, self$mu[[k]])
        self$covar_mtrx[[k]] <- (t(centered_X) %*% (centered_X * responsibilities[, k])) / n_k[k]
      }
      
      # Update likelihoods using matrix operations
      likelihoods_q1 <- sapply(seq_len(K), function(k) {
        p_k <- self$p[k]
        mu_k <- self$mu[[k]]
        covar_k <- self$covar_mtrx[[k]]
        inv_covar_k <- solve(covar_k)
        det_covar_k <- det(covar_k)
        
        diff <- t(t(X) - mu_k)
        mahalanobis_dist <- rowSums((diff %*% inv_covar_k) * diff)
        log(p_k) - 0.5 * (p * log(2 * pi) + log(det_covar_k) + mahalanobis_dist)
      })
      
      # Check for convergence
      if (max(abs(likelihoods_q1 - likelihoods_q0)) < epsilon || iter >= max_iter) {
        break
      }
    }
  },
  
  # Function to predict the cluster for new data points
  predict = function(X_new) {
    K <- length(self$p)
    n_new <- nrow(X_new)
    
    # Calculate the log-likelihoods for each component
    log_likelihoods <- sapply(seq_len(K), function(k) {
      p_k <- self$p[k]
      mu_k <- self$mu[[k]]
      covar_k <- self$covar_mtrx[[k]]
      inv_covar_k <- solve(covar_k)
      det_covar_k <- det(covar_k)
      
      diff <- t(t(X_new) - mu_k)
      mahalanobis_dist <- rowSums((diff %*% inv_covar_k) * diff)
      log(p_k) - 0.5 * (ncol(X_new) * log(2 * pi) + log(det_covar_k) + mahalanobis_dist)
    })
    
    # Assign each data point to the component with the highest likelihood
    predicted_clusters <- max.col(log_likelihoods)
    return(predicted_clusters)
  }
)
)