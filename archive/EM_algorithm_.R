library(R6)
library(mvtnorm)


em_algorithm <- function(X, K, model = "basic", nb_initializations = 10, max_iterations = 100,
                         loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step = 10, init_method = "random") {

  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]

  # Compute the number of model parameters (check the correctness of this function)
  nb_model_parameters <- compute_nb_model_params(K, model, nb_features)
  cat("Number of model parameters: ", nb_model_parameters, "\n")

  # Model parameters initialization
  params <- initialize_parameters(X, K, nb_initializations, nb_iter_init_step, init_method)

  if (is.null(params)) {
    stop("Initialization failed. Parameters not returned.")
  }

  proportions <- params$proportions
  mu <- params$mu
  sigma <- params$sigma

  # Initialize loss tracking
  losses <- numeric(max_iterations + 1)

  # Compute initial loss
  first_loss <- compute_loss(X, K, proportions, mu, sigma)
  losses[1] <- first_loss
  cat("Initial Loss: ", first_loss, "\n")  # Print initial loss

  # Variable to store t_i_k from the last iteration
  last_t_i_k <- NULL

  for (iteration in 1:max_iterations) {
    cat("Iteration:", iteration, "\n")  # Print the current iteration number

    # E-step: Compute responsibilities
    t_i_k <- E_step(X, K, proportions, mu, sigma)

    # Store t_i_k from the last iteration (will return this later)
    last_t_i_k <- t_i_k

    # M-step: Update parameters
    params <- M_step(X, t_i_k, model = model)

    # Update parameters after M-step
    proportions <- params$proportions
    mu <- params$mu
    sigma <- params$sigma

    # Compute current loss
    current_loss <- compute_loss(X, K, proportions, mu, sigma)
    losses[iteration + 1] <- current_loss
    cat("Current Loss: ", current_loss, "\n")  # Print current loss

    # Check for convergence
    if (abs(losses[iteration + 1] - losses[iteration]) < loss_treshold) {
      cat("Converged at iteration", iteration, "with loss", current_loss, "\n")
      break
    }
  }

  # Trim losses to actual iterations
  losses <- losses[1:iteration]

  # Compute BIC (ensure the bic function works correctly)
  bic_value <- bic(nb_observations, nb_model_parameters, tail(losses, 1))

  # Plot losses if required
  if (plot_losses) {
    plot(losses[-1], type = "o", col = "blue", xlab = "Iteration", ylab = "Log-Loss",
         main = paste("Loss Evolution in EM Algorithm - Model:", model))
  }

  # Assign maximum a posteriori labels
  maximum_a_posteriori <- apply(last_t_i_k, 1, function(row) which.max(row))
  cat("Maximum a posteriori labels:", maximum_a_posteriori, "\n")

  # Return results
  return(list(losses = losses, proportions = proportions, mu = mu, sigma = sigma,
              last_t_i_k = last_t_i_k, bic = bic_value, maximum_a_posteriori = maximum_a_posteriori))
}

initialize_parameters <- function(X, K, nb_initializations, nb_iter_init_step, init_method = "random") {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]

  best_loss <- -Inf  # Start with a very low value
  best_params <- NULL  # Placeholder for the best parameters
  best_initialization <- NULL  # Placeholder for the best initialization

  for (initialization in 1:nb_initializations) {
    cat("Initialization number: ", initialization, "\n")  # Print initialization number

    # Initialize parameters for this run
    proportions <- rep(1 / K, K)
    mu <- vector("list", length = K)
    sigma <- vector("list", length = K)

    # Choose initialization method
    if (init_method == "kmeans") {
      # K-means initialization
      kmeans_result <- kmeans(X, centers = K)
      mu <- vector("list", length = K)
      for (k in 1:K) {
        mu[[k]] <- kmeans_result$centers[k, ]  # Each row becomes an element in the list
      }
      sigma <- lapply(1:K, function(x) diag(nb_features))  # Identity matrix for each cluster
    } else {
      # Random initialization
      for (k in 1:K) {
        mu[[k]] <- X[sample(1:nb_observations, 1), ]  # Randomly pick a data point as the mean
        sigma[[k]] <- diag(nb_features)  # Identity matrix for covariance (you can adjust this as needed)
      }
    }

    # Perform nb_iter_init_step iterations of EM
    for (iteration in 1:nb_iter_init_step) {
      # E-step: Compute responsibilities
      t_i_k <- E_step(X, K, proportions, mu, sigma)

      # M-step: Update parameters
      params <- M_step(X, t_i_k)

      # Update parameters after M-step
      proportions <- params$proportions
      mu <- params$mu
      sigma <- params$sigma
    }

    # Compute the loss after the iterations
    current_loss <- compute_loss(X, K, proportions, mu, sigma)
    cat("Loss for this initialization: ", current_loss, "\n")  # Print the loss

    # Update the best parameters if the current loss is lower
    if (current_loss > best_loss) {
      best_loss <- current_loss
      best_params <- list(proportions = proportions, mu = mu, sigma = sigma)
      best_initialization <- initialization
    }
  }

  # Print the best initialization and its loss
  cat("Best initialization:", best_initialization, "with loss:", best_loss, "\n")

  # Return the parameters corresponding to the lowest loss
  return(best_params)
}

E_step <- function(X, K, proportions, mu, sigma) {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]

  # Initialize likelihoods matrix
  likelihoods <- matrix(0, nrow = nb_observations, ncol = K)

  epsilon <- 1e-6  # Small value to prevent underflow

  # Compute likelihoods for each observation and cluster
  for (k in 1:K) {
    # Here, you should compute the multivariate normal density for each cluster
    likelihoods[, k] <- proportions[k] * dmvnorm(X, mean = mu[[k]], sigma = sigma[[k]] + epsilon )
  }


  # Sum over clusters for each observation and normalize (responsibilities)
  row_sums <- rowSums(likelihoods)  # Reverting back from log
  t_i_k <- sweep(likelihoods, 1, row_sums, FUN = "/")  # Normalize each row

  return(t_i_k)
}



M_step <- function(X, t_i_k, model = "basic") {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]
  K <- dim(t_i_k)[2]

  # Update proportions (ensure they sum to 1)
  proportions <- colSums(t_i_k) / nb_observations

  mu <- vector("list", length = K)
  sigma <- vector("list", length = K)
  common_sigma <- matrix(0, nb_features, nb_features)  # Initialize shared covariance matrix

  # Compute weighted means (mu) for each cluster
  for (k in 1:K) {
    mu[[k]] <- colSums(t_i_k[, k] * X) / sum(t_i_k[, k])
  }

  # Compute covariance matrices
  if (model == "homoscedastic") {
    # Compute a common covariance matrix
    total_weight <- 0
    for (k in 1:K) {
      diff <- sweep(X, 2, mu[[k]], "-")  # Center the data around the mean
      common_sigma <- common_sigma + t(diff) %*% (diff * t_i_k[, k])
      total_weight <- total_weight + sum(t_i_k[, k])
    }
    common_sigma <- common_sigma / total_weight

    # Assign the common covariance matrix to all classes
    for (k in 1:K) {
      sigma[[k]] <- common_sigma
    }
  } else if (model == "CEM") {
    # CEM-specific covariance update
    for (k in 1:K) {
      diff <- sweep(X, 2, mu[[k]], "-")  # Center the data around the mean
      sigma[[k]] <- t(diff) %*% (diff * t_i_k[, k]) / sum(t_i_k[, k])
    }
    # Add any additional CEM-specific updates for other parameters
  } else if (model == "SEM") {
    # SEM-specific covariance and parameter update logic
    for (k in 1:K) {
      diff <- sweep(X, 2, mu[[k]], "-")  # Center the data around the mean
      sigma[[k]] <- t(diff) %*% (diff * t_i_k[, k]) / sum(t_i_k[, k])
    }
    # Add any structural equation-specific updates for the SEM model
  } else {
    # Separate covariance matrices for each cluster
    for (k in 1:K) {
      diff <- sweep(X, 2, mu[[k]], "-")  # Center the data around the mean
      sigma[[k]] <- t(diff) %*% (diff * t_i_k[, k]) / sum(t_i_k[, k])
    }
  }

  return(list(proportions = proportions, mu = mu, sigma = sigma))
}



compute_loss <- function(X, K, proportions, mu, sigma) {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]
  log_loss <- 0

  # Use a vectorized approach to compute the loss
  likelihoods <- matrix(0, nrow = nb_observations, ncol = K)
  epsilon = 1e-6
  # Compute likelihood for each cluster and data point
  for (k in 1:K) {
    likelihoods[,k] <- proportions[k] * dmvnorm(X, mean = mu[[k]], sigma = sigma[[k]]+epsilon)
  }

    # Avoid log(0) by ensuring the likelihood is above epsilon
    log_loss <- sum(log(rowSums(likelihoods)))

  return(log_loss)
}

compute_nb_model_params <- function(K, model, nb_features){
  nb_model_parameters <- NA

  if (model == "basic"){
    nb_model_parameters <- K * (1 + nb_features + nb_features * (nb_features + 1) / 2)
  }

  if (model == "homoscedastic"){
    nb_model_parameters <- K * (1 + nb_features) + (nb_features * (nb_features + 1) / 2)
  }

  if (model == "CEM"){
    # Adjusted formula for CEM
    nb_model_parameters <- K * (1 + nb_features) + (nb_features * (nb_features + 1) / 2) + K
    # Add any latent variable parameters or additional complexity here
  }

  if (model == "SEM"){
    # Adjusted formula for SEM
    nb_model_parameters <- K * (1 + nb_features) + nb_features * (nb_features + 1) / 2
    # Add parameters for structural equations if needed
  }

  return(nb_model_parameters)
}

bic <- function(nb_observations, nb_model_parameters, log_loss){
  score <-  2 * log_loss-log(nb_observations) * nb_model_parameters
  return(score)
}

predict_gmm <- function(X_test, proportions, mu, sigma) {
  t_i_k <- E_step(X_test, K, proportions, mu, sigma)
  # Assign each data point to the cluster with the highest likelihood
  maximum_a_posteriori <- apply(t_i_k, 1, function(row) which.max(row))

  return(maximum_a_posteriori)
}

evaluate_gmm <- function(y_true, y_predic) {
  # Predicted clusters
  predicted_clusters <- y_predic

  # Find optimal correspondence between labels and clusters
  confusion_matrix <- table(predicted_clusters, y_true)
  matching <- solve_LSAP(confusion_matrix, maximum = TRUE)

  # Reassign the clusters according to the correspondence
  aligned_clusters <- matching[predicted_clusters]

  # Evaluate the accuracy
  accuracy <- mean(aligned_clusters == y_true)

  confusion_matrix <- table(aligned_clusters, y_true)

  # Return the results
  return(list(
    predicted_clusters = aligned_clusters,
    confusion_matrix = confusion_matrix,
    matching = matching,
    accuracy = accuracy
  ))
}
