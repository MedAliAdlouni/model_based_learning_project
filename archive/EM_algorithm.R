library(R6)
library(mvtnorm)


em_algorithm <- function(X, K, model = "basic", nb_initializations = 10, max_iterations = 100, loss_treshold = 1e-6, plot_losses = TRUE, nb_iter_init_step=10) {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]
  
  nb_model_parameters <- compute_nb_model_params(K, model, nb_features)
  cat("nb of model parameters: ", nb_model_parameters, "\n")  
  
  params <- initialize_parameters(X, K, nb_initializations, nb_iter_init_step)
  
  proportions <- params$proportions
  mu <- params$mu
  sigma <- params$sigma
  
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
    params <- M_step(X, t_i_k)
    
    # Update parameters after M-step
    proportions <- params$proportions
    mu <- params$mu
    sigma <- params$sigma
    
    # Compute loss
    current_loss <- compute_loss(X, K, proportions, mu, sigma)
    losses[iteration + 1] <- current_loss
    
    cat("Current Loss: ", current_loss, "\n")  # Print current loss
    
    # Check for convergence
    if (abs(losses[iteration + 1] - losses[iteration]) < loss_treshold) {
      cat("Converged at iteration", iteration, "with loss", current_loss, "\n")
      break
    }
  }
  losses <- losses[1:iteration]
  
  bic <- bic(nb_observations, nb_model_parameters, tail(losses, 1))
  
  if (plot_losses){
    plot(losses, type="o", col="blue", xlab="Iteration", ylab="Negative Log-Loss", main="Loss Evolution in EM Algorithm")
    
  }
  maximum_a_posteriori <- apply(last_t_i_k, 1, function(row) which.max(row))
  print(maximum_a_posteriori)
  
  # Return the results
  return(list(losses = losses, proportions = proportions, mu = mu, sigma = sigma, last_t_i_k = last_t_i_k, bic = bic, maximum_a_posteriori=maximum_a_posteriori))
}

  
initialize_parameters <- function(X, K, nb_initializations, nb_iter_init_step) {
  nb_observations <- dim(X)[1]
  nb_features <- dim(X)[2]
  
  best_loss <- Inf  # Start with a very high value
  best_params <- NULL  # Placeholder for the best parameters
  best_initialization <- NULL  # Placeholder for the best initialization
  
  for (initialization in 1:nb_initializations) {
    cat("Initialization number: ", initialization, "\n")  # Print initial loss
    
    # Initialize parameters for this run
    proportions <- rep(1 / K, K)
    mu <- vector("list", length = K)
    sigma <- vector("list", length = K)
    
    for (k in 1:K) {
      mu[[k]] <- rnorm(nb_features, mean = 0, sd = 1)
      sigma[[k]] <- diag(nb_features)  # Identity matrix
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
    
    # Compute the loss after 10 iterations
    current_loss <- compute_loss(X, K, proportions, mu, sigma)
    cat("loss for this initialization: ", current_loss, "\n")  # Print initial loss
    
    cat("best loss: ", best_loss, "\n")  
    
    # Check if this is the best loss so far
    if (current_loss < best_loss) {
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


E_step <- function(X, K, proportions, mu, sigma){
  nb_observations <- dim(X)[1]
  nb_features <-  dim(X)[2]
  likelihoods <- matrix(0, nrow = nb_observations, ncol = K)

  for (i in 1:nb_observations){
    for (k in 1:K){
      likelihoods[i, k] = proportions[k] * dmvnorm(X[i, ], mean = mu[[k]], sigma = sigma[[k]] )
    }
  }
  
  # Print likelihoods for debugging
  #print("Likelihoods matrix:")
  #print(likelihoods)
  
  row_sums <- rowSums(likelihoods)
  t_i_k <- sweep(likelihoods, 1, row_sums, FUN = "/")
  
  # Print t_i_k for debugging
  #print("Responsibilities matrix (t_i_k):")
  #print(t_i_k)
  
  return(t_i_k)
}


M_step <- function(X, t_i_k){
  nb_observations <- dim(X)[1]
  nb_features <-  dim(X)[2]
  K <- dim(t_i_k)[2]
  
  proportions <- rep(1/K, K)
  mu <- vector("list", length = K)
  sigma <- vector("list", length = K)
  
  col_sums <- colSums(t_i_k)
  proportions <- col_sums / nb_observations
  
  for (k in 1:K){
    mu[[k]] <- rep(0, nb_features)
    for (i in 1:nb_observations){
     mu[[k]] <- mu[[k]] + t_i_k[i,k] * X[i, ]
    }
    mu[[k]] <- mu[[k]] / col_sums[k]
  }
  
  
  for (k in 1:K){
    sigma[[k]] <- matrix(0, nrow = nb_features, ncol = nb_features)
    for (i in 1:nb_observations){
      diff <- X[i, ] - mu[[k]]
      sigma[[k]] <- sigma[[k]] + t_i_k[i,k] * (diff %*% t(diff))
    }
    sigma[[k]] <- sigma[[k]] / col_sums[k]
    
  }
  list(proportions=proportions, mu=mu, sigma=sigma)
}


compute_loss <- function(X, K, proportions, mu, sigma){
  nb_observations <- dim(X)[1]
  nb_features <-  dim(X)[2]
  log_loss <- 0 
  epsilon <- 1e-10
  
    for (i in 1:nb_observations){
    loss <- 0
    for (k in 1:K){
      loss <- loss + proportions[k] * dmvnorm(X[i, ], mean = mu[[k]], sigma = sigma[[k]] )
      
    }
    loss <- max(loss, epsilon)
    log_loss <- log_loss + log(loss)
  }
  neg_log_loss <- -log_loss / nb_observations
  
  return(neg_log_loss)
  
}


compute_nb_model_params <- function(K, model, nb_features){
  nb_model_parameters <- NA
  if (model == "basic"){
    nb_model_parameters <- K*(1 + nb_features + nb_features*(nb_features-1)/2)
  }
  
  return(nb_model_parameters)
}


bic <- function(nb_observations, nb_model_parameters, neg_log_loss){
  score <- log(nb_observations) * nb_model_parameters + 2 * neg_log_loss
  return(score)
}