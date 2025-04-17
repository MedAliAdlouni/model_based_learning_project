# Perform PCA
pca_result <- prcomp(mnist_Xtrain)

# Calculate the explained variance ratio
explained_variance_ratio <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Set the numbers of components you want to evaluate
num_components <- c(20, 50, 75, 100, 150, 200, 250, 300)

# Initialize a vector to store the cumulative variance explained for each case
variance_explained <- numeric(length(num_components))

# Calculate the cumulative variance explained for each number of components
for (i in seq_along(num_components)) {
  variance_explained[i] <- sum(explained_variance_ratio[1:num_components[i]])
}

# Create the plot
library(ggplot2)

# Create a data frame for plotting
variance_df <- data.frame(
  Components = num_components,
  Variance_Explained = variance_explained
)

# Plot the variance explained with detailed ticks
ggplot(variance_df, aes(x = Components, y = Variance_Explained)) +
  geom_line() +
  geom_point() +
  labs(title = "Variance Explained by Principal Components",
       x = "Number of Components",
       y = "Cumulative Variance Explained") +
  scale_x_continuous(breaks = seq(50, 300, by = 50)) +  # Add more detailed ticks on the X axis
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +  # Add more detailed ticks on the Y axis
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 16))
