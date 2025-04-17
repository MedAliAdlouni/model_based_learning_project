# Gaussian Mixture Model Clustering Report

This report explores the implementation of a Gaussian Mixture Model (GMM) for clustering quantitative data, utilizing the Expectation-Maximization (EM) algorithm. Two variants of the model are developed: 

1. A **full model** with unrestricted covariance matrices.
2. A **homoscedastic model** with shared covariance matrices across clusters.

The implementation is encapsulated within an R package, which includes functionality for parameter estimation, cluster assignment, and model selection via the Bayesian Information Criterion (BIC). The methodology is applied to the **Iris** and **MNIST** datasets, and the results are evaluated for clustering performance.

## Project Structure

Here’s an overview of the folder and file structure in the project:

### Root Directory
- `.gitattributes` - Git configuration for file attributes
- `.Rhistory` - R session history
- `mnist_data_load.R` - Script for loading MNIST dataset
- `MyGMMPackage_0.1.0.tar.gz` - Compiled R package containing the GMM implementation
- `project.Rproj` - RStudio project file
- `report_ELADLOUNI_MOHAMMEDALI.pdf` - Final project report
- `run_EM_algorithm_IRIS_dataset.R` - Script for running the EM algorithm on the Iris dataset
- `run_EM_algorithm_MNIST_dataset.R` - Script for running the EM algorithm on the MNIST dataset

### Archive Directory
This folder contains older versions and additional scripts related to the Gaussian Mixture Model:

- `EM_algorithm.R` - Original EM algorithm implementation
- `EM_algorithm_.R` - Alternate version of the EM algorithm
- `GMM_classification_algo.R` - Algorithm for classification using GMM
- `GMM_classification_run.R` - Script for running the classification algorithm
- `GMM_clustering_algo.R` - Algorithm for clustering using GMM
- `GMM_clustering_run.R` - Script for running the clustering algorithm
- `temp.R` - Temporary scripts for testing

### Data Directory
Contains datasets used in the project:

- `.Rhistory` - R session history for data manipulation
- `mnist.RData` - Preprocessed MNIST data
- `t10k-images-idx3-ubyte` - Test images for MNIST dataset
- `t10k-labels-idx1-ubyte` - Test labels for MNIST dataset
- `train-images-idx3-ubyte` - Training images for MNIST dataset
- `train-labels-idx1-ubyte` - Training labels for MNIST dataset

### Plots Directory
This folder contains visualizations related to the GMM clustering and evaluation:

- `bic_scores.png` - Plot showing BIC scores for model selection
- `different_init.png` - Plot comparing clustering results with different initializations
- `IRIS8BIC.png` - BIC score plot for the Iris dataset
- `iris_bic` - Additional BIC plot for the Iris dataset
- `loss_iris.png` - Loss plot for clustering on the Iris dataset
- `mnist_bic.png` - BIC score plot for the MNIST dataset
- `pca_vs_tsne.png` - Comparison of PCA and t-SNE dimensionality reduction techniques
- `variance_explained_byPCA.png` - Plot showing variance explained by PCA

## Methodology

### Gaussian Mixture Model (GMM)
We implement two variants of the Gaussian Mixture Model:

- **Full Model**: Unrestricted covariance matrices for each cluster.
- **Homoscedastic Model**: Shared covariance matrices across all clusters.

The EM algorithm is used for parameter estimation and cluster assignment. The model is evaluated using the Bayesian Information Criterion (BIC), which helps select the optimal number of clusters.

### Datasets
- **Iris Dataset**: A popular dataset for classification and clustering tasks, containing measurements of different flower species.
- **MNIST Dataset**: A large dataset of handwritten digits commonly used for machine learning tasks.

### Scripts and Functions
- `run_EM_algorithm_IRIS_dataset.R`: Runs the GMM EM algorithm on the Iris dataset.
- `run_EM_algorithm_MNIST_dataset.R`: Runs the GMM EM algorithm on the MNIST dataset.

## Installation

To install the R package and run the project, follow these steps:

1. Install the required dependencies in R (see `MyGMMPackage_0.1.0.tar.gz` for the package).
2. Load the dataset using the provided data files in the `data` directory.
3. Use the `run_EM_algorithm_IRIS_dataset.R` or `run_EM_algorithm_MNIST_dataset.R` scripts to run the GMM algorithm.
4. Evaluate the results using the BIC and visualize the plots in the `plots` folder.

## Results

The results of the clustering, along with evaluation metrics and plots, are provided in the `plots` directory. Key visualizations include:

- BIC scores for model selection.
- Loss plots for both Iris and MNIST datasets.
- Comparison of PCA and t-SNE dimensionality reduction.

## Conclusion

This project demonstrates the implementation and application of the Expectation-Maximization algorithm in clustering quantitative data using a Gaussian Mixture Model. The method is tested on the Iris and MNIST datasets, and performance is evaluated using BIC scores and visualizations.

For more details, refer to the full report `report_ELADLOUNI_MOHAMMEDALI.pdf`.
