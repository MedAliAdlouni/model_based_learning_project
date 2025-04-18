# Model-Based Learning Project

This project implements a **custom Expectation-Maximization (EM) R package** for clustering using **Gaussian Mixture Models (GMMs)**. It supports flexible covariance structures and is evaluated on both low- and high-dimensional datasets, including Iris and MNIST.

------------------------------------------------------------------------

## 📌 Objectives

-   Implement GMM clustering with **full** and **homoscedastic** covariance models\
-   Apply the EM algorithm with **multiple initializations** and **convergence tracking**\
-   Use **Bayesian Information Criterion (BIC)** for model selection\
-   Evaluate clustering performance on **Iris** and **MNIST** datasets using confusion matrices and accuracy

------------------------------------------------------------------------

## 📁 Project Structure

| Path                        | Description                                   |
|-----------------------------|-----------------------------------------------|
| `MyGMMPackage_0.1.0.tar.gz` | Main R package implementing the EM algorithm  |
| `R/`                        | All R scripts for loading data and running EM |
| `data/`                     | Input datasets (Iris, MNIST)                  |
| `results/`                  | Output plots, confusion matrices, etc.        |
| `docs/`                     | Project report and description (PDF format)   |
| `renv.lock`                 | Reproducible environment snapshot             |
| `requirements.txt`          | List of required R packages                   |

------------------------------------------------------------------------

## 📊 Dataset Summary

| Dataset | Size   | Features                     | Classes |
|---------|--------|------------------------------|---------|
| Iris    | 150    | 4                            | 3       |
| MNIST   | 5,000+ | 784 → 3/50 (after t-SNE/PCA) | 10      |

> MNIST dimensionality was reduced using PCA and t-SNE for memory size issues.

------------------------------------------------------------------------

## 🧠 GMM Model Variants

-   **basic**: Full covariance matrix per cluster\
-   **homoscedastic**: Shared covariance matrix across clusters\
-   Also supports experimental variants: **CEM** and **SEM**

------------------------------------------------------------------------

## 🧪 Running the Code

1.  **Clone the repo**

    ``` bash
    git clone https://github.com/MedAliAdlouni/model_based_learning_project.git
    cd model_based_learning_project
    ```

2.  **Install required R packages**

``` bash
pkgs <- readLines("requirements.txt")
install.packages(pkgs)
```

3.  **Run EM on Iris dataset**

``` bash
source("run_EM_algorithm_IRIS_dataset.R")
```

4.  **Run EM on MNIST dataset**

``` bash
source("run_EM_algorithm_MNIST_dataset.R")
```

5.  **Open project report**

Open `report_ELADLOUNI_MOHAMMEDALI.pdf` for detailed methodology, results, and analysis.
