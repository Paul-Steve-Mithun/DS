# Load required libraries
library(corrplot)


# Load the dataset
data("mtcars")


# Pearson
pearson_corr <- cor(mtcars, method = "pearson")
cat("Pearson correlation with mpg:\n")
print(pearson_corr)


eigen_decomp <- eigen(pearson_corr)
cat("\n Eigenvalues:\n")
print(eigen_decomp$values)
cat("\n Eigenvectors (Principal Components):\n")
print(eigen_decomp$vectors)