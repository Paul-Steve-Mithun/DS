# Load required libraries
library(ggplot2)
library(corrplot)
library(reshape2)
library(stats)

# Load the dataset
data("mtcars")

# -------------------------------
# 1. Pearson & Spearman Correlations with mpg
# -------------------------------
# Pearson
pearson_corr <- cor(mtcars$mpg, mtcars[, -1], method = "pearson")
cat("Pearson correlation with mpg:\n")
print(pearson_corr)

# Spearman
spearman_corr <- cor(mtcars$mpg, mtcars[, -1], method = "spearman")
cat("\nSpearman correlation with mpg:\n")
print(spearman_corr)

# -------------------------------
# 2. Identify Key Influencing Variables
# -------------------------------
# Sort Pearson correlations
sorted_pearson <- sort(pearson_corr, decreasing = TRUE)
cat("\nMost positively and negatively correlated with mpg:\n")
print(sorted_pearson)
# Observation: Strongest negative correlation → wt, hp
# Positive → qsec (acceleration)

# -------------------------------
# 3. Correlation Heatmap
# -------------------------------
cor_matrix <- cor(mtcars, method = "pearson")
corrplot(cor_matrix, method="color", addCoef.col = "black",
         tl.col = "black", number.cex = 0.7, main = "Correlation Heatmap")

# -------------------------------
# 4. Scatter Plots: mpg vs key variables
# -------------------------------
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(color="blue") +
  geom_smooth(method="lm", col="red") +
  labs(title="MPG vs Weight", x="Weight", y="MPG")
ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(color="darkgreen") +
  geom_smooth(method="lm", col="red") +
  labs(title="MPG vs Horsepower", x="Horsepower", y="MPG")

# -------------------------------
# 5. Box Plot: mpg by number of cylinders
# -------------------------------
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(x=cyl, y=mpg, fill=cyl)) +
  geom_boxplot() +
  labs(title="MPG by Number of Cylinders", x="Cylinders", y="MPG")
# -------------------------------
# 6. Bar Plot: Average MPG by gear
# -------------------------------
ggplot(mtcars, aes(x=gear, y=mpg, fill=gear)) +
  geom_bar(stat="identity") +
  labs(title="Average MPG by Gear", x="Gear", y="Average MPG")

# -------------------------------
# 7. Clustering Dendrogram
# -------------------------------
# Compute distances between features
dist_matrix <- dist(scale(mtcars))
clust <- hclust(dist_matrix)
plot(clust, main = "Hierarchical Clustering Dendrogram")