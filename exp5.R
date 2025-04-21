# Load required libraries
library(e1071) # for skewness
library(dplyr) # for data manipulation
library(ggplot2)

# Load 
data("mtcars")


# 1. Detect Outlier Using IQR Method
detect_iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  return(which(x < lower | x > upper))
}
iqr_outliers_mpg <- detect_iqr_outliers(mtcars$mpg)
iqr_outliers_hp <- detect_iqr_outliers(mtcars$hp)
iqr_outliers_wt <- detect_iqr_outliers(mtcars$wt)



# 2. Detect Outlier Using Z-Score Method
detect_zscore_outliers <- function(x, threshold = 3) {
  z <- scale(x)
  return(which(abs(z) > threshold))
}
z_outliers_mpg <- detect_zscore_outliers(mtcars$mpg)
z_outliers_hp <- detect_zscore_outliers(mtcars$hp)
z_outliers_wt <- detect_zscore_outliers(mtcars$wt)


# 3. Probabilities of Selecting an Outlier
p_mpg_iqr <- length(iqr_outliers_mpg) / nrow(mtcars)
p_hp_iqr <- length(iqr_outliers_hp) / nrow(mtcars)
p_wt_iqr <- length(iqr_outliers_wt) / nrow(mtcars)
cat("Probability of selecting an outlier (IQR method):\n")
cat("MPG:", p_mpg_iqr, " | HP:", p_hp_iqr, " | WT:", p_wt_iqr, "\n\n")


# 4. Conditional Probability (Example)
# P(Outlier in HP | Outlier in MPG)
outlier_mpg_hp <- intersect(iqr_outliers_mpg, iqr_outliers_hp)
p_cond <- length(outlier_mpg_hp) / length(iqr_outliers_mpg)
cat("P(HP is outlier | MPG is outlier):", p_cond, "\n\n")

# -------------------------------
# 5. Independence Check (Simplified)
# If P(A ∩ B) ≈ P(A) * P(B), then independent
# -------------------------------
p_hp <- p_hp_iqr
p_mpg <- p_mpg_iqr
p_both <- length(outlier_mpg_hp) / nrow(mtcars)
cat("P(HP ∩ MPG):", p_both, "\n")
cat("P(HP) * P(MPG):", p_hp * p_mpg, "\n\n")

# -------------------------------
# 6. Skewness and Distribution Shapes
# -------------------------------
skew_mpg <- skewness(mtcars$mpg)
skew_hp <- skewness(mtcars$hp)
skew_wt <- skewness(mtcars$wt)
cat("Skewness:\nMPG:", skew_mpg, "| HP:", skew_hp, "| WT:", skew_wt, "\n\n")


# 7. Correlation Matrix
cor_matrix <- cor(mtcars)
print(cor_matrix)


# 8. Compare Outlier Detection Methods
compare_outliers <- function(iqr_idx, z_idx) {
  both <- intersect(iqr_idx, z_idx)
  only_iqr <- setdiff(iqr_idx, z_idx)
  only_z <- setdiff(z_idx, iqr_idx)
  list(
    common = mtcars[both, ],
    only_iqr = mtcars[only_iqr, ],
    only_z = mtcars[only_z, ]
  )
}
result_mpg <- compare_outliers(iqr_outliers_mpg, z_outliers_mpg)
cat("Common Outliers (MPG):", rownames(result_mpg$common), "\n")


# 9. Plot Distribution
ggplot(mtcars, aes(x=mpg)) + geom_histogram(bins=10, fill="skyblue") +
  geom_vline(xintercept=quantile(mtcars$mpg, c(0.25, 0.75)), linetype="dashed") +
  labs(title="MPG Distribution with IQR Lines")
ggplot(mtcars, aes(x=hp)) + geom_density(fill="lightgreen", alpha=0.6) +
  labs(title="Density Plot of Horsepower")
ggplot(mtcars, aes(x=wt)) + geom_boxplot(fill="orange") +
  labs(title="Boxplot of Weight")