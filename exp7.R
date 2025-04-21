install.packages("Metrics")

library(ggplot2)
library(Metrics)

# ---------------------------------------
# 1. Create Sample Sales Data
# ---------------------------------------
set.seed(123)
months <- 1:60
sales <- 100 + 2 * months + rnorm(60,0,10) 
df <- data.frame(months, sales)

# ---------------------------------------
# 2. Scatter Plot of Sales Over Time
# ---------------------------------------
ggplot(df, aes(x = months, y = sales)) +
  geom_point(color = "blue") +
  labs(title = "Sales Over Time", x = "Month", y = "Sales")

# ---------------------------------------
# 3. Linear Regression
# ---------------------------------------
model_linear <- lm(sales ~ months, data = df)
summary(model_linear)

# ---------------------------------------
# 4. Quadratic Regression
# ---------------------------------------
model_quad <- lm(sales ~ months + I(months^2), data = df)
summary(model_quad)

# ---------------------------------------
# 5. Compare Models: R-squared and RMSE
# ---------------------------------------
# Predictions
pred_linear <- predict(model_linear, newdata = df)
pred_quad <- predict(model_quad, newdata = df)
# R-squared
rsq_linear <- summary(model_linear)$r.squared
rsq_quad <- summary(model_quad)$r.squared
# RMSE
rmse_linear <- rmse(df$sales, pred_linear)
rmse_quad <- rmse(df$sales, pred_quad)
cat("Linear Model - R²:", rsq_linear, "| RMSE:", rmse_linear, "\n")
cat("Quadratic Model - R²:", rsq_quad, "| RMSE:", rmse_quad, "\n")

# ---------------------------------------
# 6. Visualize Both Models
# ---------------------------------------
df$linear_fit <- pred_linear
df$quad_fit <- pred_quad
ggplot(df, aes(x = months, y = sales)) +
  geom_point() +
  geom_line(aes(y = linear_fit), color = "red", linetype = "dashed") +
  geom_line(aes(y = quad_fit), color = "green") +
  labs(title = "Linear vs Quadratic Regression Fit", y = "Sales")

# ---------------------------------------
# 7. Predict Sales for Next 6 Months (Months 61–66)
# ---------------------------------------
future_months <- data.frame(months = 61:66)
future_preds <- data.frame(
  Month = 61:66,
  Linear_Prediction = predict(model_linear, newdata = future_months),
  Quadratic_Prediction = predict(model_quad, newdata = future_months)
)
print(future_preds)

# ---------------------------------------
# 8. Conclusion
# ---------------------------------------
if (rsq_quad > rsq_linear & rmse_quad < rmse_linear) {
  cat("\n Quadratic model fits the data better.\n")
} else {
  cat("\n Linear model fits the data better.\n")
}