library(ggplot2)      # For scatter plots, bar plots, box plots, facets
library(corrplot)     # For correlation heatmap
library(e1071)        # For skewness calculation
library(caret)        # For confusion matrix and evaluation metrics

# Modeling & Statistics
library(pls)          # For Principal Component Regression (PCR)
library(car)          # For checking multicollinearity using VIF

# Optional (for building a Shiny App - only if you plan to run the app)
library(shiny)

# Load airquality dataset
data <- read.csv("D:/DS/AirQuality.csv",sep=";")


# 1. Data Exploration and Preprocessing
data("airquality")
str(airquality)
summary(airquality)
head(airquality)

# Handle missing values
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)

# Convert Month to factor
airquality$Month <- factor(airquality$Month)

# Scale numeric features
airquality_scaled <- airquality
airquality_scaled[, 1:4] <- scale(airquality[, 1:4])

# Engineer new feature: Temp in Celsius
airquality_scaled$Temp_C <- (airquality$Temp - 32) * 5/9

write.csv(airquality_scaled, "preprocessed_airquality.csv", row.names = FALSE)

# 2. Visualizations using mtcars
data("mtcars")
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)

hist(mtcars$mpg, col="skyblue", main="Histogram of MPG", xlab="Miles per Gallon")
barplot(table(mtcars$am), col="lightgreen", main="Automatic vs Manual Cars")
boxplot(mtcars$hp ~ mtcars$cyl, col="orange", main="Horsepower by Cylinders")

library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl)) + geom_point(size=3) + theme_minimal()
barplot(table(mtcars$gear), main="Car Counts by Gear", col="steelblue")

# 3. Scatter Plot with Trend Line
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() + 
  geom_smooth(method="lm", se=FALSE, col="red")

# 4. Facets, Bar Plots, Heatmaps
# Facet histogram of mpg by gear
ggplot(mtcars, aes(mpg)) + 
  geom_histogram(binwidth = 2, fill = "skyblue") + 
  facet_wrap(~gear)

# Correlation heatmap (numeric columns only)
library(corrplot)
num_mtcars <- mtcars[sapply(mtcars, is.numeric)]
corrplot(cor(num_mtcars), method = "color")


# 5. Shiny App (code only, to be run in shiny environment)
library(shiny)

ui <- fluidPage(
  titlePanel("AirQuality App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "X-axis:", names(airquality)[sapply(airquality, is.numeric)]),
      selectInput("y", "Y-axis:", names(airquality)[sapply(airquality, is.numeric)])
    ),
    mainPanel(
      verbatimTextOutput("sum"),
      plotOutput("scatter"),
      plotOutput("box")
    )
  )
)

server <- function(input, output) {
  output$sum <- renderPrint({ summary(airquality) })
  output$scatter <- renderPlot({
    plot(airquality[[input$x]], airquality[[input$y]], 
         main = "Scatter Plot", xlab = input$x, ylab = input$y, col = "blue")
  })
  output$box <- renderPlot({
    boxplot(airquality$Ozone, main = "Boxplot of Ozone", col = "orange")
  })
}

shinyApp(ui, server)


# 6. Distribution, Outliers, Skewness
hist(airquality$Ozone)
boxplot(airquality$Ozone)
e1071::skewness(airquality$Ozone)

# 7. Pearson and Spearman Correlations + Visuals

# Ensure Month is retained before removing NAs
num_data <- airquality[, c("Ozone", "Temp", "Month", "Wind", "Solar.R")]
num_data <- na.omit(num_data)

# Convert Month to factor for plotting
num_data$Month <- factor(num_data$Month)

# Pearson and Spearman correlation
cor_pearson <- cor(num_data$Ozone, num_data$Temp, method = "pearson")
cor_spearman <- cor(num_data$Ozone, num_data$Temp, method = "spearman")

cat("Pearson Correlation (Ozone vs Temp):", cor_pearson, "\n")
cat("Spearman Correlation (Ozone vs Temp):", cor_spearman, "\n")

# Scatter plot
plot(num_data$Ozone, num_data$Temp,
     main = "Ozone vs Temperature", xlab = "Ozone", ylab = "Temperature",
     col = "blue", pch = 19)

# Box Plot
boxplot(Ozone ~ Month, data = num_data,
        main = "Ozone Levels by Month", xlab = "Month", ylab = "Ozone",
        col = "lightgreen")

# Bar Plot of Average Ozone per Month
avg_ozone <- tapply(num_data$Ozone, num_data$Month, mean)
barplot(avg_ozone,
        main = "Average Ozone by Month", xlab = "Month", ylab = "Mean Ozone",
        col = "orange")

# Correlation Heatmap
library(corrplot)
corr_matrix <- cor(num_data[, c("Ozone", "Temp", "Wind", "Solar.R")])
corrplot(corr_matrix, method = "color")


# 8. Principal Component Regression (PCR)
library(pls)
pcr_model <- pcr(Ozone ~ ., data=na.omit(airquality), scale=TRUE, validation="CV")
summary(pcr_model)

# 9. Linear vs Quadratic Regression
lm1 <- lm(mpg ~ wt, data=mtcars)
lm2 <- lm(mpg ~ wt + I(wt^2), data=mtcars)
summary(lm1)$r.squared
summary(lm2)$r.squared
sqrt(mean(lm1$residuals^2))
sqrt(mean(lm2$residuals^2))

# 10. Correlation Matrix and Feature Selection
num_data <- airquality[sapply(airquality, is.numeric)]
cat("Correlation Matrix:\n")
print(cor(num_data, use = "complete.obs"))


# 11. Logistic Regression + PCA
df <- na.omit(airquality)
df$HotDay <- ifelse(df$Temp > 80, 1, 0)  # Binary target

model <- glm(HotDay ~ Ozone + Wind + Solar.R, data = df, family = binomial)
summary(model)

# PCA
pca <- prcomp(df[, c("Ozone", "Wind", "Solar.R")], scale. = TRUE)
summary(pca)


# 12. Matrix Form of Linear Regression
X <- as.matrix(cbind(1, mtcars$wt, mtcars$hp))
y <- as.matrix(mtcars$mpg)
beta <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta)

# 13. Distribution + Conditional Probability Insight
hist(airquality$Temp)
boxplot(airquality$Temp)

# 14. Regression Coefficients and Multicollinearity
lm_model <- lm(mpg ~ wt + hp + disp, data=mtcars)
summary(lm_model)
car::vif(lm_model)

# 15. Cyl vs. HP Plot
boxplot(mtcars$hp ~ mtcars$cyl, main="HP by Cylinders", col="purple")

# 16. Logistic Regression Evaluation
model <- glm(am ~ mpg + wt, data=mtcars, family=binomial)
summary(model)

# 17. Handle Missing Values + Convert Factors
cat("Variables:\n")
str(airquality)
cat("\nHandling Missing Values:\n")
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)
cat("Missing values handled.\n")

# Convert categorical
airquality$Month <- factor(airquality$Month)
cat("Converted 'Month' to factor.\n")

# 18. mpg vs. wt colored by cyl + gear barplot
ggplot(mtcars, aes(wt, mpg, color=cyl)) + geom_point()
barplot(table(mtcars$gear), col="gold")

# 19. Imputation Methods
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE)
cat("Missing values in Q19 handled again.\n")

# 20. PCA for Dimensionality Reduction
pca <- prcomp(na.omit(airquality[,1:4]), scale. = TRUE)
summary(pca)
plot(pca$x[,1:2], col=airquality$Month)
