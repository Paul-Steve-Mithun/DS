# Load required packages
install.packages("corrplot")
install.packages("reshape2")
install.packages("vioplot")

library(ggplot2)
library(reshape2)
library(vioplot)
library(dplyr)
library(corrplot)

# Load mtcars dataset
data("mtcars")

# Identify variable types
str(mtcars)

# Numerical: mpg, hp, wt, disp, etc.
# Convert some variables to categorical for better plotting
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

# 1. Box plots
boxplot(mtcars$mpg, main="Boxplot of MPG", col="lightblue")

# 2. Histogram
hist(mtcars$hp, breaks=10, col="lightgreen", main="Histogram of Horsepower", xlab="HP")

# 3. Violin plot
vioplot::vioplot(mtcars$wt, col="orange", names="Weight", main="Violin Plot of Weight")

# 4. Scatter plot with trend line: mpg vs wt
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "MPG vs Weight", x = "Weight", y = "Miles per Gallon") + 
  theme(legend.position = "top")

# 5. Faceted scatter: mpg vs hp by number of cylinders
ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(aes(color=cyl), size=3) +
  facet_wrap(~ cyl) +
  labs(title = "MPG vs HP by Cylinders")

# 6. Bar plot: Count of cars by gear
ggplot(mtcars, aes(x=gear)) +
  geom_bar(fill="purple") +
  labs(title="Number of Cars by Gear", x="Gear", y="Count")

# 7. Correlation heatmap
cor_matrix <- cor(mtcars)
corrplot(cor_matrix, method="color", addCoef.col = "black",
         tl.col = "black", number.cex = 0.7, main = "Correlation Heatmap")