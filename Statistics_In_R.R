#Univariate Analysis
#1.What are the summary statistics of each variable?
data("mtcars")
summary(mtcars)

#2.What is the distribution of miles per gallon (mpg)? (Histogram)
hist(mtcars$mpg, main="Distribution of MPG", xlab="Miles per Gallon")

#3.What is the frequency of cars with different numbers of cylinders? (Bar plot)
barplot(table(mtcars$cyl), main="Frequency of Cylinders", xlab="Cylinders", ylab="Frequency")

#Bivariate Analysis
#4.Is there a correlation between mpg and hp (horsepower)? (Scatter plot with trend line)
plot(mtcars$hp, mtcars$mpg); 
abline(lm(mpg ~ hp, data = mtcars), col="gray")

#5.How does mpg vary across cars with different numbers of cylinders? (Box plot)
boxplot(mpg ~ cyl, data=mtcars, main="MPG by Cylinder", xlab="Cylinders", ylab="MPG")

#6.Is there an association between transmission type (am: 0 = automatic, 1 = manual) 
#and the number of gears (gear)? (Stacked bar plot)
barplot(table(mtcars$am, mtcars$gear), beside=FALSE, legend=TRUE, main="Transmission vs Gears")

#Multivariate Analysis
#7.How do mpg, hp, and wt (weight) interact? (Scatter plot with color gradient)
plot(mtcars$hp, mtcars$mpg, col=mtcars$wt, pch=16, main="MPG vs HP with Weight", xlab="Horsepower", ylab="Miles per Gallon")

#8.Can we predict mpg based on hp and wt using linear regression?
lm(mpg ~ hp + wt, data=mtcars)

#9.what is the pairwise correlation between all numerical variables in the dataset?
cor(mtcars)

#Final Step
#10.What is the pairwise correlation between all numerical variables? (Pair plot or correlation matrix heatmap)
heatmap(cor(mtcars), symm=TRUE, main="Correlation Heatmap")
