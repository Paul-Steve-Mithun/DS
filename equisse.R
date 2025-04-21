install.packages("esquisse")
library(esquisse)

# Load mtcars
data("mtcars")


library(ggplot2)
barplot(table(mtcars$cyl), main="Frequency of Cylinders", xlab="Cylinders", ylab="Frequency")



