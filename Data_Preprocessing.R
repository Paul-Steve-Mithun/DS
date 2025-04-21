data<-read.csv("C:/Users/drums/OneDrive/Desktop/SEM V/EDA LAB/Semester/SalesData.csv")

#Check the Names and Data Types of Variables (Columns) 
str(data)

# Identify Variables with No Usable Statistical or Graphical Information 
null_values <- is.na(data) 
rows_with_na <- apply(null_values, 1, any)
rows_with_na_data <- data[rows_with_na, ]
rows_with_na_data

# Identify Variables with Missing Data
summary(null_values)
missing_data_summary <- colSums(null_values)
missing_data_summary[missing_data_summary > 0]

#Data Imputation
#a. Delete All Rows with Missing Data 
s1<-na.omit(data)
s1

#b. Replace Missing Values with the Mean Value of Each Variable 
data$Sales[is.na(data$Sales)] <- mean(data$Sales, na.rm = TRUE) 
data$Profit[is.na(data$Profit)] <- mean(data$Profit, na.rm = TRUE) 
data$Unit.Price[is.na(data$Unit.Price)] <- mean(data$Unit.Price, na.rm = TRUE)
summary(data)

#c. Replace Missing Values with Random Values 
data$Sales[is.na(data$Sales)] <- runif(n = sum(is.na(data$Sales)),
          min = min(data$Sales, na.rm = TRUE), 
          max = max(data$Sales, na.rm = TRUE))
summary(data)

#d. Replace categorical variables in SalesData.csv
categorical_vars <- sapply(data, function(x) is.factor(x) || is.character(x))
categorical_vars <- names(data)[categorical_vars]
categorical_vars
set.seed(123)  
for (col in categorical_vars) {
  missing_indices <- which(is.na(data[[col]]))  
  if (length(missing_indices) > 0) {
    non_missing_values <- data[[col]][!is.na(data[[col]])]  
    data[[col]][missing_indices] <- sample(non_missing_values, length(missing_indices), replace = TRUE)
  }
}
colSums(is.na(data))

#Data Exploration
#boxplot 
numeric_columns <- sapply(s1, is.numeric)
par(mfrow = c(1, sum(numeric_columns)))
for (column in colnames(s1)[numeric_columns]) {
  boxplot(s1[[column]], main = column, col = "lightblue", outline = TRUE)}

# Identify outliers in Order Priority 
summary(data$Order.Priority) 
barplot(table(data$Order.Priority)) 

# Distribution table for categorical variables 
table(data$Order.Priority) 
table(data$Product.Category)
barplot(table(data$Product.Category))

# Histograms for numeric variables 
hist(data$Sales) 
hist(data$Shipping.Cost)

# Test for normality 
shapiro.test(data$Sales) 
shapiro.test(data$Shipping.Cost)
# Transform variables 
data$Sales_sqrt <- sqrt(data$Sales)
data$shipping_log <- log(data$Shipping.Cost)
# Calculate before/after skewness values 
install.packages("e1071")
library(e1071)
skewness(data$Sales)
skewness(data$Sales_sqrt) 
skewness(data$Shipping.Cost) 
skewness(data$shipping_log)

# Create quantile-quantile plots 
qqnorm(data$Sales) 
qqline(data$Sales) 
qqnorm(data$Sales_sqrt) 
qqline(data$Sales_sqrt) 
qqnorm(data$Shipping.Cost) 
qqline(data$Shipping.Cost) 
qqnorm(data$shipping_log) 
qqline(data$shipping_log)


#Z-Score Normalization
z_score_standardized <- scale(numeric_columns)
z_score_standardized

# Min-Max Normalization function
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
min_max_normalized <- as.data.frame(lapply(data[numeric_columns], min_max_normalize))
head(min_max_normalized)



#Creating New Variables
#a. Derive a Flag Variable
data$Profit.flag <- ifelse(data$Profit > 0, 1, 0) 
# Distribution table for Profit.flag 
table(data$Profit.flag)

# Derive the new variable T.cost
data$T.cost <- data$`Order.Quantity` * data$`Unit.Price` + data$`Shipping.Cost`
# Plot histogram for T.cost
hist(data$T.cost, main="Histogram of Total Cost (T.cost)", xlab="Total Cost", col="skyblue", border="black")