# Install and load the pls package
install.packages("pls")
library(pls)

# Sample data
data <- data.frame(
  sqft = c(1000, 1500, 2000, 2500, 1800),
  bedrooms = c(2, 3, 4, 4, 3),
  location = c(1, 2, 3, 2, 1),
  price = c(200000, 300000, 400000, 450000, 330000)
)

# Manual Normal Equation
X <- as.matrix(cbind(Intercept = 1, data$sqft, data$bedrooms, data$location))
Y <- as.matrix(data$price)

beta <- solve(t(X) %*% X) %*% t(X) %*% Y
print(beta)

# Fit PCR model
pcr_model <- pcr(price ~ sqft + bedrooms + location, 
                 data = data, 
                 scale = TRUE, 
                 validation = "CV", 
                 segments = 2)
summary(pcr_model)

pred <- predict(pcr_model, ncomp = 1)
print(pred)
