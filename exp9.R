heart <- read.csv("C:/Users/drums/OneDrive/Desktop/SEM VI/DS Lab/DS_SEM/heart.csv")

# Train-test split
set.seed(123)

train_idx <- sample(1:nrow(heart), 0.7 * nrow(heart))
train <- heart[train_idx, ]
test <- heart[-train_idx, ]

# Logistic regression
model <- glm(target ~ ., data=train, family="binomial")
pred <- predict(model, newdata=test, type="response")
pred_class <- ifelse(pred > 0.5, 1, 0)

# Evaluation
conf_matrix <- table(Predicted=pred_class, Actual=test$target)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# PCA
pca <- prcomp(train[,-ncol(train)], scale. = TRUE)
summary(pca)