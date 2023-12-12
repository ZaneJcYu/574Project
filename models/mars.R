library(earth)
library(magrittr)
library(caret)
library(dplyr)

# Seed for reproducibility
set.seed(1)

# Read in data
data <- read.csv("CD8Tcell_screened1.csv")
names(data)[2] <- "condition"
data$condition <- as.factor(data$condition)
train <- data[which(data$train == 1),]
test <- data[which(data$train == 0),]

# Get rid of train/test indicators in train and test sets
train <- select(train, -413)
test <- select(test, -413)

# MARS Model
mars <- earth(condition~., data=train[,-1], degree=1, nprune=22)

# Predictions on Training Data
yhat <- predict(mars, newdata=test[,-1], type="class")

# Test error
test_err <- mean(yhat != test$condition)
print(paste("The test error is", test_err))

# Confusion Matrix for Predictions of Training Data
confusionMatrix(as.factor(yhat), test$condition)

# Cross Validation Grid Search
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(1, 400, length.out = 20) %>% floor()
)

# Fit MARS model using 10-fold cross-validation
cv_mars <- train(
  x = train[,-1],
  y = train$condition,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10, verboseIter=TRUE),
  tuneGrid = hyper_grid
)
# Started around 7:30 PM (12/7)
# Ended around 8:00 PM (12/7)


