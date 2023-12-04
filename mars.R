library(earth)
library(magrittr)
library(caret)
library(dplyr)

# Seed for reproducibility
set.seed(1)

# Read in data
data <- read.csv("full_set.csv")
names(data)[1] <- "cell_type"
data$cell_type <- as.factor(data$cell_type)

# MARS Model
mars <- earth(cell_type~., data=data, degree=1, nprune=12)

# Predictions on Training Data
yhat <- predict(mars, type="class")

# Training error
train_err <- mean(yhat != data$cell_type)
print(paste("The training error is", train_err))

# Confusion Matrix for Predictions of Training Data
confusionMatrix(data$cell_type, as.factor(yhat))

# Cross Validation Grid Search
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 50, length.out = 10) %>% floor()
)

#fit MARS model using k-fold cross-validation
cv_mars <- train(
  x = data[,-1],
  y = data$cell_type,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10, verboseIter=TRUE),
  tuneGrid = hyper_grid
)
# Started around 9:30 AM
# Ended around 3:00 PM

# 10-fold CV found that the best model had degree = 1 and nprune = 12 with 
# an estimated avg error of 0.120028

# What genes did MARS choose?
mars %>%
  coef() %>%
  broom::tidy()

