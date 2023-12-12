library(pamr)
library(caret)

set.seed(1)

# Get data
data <- read.csv("CD8Tcell_screened1.csv")
train <- data[which(data$train==1),]
test <- data[which(data$train==0),]
train_x <- t(train[,-c(1,2,413)])
train_y <- train[,2]
test_x <- t(test[,-c(1,2,413)])
test_y <- test[,2]

# NSC with 10-fold CV0
nsc <- pamr.train(data=list(x=train_x, y=train_y))
nsc.cv <- pamr.cv(nsc, data=list(x=train_x, y=train_y), nfold=10)
best_thresh <- nsc.cv$threshold[which.min(nsc.cv$error)]

# Test Predictions
yhat <- pamr.predict(nsc, newx=test_x, threshold=best_thresh, type="class")

# Test error
test_err <- mean(yhat != test_y)
print(paste("The test error is", test_err))

# Confusion Matrix
confusionMatrix(yhat, as.factor(test_y))
# Genes that survived thresholding
genes <- pamr.listgenes(nsc,
                        data=list(x=train_x, y=train_y,
                                  genenames=rownames(train_x),
                                  geneid=as.character(1:nrow(train_x))),
                        threshold=best_thresh)
gene.names <- rep(0, length(genes[,1]))
for(idx in 1:length(genes[,1])){
  print(rownames(train_x)[as.numeric(genes[,1][idx])])
  gene.names[idx] <- rownames(train_x)[as.numeric(genes[,1][idx])]
}
print(paste(length(genes[,1]), "genes survived thresholding."))


