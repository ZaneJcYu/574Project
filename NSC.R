library(pamr)
library(caret)

set.seed(1)

# Get data
data <- read.csv("full_set.csv")
data_x <- t(data[,-1])
data_y <- data[,1]

# NSC with 10-fold CV
nsc <- pamr.train(data=list(x=data_x, y=data_y))
nsc.cv <- pamr.cv(nsc, data=list(x=data_x, y=data_y), nfold=10)
best_thresh <- nsc.cv$threshold[which.min(nsc.cv$error)]

# 10-fold CV average model error estimate
est_err <- min(nsc.cv$error)
print(paste("The estimated model error is", round(est_err, 5)))

# Training Predictions
yhat <- pamr.predict(nsc, newx=data_x, threshold=best_thresh, type="class")

# Training error
train_err <- mean(yhat != data_y)
print(paste("The training error is", train_err))

# Confusion Matrix
confusionMatrix(as.factor(data_y), yhat)

# Genes that survived thresholding
genes <- pamr.listgenes(nsc,
                        data=list(x=data_x, y=data_y,
                                  genenames=rownames(data_x),
                                  geneid=as.character(1:nrow(data_x))),
                        threshold=best_thresh)
for(id in genes[,1]){
  print(rownames(data_x)[as.numeric(id)])
}
print(paste(length(genes[,1]), "genes survived thresholding."))


