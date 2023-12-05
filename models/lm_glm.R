library(e1071)
library(MASS)
setwd('/574Project/')

dataset <- read.csv('574Project/train_and_test.csv')
dim(dataset)
y <- c(rep(1,517),rep(-1,471),rep(1,57),rep(-1,46))
train_x <- dataset[,-1]
x <- train_x

#### ratio ####
ratio <- c()
for (i in 1:6653){
  ratio <- c(ratio,sum(x[,i]>0)/1091)
}
density <- density(ratio)
plot(density,main='Density Plot',xlab='Value',ylab='Density')
length(which(ratio>0.1))
ratio1 <- ratio[which(ratio>0.4)]


## linear regression and 

x_04 <- x[, which(ratio>0.4)]
lmfit <- lm(y~., data=cbind(y, x_04))
hat_label_lm <- sign(predict(lmfit, x_04))
table(hat_label_lm, y)

lmcv <- function(formula., D, method=c("lm", "logit"), folds=10, seeds=1){
  
  nD = nrow(D)
  if(nD < folds) stop("sample size should be larger than a given fold size")
  
  dim_p = ncol(D)-1
  
  Dpar = floor(nD/folds)
  set.seed(seeds)
  random_par = c(sample(rep(1:folds, each = Dpar), Dpar*folds, replace=F),
                 sample(1:folds, nD - Dpar*folds, replace=F))
  
  cv_error_vec = vector("numeric", length=folds)
  for(i in 1:folds){
    train = D[random_par!=i, ]
    cv = D[random_par==i, -1]
    cv_y = D[random_par==i, 1]
    n_cv = nrow(cv)
    
    ## calculate estimates based on partitions for training
    if(method=="lm") {
      trlm = lm(formula., data=train)
      
      ## calculate CV error based on the estimates and the CV partition
      hat_label = sign(predict(trlm, cv))
    }
    
    if(method=="glm"){
      train[, 1] <- 1*(train[, 1]==1) + 0*(train[, 1]==-1)
      trglm = glm(formula., data=train, family = binomial(link="logit"))
      ## calculate CV error based on the estimates and the CV partition
      hat_label = sign(predict(trglm, cv, type="response")-0.5)
    }
    
    cv_error_vec[i] = 1-sum(diag(table(hat_label, cv_y)))/n_cv 
  }
  
  temp = NULL
  temp$cvm = mean(cv_error_vec)
  temp$folds = folds
  temp$n_sample = nD
  temp$n_partitions = table(random_par)
  
  return(temp)
}

cv10lm <- lmcv(y~., D=cbind(y, x_04), method="lm", folds=10, seeds=1)
cv10lm


## logistic regression
y_ <- 1*(y==1)+0*(y==-1)
logit_fit <-  glm(y_~., data=cbind(y_, x_04), family = binomial(link="logit"))


predict(logit_fit, x_04, type="response")

hat_label_logit <- (logit_fit$fitted.values > .5)*1 + (logit_fit$fitted.values < .5)*-1 
table(hat_label_logit, y)


cv10logit <- lmcv(y~., D=cbind(y, x_04), method="glm", folds=10, seeds=1)
cv10logit
