library(e1071)
library(MASS)
setwd('~/Desktop/574Project/')

dataset <- read.csv('dataset.csv')
test <- read.csv('test.csv')
dim(dataset)
dim(test)
y <- c(rep(1,517),rep(-1,471),rep(1,57),rep(-1,46))
train_x <- dataset[,-1]
test_x <- test[,-1]
x <- rbind(train_x,test_x)

#### ratio ####
ratio <- c()
for (i in 1:6653){
  ratio <- c(ratio,sum(x[,i]>0)/1091)
}
density <- density(ratio)
plot(density,main='Density Plot',xlab='Value',ylab='Density')
length(which(ratio>0.1))
ratio1 <- ratio[which(ratio>0.1)]

#### SVM ####
#### kernel SVM ####
lams <- 10^seq(-3,5,by=0.5)
sigs <- c(0.001,0.005,0.1)
ss <- length(sigs)
rr <- length(lams)
ksvmtesterr <- rep(1,ss*rr)
ksvmtrainerr <- rep(1,ss*rr)
id = 0 
for (i in 1:ss){
  for(j in 1:rr){
    id = id + 1
    svmfit <- svm(x,y,scale=F,,type='C',kernel='radial',gamma=sigs[i],cost=1/lams[j],cross=5)
    trainpredict <- predict(svmfit,x,decision.values=T)
    yhat_train <- attr(trainpredict,"decision.values")
    testpredict <- predict(svmfit,test_x,decision.values=T)
    yhat_test <- attr(testpredict,"decision.values")
    ksvmtrainerr[id] <- mean(sign(yhat_train)!=y)
    ksvmtesterr[id] <- mean(sign(yhat_test)!=test_y)
  }
}
print(c("gamma","lambda","train_err","test_err"))
print(cbind(c(rep(sigs[1],rr),rep(sigs[2],rr),rep(sigs[3],rr)), c(rep(lams,ss)), ksvmtrainerr,ksvmtesterr))
#### LSVM ####
f <- function(x,y,test_x,test_y){
  lams <- 10^seq(-3,5,by=0.5)
  sigs <- c(0.001,0.005,0.1)
  ss <- length(sigs)
  rr <- length(lams)
  ksvmtesterr <- rep(1,ss*rr)
  ksvmtrainerr <- rep(1,ss*rr)
  id = 0 
  for (i in 1:ss){
    for(j in 1:rr){
      id = id + 1
      svmfit <- svm(x,y,scale=F,,type='C',kernel='radial',gamma=sigs[i],cost=1/lams[j],cross=5)
      trainpredict <- predict(svmfit,x,decision.values=T)
      yhat_train <- attr(trainpredict,"decision.values")
      testpredict <- predict(svmfit,test_x,decision.values=T)
      yhat_test <- attr(testpredict,"decision.values")
      ksvmtrainerr[id] <- mean(sign(yhat_train)!=y)
      ksvmtesterr[id] <- mean(sign(yhat_test)!=test_y)
    }
  }
  
  return (list(svmfit, ksvmtrainerr, ksvmtesterr))
}

g <- function(x,y,ratio=ratio,r=0.9){
  index <- which(ratio>r)
  x <- x[index]
  lams <- 10^seq(-3,5,by=0.5)
  lsvmtrainerr<- rep(1,length(lams))
  for (i in 1:length(lams))
  {
    svmfit <- svm(x, y, scale=F, type='C', kernel='linear',cost=1/lams[i],cross=10) 
    trainpredict <- predict(svmfit,x,decision.values=T)
    yhat_train <- attr(trainpredict,"decision.values")
    lsvmtrainerr[i] <- mean(sign(yhat_train)!=y)}
  
  return (lsvmtrainerr)
}
set.seed(1)
## all genes
train_err <- g(x,y,ratio=ratio,r=0.0)
print(c("lambda","train_err"))
print(cbind(lams, train_err))
## ratio 0.1
train_err1 <- g(x,y,ratio=ratio,r=0.1)
print(c("lambda","train_err"))
print(cbind(lams, train_err1))

## ratio 0.2
length(which(ratio>0.2))
train_err2 <- g(x,y,ratio=ratio,r=0.2)
print(c("lambda","train_err"))
print(cbind(lams, train_err2))

## ratio 0.3
length(which(ratio>0.3))
train_err3 <- g(x,y,ratio=ratio,r=0.3)
print(c("lambda","train_err"))
print(cbind(lams, train_err3))

## ratio 0.4

####
index <- which(ratio>0.4)
train_x <- x[index]
fit1 <- svm(train_x, y, scale=F, type='C', kernel='linear',cost=1/(1e1),cross=10) 
trainpredict <- predict(fit1,train_x,decision.values=T)
trainpredict <- as.numeric(trainpredict)
indep1 <- which(trainpredict==2)
inden1 <- which(trainpredict==1)
trainpredict[indep1] <-rep(1,length(indep1))
trainpredict[inden1] <- rep(-1,length(inden1))
total <- sum(trainpredict==y)
tp <- length(intersect(indep1,which(y==1)))
tn <- length(intersect(inden1,which(y==-1)))
fp <- length(indep1)-tp
####
length(which(ratio>0.4))
train_err4 <- g(x,y,ratio=ratio,r=0.4)
print(c("lambda","train_err"))
print(cbind(lams, train_err4))


## ratio 0.5
length(which(ratio>0.5))
train_err5 <- g(x,y,ratio=ratio,r=0.5)
print(c("lambda","train_err"))
print(cbind(lams, train_err5))

## ratio 0.6
length(which(ratio>0.6))
train_err6 <- g(x,y,ratio=ratio,r=0.6)
print(c("lambda","train_err"))
print(cbind(lams, train_err6))

## ratio 0.7
length(which(ratio>0.7))
train_err7 <- g(x,y,ratio=ratio,r=0.7)
print(c("lambda","train_err"))
print(cbind(lams, train_err7))

## ratio 0.9
length(which(ratio>0.9))
train_err9 <- g(x,y,ratio=ratio,r=0.9)
print(c("lambda","train_err"))
print(cbind(lams, train_err9))


#### Visualization ####
colors <- hcl.colors(9,alpha=0.6)
plot(train_err~lams,type='l',col=colors[1],
     xlim=c(1e-3,4e4),
     xlab='Lambdas',ylab='Training error',main='Validation errors')
lines(train_err1~lams,col=colors[2])
lines(train_err2~lams,col=colors[3])
lines(train_err3~lams,col=colors[4])
lines(train_err4~lams,col=colors[5])
lines(train_err5~lams,col=colors[6])
lines(train_err6~lams,col=colors[7])
lines(train_err7~lams,col=colors[8])
lines(train_err9~lams,col=colors[9])
ratios <- seq(0,0.9,by=0.1)[-9]
legend('topright',legend=ratios,fill=colors,cex=0.6)

#### PCA ####
set.seed(2023)
colors <- c(rep('red',517),rep('blue',471),rep('red',57),rep('blue',46))
par(mfrow=c(2,4))
## 6653 genes
sx <- scale(x)
pX <- prcomp(sx)
X <- as.matrix(x)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 1184
nsx <- sx[,which(ratio>0.1)]
X <- as.matrix(x[,which(ratio>0.1)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 368
nsx <- sx[,which(ratio>0.2)]
X <- as.matrix(x[,which(ratio>0.2)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
##
nsx <- sx[,which(ratio>0.3)]
X <- as.matrix(x[,which(ratio>0.3)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 164
nsx <- sx[,which(ratio>0.4)]
X <- as.matrix(x[,which(ratio>0.4)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 85
nsx <- sx[,which(ratio>0.5)]
X <- as.matrix(x[,which(ratio>0.5)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 60
nsx <- sx[,which(ratio>0.6)]
X <- as.matrix(x[,which(ratio>0.6)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 45
nsx <- sx[,which(ratio>0.7)]
X <- as.matrix(x[,which(ratio>0.7)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)
## 20
nsx <- sx[,which(ratio>0.9)]
X <- as.matrix(x[,which(ratio>0.9)])
pX <- prcomp(nsx)
pc2 <- X %*% pX$rotation[,1:2]
plot(pc2,col=colors)



######## Expression ########

expression_length <- rep(1,1091)
for (i in 1:1091){
  l <- length(which(x[i,1:6653]>0))
  expression_length[i] <- l
}
expression_length
d <- density(expression_length)
plot(d,xlab='Gene numbers',ylab='Density',main='Density of gene expression')


