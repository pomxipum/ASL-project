## Logistic Regression
rm(list=ls())
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
install.packages("glmnet")
library("glmnet")
library(caret)

load("bows.RData")

x.train <- as.matrix(bow.train[,-1])
y.train <- as.vector(bow.train[,1])
x.val <- as.matrix(bow.val[,-1])
y.val <- as.vector(bow.val[,1])
x.test1 <- as.matrix(bow.test1[,-1])
y.test1 <- as.vector(bow.test1[,1])
x.test2 <- as.matrix(bow.test2[,-1])
y.test2 <- as.vector(bow.test2[,1])



findBest <- function(x.train, y.train, x.val, y.val){
  
  sequence <-seq(0,1,0.1)
  bestAlpha <- 0
  bestLambda <- 0
  oldResult <- 0
  resultRet <- list("alpha"=0, "lambda"=0)
  for(i in sequence){
    glmnet.fit <- glmnet(x=x.train,y=y.train, family = "multinomial", nlambda =1000, alpha = i)  
    glmnet.pred <- predict(glmnet.fit, newx = x.val, type="class")
    for(j in 1:(length(glmnet.fit$lambda))){
      matrix <- confusionMatrix( glmnet.pred[,j], y.val)
      result <- matrix$overall[1]
      if(result > oldResult){
        oldResult <- result
        resultRet <- (list("alpha"=i, "lambda"=glmnet.fit$lambda[j], 
                           "lambdas"= glmnet.fit$lambda, "accuracy" = result,
                           "prediction"=glmnet.pred[,j]))
      }
    }
  }
  return(resultRet)
}

# do not run 
# lrParams <- findBest()
load(file= "lrParams.RData")
lrParams$alpha
x.train <- rbind(x.train, x.val)
y.train <- c(y.train, y.val)
glmnet.fit <- glmnet(x=x.train, y=y.train,
                     family="multinomial", lambda = lrParams$lambda, 
                     alpha=lrParams$alpha)
glmnet.pred1 <- predict(glmnet.fit, newx = x.test1, type="class")
lrTest1 <- confusionMatrix( glmnet.pred1, y.test1)
save(lrTest1, file="lrTest1.RData")

glmnet.pred2 <- predict(glmnet.fit, newx = x.test2, type="class")
lrTest2 <- confusionMatrix(glmnet.pred2, y.test2)
save(lrTest2, file="lrTest2.RData")
