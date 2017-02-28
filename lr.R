## Logistic Regression
rm(list=ls())
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
install.packages("glmnet")
library("glmnet")
library(caret)

x.train <- as.matrix(bow.train[,-1])
y.train <- as.vector(bow.train[,1])
x.val <- as.matrix(bow.val[,-1])
y.val <- as.vector(bow.val[,1])
glmnet.fit <- glmnet(x=x.train,y=y.train, family = "multinomial", alpha = 0)  
glmnet.pred <- predict(glmnet.fit, newx = x.val, type="class")
matrix <- confusionMatrix( glmnet.pred[,50], y.val)
matrix$overall[1]
dim(glmnet.pred)



findBest <- function(){
  x.train <- as.matrix(bow.train[,-1])
  y.train <- as.vector(bow.train[,1])
  x.val <- as.matrix(bow.val[,-1])
  y.val <- as.vector(bow.val[,1])
  
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
