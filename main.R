#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "R.matlab", "FNN", "caret",
                      "e1071", "randomForest", "glmnet", "pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(e1071)
library(randomForest)
library(glmnet)

# load functions
source("functions.R")

#------------------------------------------------------------------------------#
####                             Parameters                                 ####
#------------------------------------------------------------------------------#
categories <- c("bicycles", "cars", "motorbikes", "people")
imgsets <- c("train", "val", "test1", "test2")

#------------------------------------------------------------------------------#
####                          Read image sets                               ####
#------------------------------------------------------------------------------#
for (s in imgsets){
  set <- c()
  cat <- c()
  
  for (c in categories){
    set.file <- paste("imgsets/imgset_VOC", c, "_", s, ".txt", sep="")
    listfiles <- readLines(set.file)
    set <- c(set, listfiles)
    cat <- c(cat, rep(c, length(listfiles)))
  }
  
  assign(s, cbind("File"=set, "Cat"=cat))
}

#------------------------------------------------------------------------------#
####                      Build visual dictionary                           ####
#------------------------------------------------------------------------------#
## Randomy sample ~ 100000 descriptors from train and val sets
random.desc <- random.sampling(100000, rbind(train,val)[,1])
# save(random.desc, file = "random.desc.RData")

## Load the randomly sampled descriptors
load("random.desc.RData")

## Run k-means
big.clusters <- kmeans(random.desc, centers = 10, iter.max = 10000, 
                       nstart = 25, algorithm = "MacQueen")
big.clusters <- big.clusters$cluster
for (i in 1:10){
  points <- random.desc[which(big.clusters==i),]
  small.clusters <- kmeans(points, centers = 100, iter.max = 10000, 
                           nstart = 25, algorithm = "MacQueen")
  if (i==1)
    centers <- small.clusters$centers
  else
    centers <- rbind(centers, small.clusters$centers)
}
# save(centers, file = "centers.RData")

#------------------------------------------------------------------------------#
####                   Compute BoW for all images                           ####
#------------------------------------------------------------------------------#
load("centers.RData")

bow.train <- compute.set.bow(train[,1], centers)
bow.train <- cbind('class'= as.factor(train[,2]),as.data.frame(bow.train))

bow.val <- compute.set.bow(val[,1], centers)
bow.val <- cbind('class'= as.factor(val[,2]),as.data.frame(bow.val))

bow.test1 <- compute.set.bow(test1[,1], centers)
bow.test1 <- cbind('class'= as.factor(test1[,2]),as.data.frame(bow.test1))

bow.test2 <- compute.set.bow(test2[,1], centers)
bow.test2 <- cbind('class'= as.factor(test2[,2]),as.data.frame(bow.test2))

# save(bow.train, bow.val, bow.test1, bow.test2, file='bows.RData')

#------------------------------------------------------------------------------#
####                          Tuning parameters                             ####
#------------------------------------------------------------------------------#
load("bows.RData")

## SVM
logspace <- function(d1, d2, n) exp(log(10)*seq(d1, d2, length.out=n)) 
C_range <- logspace(-2, 5, 50)
g_range <- logspace(-4, 1, 30)

svm.lin.tune <- tune(svm, train.x = bow.train[,-1], train.y = bow.train[,1], 
                     validation.x = bow.val[,-1], validation.y = bow.val[,1],
                     kernel="linear", ranges = list(cost=logspace(-2,5, 100)),
                     tunecontrol = tune.control(sampling = "fix"))
svmLinParams <- svm.lin.tune$best.parameters
# save(svmLinParams, file = "svmLinParams.RData")

svm.poly.tune <- tune(svm, train.x = bow.train[,-1], train.y = bow.train[,1], 
                      validation.x = bow.val[,-1], validation.y = bow.val[,1],
                      kernel="polynomial", 
                      ranges = list(cost=C_range, gamma=g_range),
                      tunecontrol = tune.control(sampling = "fix"))
svmPolyParams <- svm.poly.tune$best.parameters
# save(svmPolyParams, file = "svmPolyParams.RData")

svm.rad.tune <- tune(svm, train.x = bow.train[,-1], train.y = bow.train[,1], 
                     validation.x = bow.val[,-1], validation.y = bow.val[,1],
                     kernel="radial",
                     ranges = list(cost=C_range, gamma=g_range),
                     tunecontrol = tune.control(sampling = "fix"))
svmRadParams <- svm.rad.tune$best.parameters
# save(svmRadParams, file = "svmRadParams.RData")


## Logistic Regression
x.train <- as.matrix(bow.train[,-1])
y.train <- as.vector(bow.train[,1])
x.val <- as.matrix(bow.val[,-1])
y.val <- as.vector(bow.val[,1])

alpha_range <- seq(0,1,0.1)
nlambda = 1000
lrParams <- logreg.tune(x.train, y.train, x.val, y.val, 
                        nlambda = nlambda, alphas = alpha_range)
# save(lrParams, file= "lrParams.RData")

# Random forest
mtries <- seq(200,700,20)
rfParams <- rf.tune(bow.train, bow.val, mtries)
# save(rfParams, file = "rfParams.RData")


#------------------------------------------------------------------------------#
####                                  Test                                  ####
#------------------------------------------------------------------------------#
## Load optimal parameters
load("svmLinParams.RData")
load("svmPolyParams.RData")
load("svmRadParams.RData")
load("lrParams.RData")
load("rfParams.RData")

## Train model
svm.lin <- svm(class ~ ., data = rbind(bow.train, bow.val), 
               kernel = "linear", cost= svmLinParams$cost, probability = T)
svm.poly <- svm(class ~ ., data = rbind(bow.train, bow.val), 
                kernel = "polynomial", probability = T,
                cost = svmPolyParams$cost, gamma = svmPolyParams$gamma)
svm.rad <- svm(class ~ ., data = rbind(bow.train, bow.val), kernel = "radial", 
               cost = svmRadParams$cost, gamma = svmRadParams$gamma,
               probability = T)

x.train <- as.matrix(rbind(bow.train[,-1],bow.val[,-1]))
y.train <- c(as.vector(bow.train[,1]), as.vector(bow.val[,1]))
lr <- glmnet(x=x.train, y=y.train, family="multinomial", 
             lambda = lrParams$lambda, alpha=lrParams$alpha)

rf <- randomForest(class~., data = rbind(bow.train, bow.val), ntrees = 100, 
                   mtry = rfParams$mtry)

## Test 1
hat.svm.lin1 <- predict(svm.lin, bow.test1[,-1], probability = T)
roc.multi(attr(hat.svm.lin1, "probabilities"), bow.test1[,1])
confusionMatrix(hat.svm.lin1, bow.test1[,1])

hat.svm.poly1 <- predict(svm.poly, bow.test1[,-1], probability = T)
roc.multi(attr(hat.svm.poly1, "probabilities"), bow.test1[,1])
confusionMatrix(hat.svm.poly1, bow.test1[,1])

hat.svm.rad1 <- predict(svm.rad, bow.test1[,-1], probability = T)
roc.multi(attr(hat.svm.rad1, "probabilities"), bow.test1[,1])
confusionMatrix(hat.svm.rad1, bow.test1[,1])

hat.lr.prob1 <- predict(lr, as.matrix(bow.test1[,-1]), type="response")[,,1]
roc.multi(hat.lr.prob1, bow.test1[,1])
hat.lr1 <- predict(lr, as.matrix(bow.test1[,-1]), type = "class")
confusionMatrix(hat.lr1, bow.test1[,1])

hat.rf.prob1 <- predict(rf, bow.test1[,-1], type="prob")
roc.multi(hat.rf.prob1, bow.test1[,1])
hat.rf1 <- predict(rf, bow.test1[,-1], type = "response")
confusionMatrix(hat.rf1, bow.test1[,1])

## Test 2
hat.svm.lin2 <- predict(svm.lin, bow.test2[,-1], probability = T)
roc.multi(attr(hat.svm.lin2, "probabilities"), bow.test2[,1])
confusionMatrix(hat.svm.lin2, bow.test2[,1])

hat.svm.poly2 <- predict(svm.poly, bow.test2[,-1], probability = T)
roc.multi(attr(hat.svm.poly2, "probabilities"), bow.test2[,1])
confusionMatrix(hat.svm.poly2, bow.test2[,1])

hat.svm.rad2 <- predict(svm.rad, bow.test2[,-1], probability = T)
roc.multi(attr(hat.svm.rad2, "probabilities"), bow.test2[,1])
confusionMatrix(hat.svm.rad2, bow.test2[,1])

hat.lr.prob2 <- predict(lr, as.matrix(bow.test2[,-1]), type="response")[,,1]
roc.multi(hat.lr.prob2, bow.test2[,1])
hat.lr2 <- predict(lr, as.matrix(bow.test2[,-1]), type = "class")
confusionMatrix(hat.lr2, bow.test2[,1])

hat.rf.prob2 <- predict(rf, bow.test2[,-1], type="prob")
roc.multi(hat.rf.prob2, bow.test2[,1])
hat.rf2 <- predict(rf, bow.test2[,-1], type = "response")
confusionMatrix(hat.rf2, bow.test2[,1])
