#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "R.matlab", "FNN", "caret",
                      "doParallel", "kernlab", "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(caret)
library(doParallel)
library(e1071)

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

## Load the randomly sampled descroptors
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

save(bow.train, bow.val, bow.test1, bow.test2, file='bows.RData')

#------------------------------------------------------------------------------#
####                        Supervised learning                             ####
#------------------------------------------------------------------------------#
load("bows.RData")

## SVM
svm.lin.tune <- tune(svm, train.x = bow.train[,-1], train.y = bow.train[,1], 
     validation.x = bow.val[,-1], validation.y = bow.val[,1],
     kernel="radial", nrepeat = 10,
     ranges = list(cost=1:15, epsilon=seq(0.1,0.5,0.1)),
     tunecontrol = tune.control(sampling = "fix"))

registerDoParallel(detectCores()-1)
svm.lin <- train(class ~ ., data = rbind(bow.train, bow.val), 
                 method = "svmLinear", epsilon=0.1, cost=1)
hatsvm.lin <- predict(svm.lin, newdata = bow.test2)
confusionMatrix(hatsvm.lin,bow.test2$class)
save(svm.lin, file='svm.lin')

svm.rad <- train(class ~ ., data = rbind(bow.train, bow.val), 
                 method = "svmRadial", epsilon=0.01, cost=15)
hatsvm.rad <- predict(svm.rad, newdata = bow.test1)
confusionMatrix(hatsvm.rad,bow.test1$class)
save(svm.lin, file='svm.lin')


