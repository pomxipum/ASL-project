#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "R.matlab", "FNN", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
#library(imager)

# load functions
source("BoF.R")

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


#------------------------------------------------------------------------------#
####                        Supervised learning                             ####
#------------------------------------------------------------------------------#
