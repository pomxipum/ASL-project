#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "R.matlab")
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


#------------------------------------------------------------------------------#
####                       Bag of Words model                               ####
#------------------------------------------------------------------------------#
## Load all descriptors of all images into one matrix
# ATTETION: This will take several hours and at least 12GB of RAM
#           it's advised not to rerun
# mat.desc <- get.features("imgsets")
# save(mat.desc, "all-desc.RData")

## Instead, load the complete matrix
load("all-desc.R")

## Run k-means
clusters <- kmeans(mat.desc, centers = 1000, iter.max = 10000, nstart = 50)

