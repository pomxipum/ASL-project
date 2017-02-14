#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
#### Setting environnement ####
# install missing packages
list.of.packages <- c("rstudioapi", "RColorBrewer", "imager")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos = "http://cran.rstudio.com/")

# set working directory
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
rm(list=ls())

# load libraries
library(imager)

#------------------------------------------------------------------------------#
####              Example: compute SIFTs for some images                    ####
#------------------------------------------------------------------------------#
source("computeSIFTsImage.R")
imgs <- c("VOC2005_1/PNGImages/ETHZ_motorbike-testset/motorbikes020-rt.png",
          "VOC2005_1/PNGImages/TUGraz_cars/carsgraz_162.png",
          "VOC2005_1/PNGImages/TUGraz_person/person_087.png",
          "VOC2005_1/PNGImages/TUGraz_bike/bike_100.png")
