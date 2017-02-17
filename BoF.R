#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
library(R.matlab)


#------------------------------------------------------------------------------#
# *Function: norm.vect                                                         #
# *Description: Compute the Euclidien norm of a vector                         #
#                                                                              #
# *Input: - x: numeric vector                                                  #
# *Output: norm of x                                                           #
#------------------------------------------------------------------------------#
norm.vect <- function(x) sqrt(sum(x*x)) 


#------------------------------------------------------------------------------#
# *Function: random.sampling                                                   #
# *Description: randomly sample descriptors from training and validation image #
#               sets                                                           #
#                                                                              #
# *Input: - n: number of descriptors to be sampled (approximately)             #
#         - listfiles: character vector contains paths to image files          #
# *Output: -  res: matrix contains around n descriptors                        #
#------------------------------------------------------------------------------#
random.sampling <- function(n, listfiles){
  nb.img <- length(listfiles)
  nb.desc.per.img <- ceiling(n/nb.img)
  nb.desc.tot <- nb.desc.per.img*nb.img

  res <- matrix(0, ncol=128, nrow=nb.desc.tot)
  
  cD <- 1
  for (img in listfiles){
    mat <- t(readMat(img)$d)
    nb.desc <- nrow(mat)
    ra <- sample(1:nb.desc,nb.desc)
    
    c <- 0
    for (r in ra){
      sift <- mat[r,]
      nn <- norm.vect(sift)
      if (nn > 20){
        res[cD,] <- sift
        cD <- cD + 1
        if (cD %% 10000 == 0)
          print(paste(cD, 'descriptors selected'))
        c <- c + 1
      }
      if (c==nb.desc.per.img)
        break
    }
  }
  print(paste("Number of descriptors sampled per image:", nb.desc.per.img))
  print(paste("Total number of descriptors sampled:", nb.desc.tot))
  return(res)
}


#------------------------------------------------------------------------------#
# *Function:                                                   #
# *Description:                                    #
#                                                                              #
# *Input: -                                                   #
# *Output: -                                                           #
#------------------------------------------------------------------------------#
compute.bow <- funtion(){
  
}

#------------------------------------------------------------------------------#
# *Function:                                                   #
# *Description:                                    #
#                                                                              #
# *Input: -                                                   #
# *Output: -                                                           #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# *Function:                                                   #
# *Description:                                    #
#                                                                              #
# *Input: -                                                   #
# *Output: -                                                           #
#------------------------------------------------------------------------------#

