#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
library(R.matlab)
library(FNN)

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
# *Function: compute.set.bow                                                   #
# *Description: Compute bow reprentations for images from a trained            # 
#               dictionary                                                     #
#                                                                              #
# *Input: - listfiles: chacracter vector contains paths to matlab files which  #
#           contain the descriptors of images                                  #
#         - centers: representative words in dictionary (produced by k-means)  #
#         - sigma: weighting parameter in Liu et al 2011 (default = 115,       #
#           suitable for descriptors produced by VLFeat)                       #
#         - k: number of nearest centers (default = 10 as in Liu et al 2011)   #
# *Output: - bow: matrix contains the bow representations of each image on     #
#            each row                                                          #
#------------------------------------------------------------------------------#
compute.set.bow <- function(listfiles, centers, sigma=115, k=10){
  bow <- matrix(0, ncol = nrow(centers)+1, nrow = length(listfiles))
  for (i in 1:length(listfiles)){
    bow[i,] <- compute.bow(listfiles[i], centers, sigma, k)
  }
  return(bow)
}


#------------------------------------------------------------------------------#
# *Function: compute.bow                                                       #
# *Description: Compute bow reprentation for one image from a trained          # 
#               dictionary                                                     #
#                                                                              #
# *Input: - file: path to matlab file which contains the descriptors of image  #
#         - centers: representative words in dictionary (produced by k-means)  #
#         - sigma: weighting parameter in Liu et al 2011 (default = 115,       #
#           suitable for descriptors produced by VLFeat)                       #
#         - k: number of nearest centers (default = 10 as in Liu et al 2011)   #
# *Output: - histo: numeric vector as BoW representation of image              #
#------------------------------------------------------------------------------#
compute.bow <- function(file, centers, sigma=115, k=10){
  beta <- 1/(2*sigma*sigma)
  features <- get.features(file)
  X <- features$sifts
  k.nn <- get.knnx(centers, X)
  nn.dist <- k.nn$nn.dist
  nn.index <- k.nn$nn.index
  
  codebook <- matrix(0, ncol=nrow(X), nrow=nrow(centers))
  for (i in 1:nrow(X)){
    s <- sum(exp(-beta*nn.dist[i,]))
    for (j in 1:k){
      codebook[nn.index[i,j],i] <- (exp(-beta*nn.dist[i,j]))/s
    }
  }
  
  histo <- sapply(1:nrow(codebook), function(k) max(codebook[k,]))
  # add null feature
  histo <- c(features$p.null,histo)
  return(histo)
}

#------------------------------------------------------------------------------#
# *Function: get.features                                                      #
# *Description: filter out the non-null features of image                      #
#                                                                              #
# *Input: - file: path to matlab file which contains the features              #
# *Output: - features: matrix of non-null features                             #
#          - p.null: probability of null features                              #
#------------------------------------------------------------------------------#
get.features <- function(file){
  mat <- t(readMat(file)$d)
  r <- rowSums(mat)
  features <- mat[which(r!=0),]
  p.null <- (nrow(mat)-nrow(features))/nrow(mat)
  return(list(sifts = features, p.null = p.null))
}


#------------------------------------------------------------------------------#
# *Function:                                                   #
# *Description:                                    #
#                                                                              #
# *Input: -                                                   #
# *Output: -                                                           #
#------------------------------------------------------------------------------#

