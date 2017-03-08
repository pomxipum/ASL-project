#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
library(R.matlab)
library(glmnet)
library(caret)
library(randomForest)
library(pROC)
library(RColorBrewer)

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
# *Function: logreg.tune                                                       #
# *Description: Tuning the parameters for penalized logistic regression method #
#                                                                              #
# *Input: - x.train: training matrix                                           #
#         - y.train: response vector for traning matrix                        #
#         - x.val: validation matrix                                           #
#         - y.train: reponse vector for validation matrix                      #
#         - nblambda: number of testing values for lambda                      #
#         - alphas: testing values for alpha                                   #
# *Output: - resultRet: list of optimal parameters                             #
#------------------------------------------------------------------------------#
logreg.tune <- function(x.train, y.train, x.val, y.val, nlambda, alphas){
  bestAlpha <- 0
  bestLambda <- 0
  oldResult <- 0
  resultRet <- list("alpha"=0, "lambda"=0)
  for(i in alphas){
    glmnet.fit <- glmnet(x=x.train,y=y.train, family = "multinomial", 
                         nlambda = nblambda, alpha = i)  
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


#------------------------------------------------------------------------------#
# *Function: rf.tune                                                           #
# *Description: Tuning the parameters for random forest method                 #
#                                                                              #
# *Input: - train: training data frame, with the label variable named "class"  #
#         - val: validation data frame, with the label variable named "class"  #
#         - mtries: testing values for mtry of the randomForest function       #
# *Output: - res: list of optimal parameters                                   #
#------------------------------------------------------------------------------#
rf.tune <- function(train, val, mtries){
  best <- 0
  old <- 0
  trueClass <- val$class
  val <- subset(val, select=-c(class))

  for (m in mtries) {
    rf <- randomForest(class~.,data=train, mtry=m, ntress = 100)
    hatrf <- predict(rf, newdata = val, type="class")
    matrix <- confusionMatrix(hatrf, trueClass)
    result <- matrix$overall[1]
    if(result > old){
      old <- result
      best <- m
    }
  }
  
  res <- list("mtry" = best)
  return(res)
}


#------------------------------------------------------------------------------#
# *Function: roc.multi                                                         #
# *Description: Draw one-vs-all ROC curves for multiclasses classification     #
#                                                                              #
# *Input: - prob: probabilities matrix for a test dataset predicted by a model #
#         - realClass: real classes of the test dataset                        #
# *Output: none, draw the ROC curves                                           #
#------------------------------------------------------------------------------#
roc.multi <- function(prob, realClass){
  classes <- colnames(prob)
  color <-brewer.pal(length(classes),"Dark2")
  
  legend.text <- classes
  temp <- realClass
  levels(temp)[1] <- 0
  levels(temp)[-1] <- 1
  ROC <- roc(temp, prob[,1])
  plot(ROC, col = color[1], lwd=2)
  legend.text[1] <- paste(legend.text[1], " (AUC = ", round(ROC$auc,3), 
                          ")", sep="")
  
  for (i in 2:length(classes)){
    temp <- realClass
    levels(temp)[i] <- 0
    levels(temp)[-i] <- 1
    ROC <- roc(temp, prob[,1])
    plot(ROC, add = T, col = color[i], lwd=2)
    legend.text[i] <- paste(legend.text[i], " (AUC = ", round(ROC$auc,3), 
                            ")", sep="")
  }
  
  legend(x=0.5, y = 0.35, legend = legend.text, title = "Class", lty=1, lwd = 2,
         col = color, cex=0.8, bty="n")
}



