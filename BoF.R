#------ Margot Selosse, Hoai Thu Nguyen, Susana Pacheco, Dongjie Zhang --------#
#------------------- Advance Supervised Learning project ----------------------# 
#--------------------------------- 2016/2017 ----------------------------------#
library(R.matlab)

get.features <- function(imgsets){
  sets <- list.files(imgsets, full.names=T)
  test <- grep('test', sets)
  sets <- sets[-test]
  
  for (setname in sets){
    listfiles <- readLines(setname)
    for (filename in listfiles){
      mat <- t(readMat(filename)$d)
      if (!exists("res"))
        res <- mat
      else
        res <- rbind(res,mat)
    }
  }
  return(res)
}



res <- get.features("imgsets")
save(res, "all-desc.RData")