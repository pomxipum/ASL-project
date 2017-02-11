path <- "C:/Users/Hoai Thu Nguyen/Dropbox/DM/ASL/Projet/imgsets/"
setwd(path)

library(stringr)

files <- list.files(path)
for (f in files){
  img <- readLines(f)
  img <- unlist(str_replace_all(img, "Annotations", "DescSIFT"))
  writeLines(img, f)
}