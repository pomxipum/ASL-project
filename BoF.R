this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

norm_vec <- function(x) sqrt(sum(x^2))

desc <- read.table("test_desc.txt")
norm_vec(desc[1,])
max(desc)
