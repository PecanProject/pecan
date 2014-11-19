## Process raw outputs from MCMC
library(data.table)

mcmc.matrix <- function(fname){
  f.raw <- fread(fname, header=FALSE)
  print("File read! Converting to vector...")
  f.vec <- f.raw$V1
  print("Converted! Extracting header...")
  f.head <- f.vec[1:2106]
  print("Complete! Extracting values...")
  f.vals <- as.numeric(unlist(strsplit(f.vec[-2106:0], " ")))
  print("Complete! Converting to matrix...")
  f.mat <- matrix(f.vals, ncol=2106, byrow=TRUE)
  print("Complete!")
  colnames(f.mat) <- f.head
  return(f.mat)
}