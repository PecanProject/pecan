## Process raw outputs from MCMC
library(data.table)
library(coda)

load <- function(fname, beg=1000, thin=1){
  rawd <- read.csv(fname, header=TRUE)[-beg:0,]
  rawd <- rawd[seq(1, length(rawd[,1]), by=thin), ]
  dat <- mcmc(rawd)
  return(dat)
}

