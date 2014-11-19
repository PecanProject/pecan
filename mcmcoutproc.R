## Process raw outputs from MCMC
library(data.table)

diag <- function(fname, beg=1000){
  dat <- fread(fname, header=TRUE, data.table=FALSE)
  par(mfrow=c(5,2))
  for(cn in colnames(dat)[1:5]){
    plot(dat[-beg:0,cn], type='l')
    hist(dat[-beg:0,cn])
    print(sprintf("%s AR %.2f", cn, accept.rate(dat[,cn])))
  }  
}

accept.rate <- function(x) length(unique(x))/length(x)