##' @title assess.params
##' @name  assess.params
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param dat           MCMC output
##' @param Xt            ensemble output matrix
##' @param mu_f_TRUE     muf before tobit2space
##' @param P_f_TRUE      Pf before tobit2space
##' 
##' @description Assessing parameter estimations after mapping model output to tobit space
##' 
##' @return make plots
##' @export
##' 

assessParams <- function(dat, Xt, mu_f_TRUE, P_f_TRUE){  
  
  imuf   <- grep("muf", colnames(dat))
  muf <- colMeans(dat[, imuf])
  mufT <- apply(Xt,2,mean)
  
  par(mfrow=c(2,2))
  for(i in 1:(length(imuf)-1)){
    plot(dat[,i],dat[,i+1])
    points(mu_f_TRUE[i],mu_f_TRUE[i+1],cex=3,col=2,pch=18)
    points(muf[i],muf[i+1],cex=3,col=3,pch=19)
    points(mufT[i],mufT[i+1],cex=3,col=4,pch=20)
  }
  plot.new()
  legend("topleft",legend=c("TRUE","post","sampT"),col=2:4,pch = 18:20)
  
  #cor(dat[,1:6])
  
  iPf   <- grep("pf", colnames(dat))
  Pf <- matrix(colMeans(dat[, iPf]),ncol(X),ncol(X))
  
  PfCI <- apply(dat[,iPf],2,quantile,c(0.025,0.975))
  par(mfrow=c(1,1))
  plot(P_f_TRUE,Pf,ylim=range(PfCI))
  abline(0,1,lty=2)
  for(i in 1:length(Pf)){
    lines(rep(as.vector(P_f_TRUE)[i],2),PfCI[,i],col=i,lwd=2)
  }
  #PfT <- cov(Xt)
  #points(P_f_TRUE,PfT,col=1:14,pch="-",cex=2)
}