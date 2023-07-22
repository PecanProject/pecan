##' @title assess.params
##' @name  assess.params
##' @author Michael Dietze and Ann Raiho \email{dietze@@bu.edu}
##' 
##' @param dat           MCMC output
##' @param Xt            ensemble output matrix
##' @param wts           ensemble weights
##' @param mu_f_TRUE     muf before tobit2space
##' @param P_f_TRUE      Pf before tobit2space
##' 
##' @description Assessing parameter estimations after mapping model output to tobit space
##' 
##' @return make plots
##' @export
##' 


assessParams <- function(dat, Xt, wts = NULL, mu_f_TRUE = NULL, P_f_TRUE = NULL){  
  #mu_f_TRUE and P_f_TRUE used for simulation

  
  #* page 6 looks more like I expected, but I’m not sure how we’re getting negative variances
  
  #* In general, the first 3 pages of pairs plots doesn’t seem to be producing anything too absurd — there’s no estimates going off to large negative values and nothing TOO far from the sample mean. That said, I do find some of the estimates to be surprising (e.g. why is the posterior for mu12 at t=2 lower than the sample mean when the ensemble shouldn’t include any zeros)
  
  imuf   <- grep("muf", colnames(dat))
  muf <- colMeans(dat[, imuf])
  iPf   <- grep("pf", colnames(dat))
  Pf <- solve(matrix(colMeans(dat[, iPf]),ncol(Xt),ncol(Xt)))
  #--- This is where the localization needs to happen - After imputing Pf
  
  if(is.null(wts)){
    mufT <- apply(Xt,2,mean)
    PfT <- stats::cov(Xt)
  }else{
    mufT <- apply(Xt,2,stats::weighted.mean,wts)
    PfT <- stats::cov.wt(Xt,wts)$cov
  }
  
  eigen_save <- matrix(NA,nrow=nrow(dat),ncol=ncol(Xt))
  for(rr in 1:nrow(dat)) {
    eigen_save[rr,] <- eigen(solve(matrix(dat[rr, iPf],ncol(Xt),ncol(Xt))))$values
  }
  
  
  graphics::par(mfrow=c(2,3))
  apply(eigen_save,2,graphics::plot,typ='l',main='Eigen Value')
  for(i in seq(1,length(iPf),7)){
    graphics::plot(dat[,iPf[i]],typ='l',main='Variance of Pf')
  }
  for(i in 1:length(muf)){
    graphics::plot(dat[,imuf[i]],typ='l',main=paste('muf',i))
    graphics::abline(h=mufT[i],col='red')
  }
  
  Xt_use <- Xt
  rownames(Xt_use)<-colnames(Xt_use) <- NULL
  
  corrplot::corrplot(stats::cov2cor((PfT)),main='correlation T')
  corrplot::corrplot(stats::cov2cor(stats::cov(Xt_use)),main='correlation estimate')
  
  mufCI <- apply(dat[,imuf],2,stats::quantile,c(0.025,0.975))
  mufTCI <- apply(Xt,2,stats::quantile,c(0.025,0.975))
  
  graphics::par(mfrow=c(1,1))
  graphics::plot(mufT,muf,pch=19,ylim=range(mufCI),xlim=range(mufTCI))
  graphics::abline(a=0,b=1,lty=2)
  for(i in 1:length(muf)){
    graphics::lines(mufTCI[,i],rep(as.vector(muf)[i],2),col=i,lwd=2)
    graphics::lines(rep(as.vector(mufT)[i],2),mufCI[,i],col=i,lwd=2)
  }
  
  #muf mufT scatter plot
  graphics::par(mfrow=c(2,2))
  for(i in 1:(length(imuf)-1)){
    graphics::plot(dat[,i],dat[,i+1],xlab=paste('mu', i),ylab=paste('mu', i+1))
    #points(mu_f_TRUE[i],mu_f_TRUE[i+1],cex=3,col=2,pch=18)
    graphics::points(muf[i],muf[i+1],cex=3,col=3,pch=19)
    graphics::points(mufT[i],mufT[i+1],cex=3,col=4,pch=20)
  }
  graphics::plot.new()
  graphics::legend("topleft",legend=c("post","sampT"),col=3:4,pch = 19:20)
  #legend("topleft",legend=c("TRUE","post","sampT"),col=2:4,pch = 18:20)
  
  graphics::boxplot(Xt,xlab='State Variables',ylab='X')
  graphics::points(muf,col='red',pch=19)
  graphics::legend("topleft",legend=c("muf"),col='red',pch = 19)
  
  #cor(dat[,1:6])
  
  #iPf   <- grep("pf", colnames(dat))
  #Pf <- matrix(colMeans(dat[, iPf]),ncol(Xt),ncol(Xt))
  
  PfCI <- apply(dat[,iPf],2,stats::quantile,c(0.025,0.975))

  diag.stopper <- diag(length(muf))
  
  graphics::par(mfrow=c(1,1))
  graphics::plot(PfT,Pf,ylim=range(PfCI),pch=19,xlab='Pf Ensemble (True)',ylab='Pf Estimated (tobit2space)')
  graphics::abline(0,1,lty=2)
  for(i in 1:length(Pf)){
    graphics::lines(rep(as.vector(PfT)[i],2),PfCI[,i],col=i,lwd=2)
    if(diag.stopper[i]==1){
      graphics::points(PfT[i],Pf[i],cex=2,pch = 7)
    }
  }
  graphics::legend('topleft','variance',pch = 7,cex=2)
  
  diag.stopper2 <- diag.stopper+1
  diag(diag.stopper2) <- 0
  
  graphics::plot(stats::cov2cor(PfT)[which(diag.stopper2==1)],
       stats::cov2cor(Pf)[which(diag.stopper2==1)],pch=19,
       ylab = 'Pf', xlab = 'Pft', main = 'Correlations')
  graphics::abline(a=0,b=1,lty=2)
  
  corrCI <- apply(dat[,iPf[which(diag.stopper2!=0)]],2,stats::quantile,c(0.025,0.975))
  
  graphics::par(mfrow=c(1,1))
  graphics::plot(PfT[which(diag.stopper2!=0)],Pf[which(diag.stopper2!=0)],
       ylim=range(corrCI),pch=19,xlab='Pf Ensemble (True)',
       ylab='Pf Estimated (tobit2space)',
       main='Non-Diagonal Covariance')
  graphics::abline(a=0,b=1,lty=2)
  for(i in 1:length(Pf)){
    if(diag.stopper2[i]==1){
      graphics::lines(rep(as.vector(PfT)[i],2),PfCI[,i],col=i,lwd=2)
    }
  }
  
  graphics::par(mfrow=c(1,1))
  graphics::plot(diag(PfT)-diag(Pf),xlab='State Variable',pch=19,
       cex=2,main='Which variance changed the most?')
  
  
  #var.change <- data.frame(mufT = signif(colMeans(Xt),digits=2),muf=signif(muf,digits=2),abs.change.var = abs(diag(PfT)-diag(Pf)))
  #var.change[order(var.change$abs.change.var),]
  
  # sort(diag(Pf)-diag(PfT),decreasing = T)
  # 
  # par(mfrow=c(3,3))
  # for(i in 1:length(Pf)) {
  #   if(diag.stopper[i]==1){
  #   plot(dat[,i+14],ylim=c(0,10)); abline(h=as.vector(PfT)[i],col='red',lwd=2)
  #   }
  # }
  #scatterplots
  #var 
  #corr #pull diags out ==1 
  #check mu v var to make sure variance is only changing near 0# shifts in Xt v X on same plot
  
  #PfT <- cov(Xt)
  #points(P_f_TRUE,PfT,col=1:14,pch="-",cex=2)
}
