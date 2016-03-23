#--------------------------------------------------------------------------------------------------#
##' 
##' @title summarize.GP 
##' @export
##'

`summarize.GP` <-
function(gp,pdf_file=NULL,txt_file=NULL){
  require("coda")
  nugget <- gp$nugget
  isotropic <- gp$isotropic
  d <- gp$d
  samp <- gp$samp
  if(is.null(pdf_file)){
    par(ask=TRUE)
  }else{
    pdf(pdf_file)
  }
  if(!is.null(txt_file)){sink(txt_file)}
  
  plot(gp$tauwjump)
  #title("JUMP: TAUW")
  
  plot(gp$psijump)
  #title("JUMP: PSI")

  tauw <- mcmc(gp$tauw[samp,])
  psi <- mcmc(gp$psi[samp,])
  mu <- mcmc(gp$mu)
  if(nugget){
    tauv <- mcmc(gp$tauv)
    print("**** TAUV ****")
    summary(tauv)
    plot(tauv,main="TAUV")
    W <- mcmc(gp$W)
    print("**** W ****")
    summary(W)
    plot(W,main="W")
  }
  print("**** TAUW ****")
  print(summary(tauw))
  print("**** PSI ****")
  print(summary(psi))
  print("**** MU ****")
  print(summary(mu))
  ##par(ask=TRUE)
  plot(tauw)
  title("TAUW")
  plot(psi)
  title("PSI")
  plot(mu)
  title("MU")
  
  ## plot ACF
  par(mfrow=c(1,1))
  if(isotropic){
    xseq <- seq(0,max(d)/2,length=100)
    plot(xseq,mean(tauw)*exp(-mean(psi)*xseq^2),type='l')
  } else {
    ##anisotropic
    rng <- 0; for(i in 1:dim) rng <- max(c(rng,sqrt(max(d[[i]]))))
    xseq <- seq(0,rng/2,length=100)
    acorr <- matrix(NA,100,dim)
    for(k in 1:dim){
      acorr[,k] <- exp(-mean(psi[,k])*xseq^2)
    }
    plot(0,0,type='n',xlim=c(0,rng/2),ylim=c(0,max(acorr)),xlab="Parameter Distance",ylab="Correlation")
    for(k in 1:dim){
      lines(xseq,acorr[,k],col=k)
    }
  }
  par(ask=FALSE)
  if(!is.null(pdf_file)) dev.off()
  if(!is.null(txt_file)) sink()
}  ##end summarize.GP

