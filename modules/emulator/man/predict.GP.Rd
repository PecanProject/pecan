\name{predict.GP}
\alias{predict.GP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.GP(gp, xpred, cI = NULL, pI = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gp}{ ~~Describe \code{gp} here~~ }
  \item{xpred}{ ~~Describe \code{xpred} here~~ }
  \item{cI}{ ~~Describe \code{cI} here~~ }
  \item{pI}{ ~~Describe \code{pI} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(gp,xpred,cI = NULL,pI = NULL){
  
  require("coda")
  npred <- length(xpred); if(is.matrix(xpred)) npred <- nrow(xpred)
  nugget <- gp$nugget
  isotropic <- gp$isotropic
  d <- gp$d
  dim <- 1#; if(!isotropic) dim <- length(d)
  x <- gp$x.compact
  x.id <- gp$x.id
  n.unique <- length(gp$x.id)
  #npred <- npred-n.unique
  y <- gp$y
 
  dprime <- NULL
  if(isotropic){
    dprime <- distance.matrix(rbind(xpred,x),2)
  } else {
    dprime <- distance(rbind(xpred,x),2)
  }

  if(gp$method == "bayes"){
    samp <- gp$samp
    tauw <- mcmc(attr(gp$tauw,"chain")[samp,])
    psi <- mcmc(attr(gp$psi,"chain")[samp,])
    mu <- mcmc(gp$mu)
    tauv <- W <- NULL
    if(nugget){
      tauv <- mcmc(gp$tauv)
      W <- mcmc(gp$W)
    }
  } else {
    ##MLE
    psi <- gp$psi
    mu <- gp$mu
    tauw <- gp$tauw
    tauv <- gp$tauv
  }
  
  ## Krige w/o interval
  if((is.null(cI) && is.null(pI))||gp$method=="MLE"){
     psibar <- NULL
     if(isotropic){
       psibar <- median(psi)
     } else {
       if(is.matrix(psi)){
         psibar <- apply(psi,2,median)
       } else {
         psibar <- psi
       }
     }
     tauwbar <- median(tauw)
     Sprime <- calcSpatialCov(dprime,psibar,tauwbar)
     S12 <- Sprime[1:(npred*dim),(npred*dim+1):(n.unique+npred*dim)]
     S22 <- Sprime[(npred*dim+1):(n.unique+npred*dim),(npred*dim+1):(n.unique+npred*dim)]
     S22inv <- solve(S22)
     ey <- eyprime <- median(mu)#mean(y)
     ybar <- tapply(y,x.id,mean)
     yprime <- eyprime + S12 \%*\% S22inv \%*\% (ybar-ey)
     return(yprime)
   }

###Credible and prediction intervals
  nsamp <- length(samp)
#  cInt <- pInt <- matrix(NA,nsamp,npred*dim)
  cInt <- pInt <- matrix(NA,nsamp,npred)
  haveTime <- require("time")
  prevTime<- NULL; if(haveTime) prevTime <- progressBar();  
  for(g in samp){
    j <- i <- which(g == samp)
    if(dim == 1){
      psi1 <- psi[i]
    }else{psi1 <- psi[i,]}
    tauw1 <- tauw[i]
    if(nugget){
      tauv1 <- tauv[i]
    }
    Sprime <- calcSpatialCov(dprime,psi1,tauw1)
    ##S22 <- Sprime[(npred*dim+1):(n.unique+npred*dim),(npred*dim+1):(n.unique+npred*dim)]
    S22 <- Sprime[(npred+1):(n.unique+npred),(npred+1):(n.unique+npred)]
    S22inv <- try(solve(S22))
    if(!is.numeric(S22inv)) next;
##    S12 <- Sprime[1:(npred*dim),(dim*npred+1):(n.unique+npred*dim)]
##    S11 <- Sprime[1:(npred*dim),1:(npred*dim)]
    S12 <- Sprime[1:(npred),(npred+1):(n.unique+npred)]
    S11 <- Sprime[1:(npred),1:(npred)]
    Sbar <- S11 - S12 \%*\% S22inv \%*\% t(S12)
    if(nugget){
      Wprime <- rmvnorm(1,S12 \%*\% S22inv \%*\% (W[i,]),Sbar)
      cInt[j,] <- mu[i] + Wprime
      pInt[j,] <- rnorm(npred*dim,cInt[j,],sqrt(tauv1))
    } else {
      cInt[j,] <- mu[i] + S12 \%*\% S22inv \%*\% (y-mu[i])
      pInt[j,] <- rmvnorm(1,cInt[j,],Sbar)
      
    }
    if(haveTime) prevTime <- progressBar(i/length(samp),prevTime)
  }
  cIntQuant <- pIntQuant <- NULL
  if(!is.null(cI)) cIntQuant <- apply(cInt,2,quantile,cI,na.rm=T)
  if(!is.null(pI)) pIntQuant <- apply(pInt,2,quantile,pI,na.rm=T)
  return(list(ci = cIntQuant,pi=pIntQuant))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
