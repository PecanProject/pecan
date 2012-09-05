\name{GaussProcess}
\alias{GaussProcess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GaussProcess(x, y, isotropic = TRUE, nugget = TRUE, method = "bayes", ngibbs = 5000, burnin = 1000, thin = 1, jump.ic = c(1.1, 0.2), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{isotropic}{ ~~Describe \code{isotropic} here~~ }
  \item{nugget}{ ~~Describe \code{nugget} here~~ }
  \item{method}{ ~~Describe \code{method} here~~ }
  \item{ngibbs}{ ~~Describe \code{ngibbs} here~~ }
  \item{burnin}{ ~~Describe \code{burnin} here~~ }
  \item{thin}{ ~~Describe \code{thin} here~~ }
  \item{jump.ic}{ ~~Describe \code{jump.ic} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function(x,y,isotropic=TRUE,nugget=TRUE,method="bayes",ngibbs=5000,burnin=1000,thin=1,jump.ic=c(1.1,0.2),...){
  ##"nugget" allows additional error in Y rather than fix interpolation to go through points

  ##check for packages
  require("mvtnorm")
  require("MCMCpack")
  haveTime <- require("time")
  prevTime<- NULL; if(haveTime) prevTime <- progressBar();
  if(burnin > ngibbs) burnin <- floor(ngibbs*0.25)
  
  if(!(method \%in\% c("bayes","MLE"))){
    stop(cat(method,"not yet implemented"))
  }
  
  
  ##deal with repeated measures
  x.full <- x
  x.id <- groupid(x)
  x.compact <- NULL
  n.unique <- max(unique(x.id))
  n <- length(x); if(is.matrix(x)) n <- nrow(x)
  for(i in unique(x.id)){
    x.compact <- rbind(x.compact,x.full[which(x.id == i)[1],])
  }
  if(!nugget && n > n.unique) stop("repeated measured detected, but nugget == FALSE")

  ##settings
  ##  isotropic <- (is.matrix(d) || (is.list(d) && length(d) == 1))         # isotropic -> correlation same in all directions
  
  ##calc distance matrix
  d <- NULL
  if(isotropic){
    d <- distance.matrix(x.compact,2)
  } else {
    d <- distance(x.compact,2)
  }
  dim <- 1; if(!isotropic) dim <- length(d)
  
  ##Storage
  samp <- seq(burnin,ngibbs,thin)
  nsamp <- length(samp)
  tauwgibbs <- jump(ngibbs,jump.ic[1])  #spatial var
  psigibbs <- jump(ngibbs,jump.ic[2],dim)   #spatial corr
  mugibbs <- rep(NA,nsamp)    #mean
  Wgibbs <- tauvgibbs <- NULL
  if(nugget){
    Wgibbs <- matrix(NA,nsamp,n.unique) #spatial random effects
    tauvgibbs <- rep(NA,nsamp)  #nugget var
  }
  
  ##IC and Priors
  mu <- mean(y)
  av <- bv <- 0.001 #nugget IG prior
  aw <- bw <- 0.001 #covariance IG prior
  ap <- bp <- 0.01  #spatial IG prior
  mu.V0 <- 10^ceiling(log10(var(y))+2)  #mean prior variance
  tauw <- tauv <- var(y)*0.5
  psi <- rep(1,dim)
  S <- calcSpatialCov(d,psi,tauw) #spatial covariance
  Tinv <- diag(1/tauv,n)
  W <- y-mu    #spatial random effects
  nseq <- 1:n.unique
  W.full <- W[x.id]
  id.count <- as.vector(table(x.id))
  X <- matrix(rep(1,n.unique),n.unique,1)

  ##maximum likelihood
  parm <- c(mu,tauw,psi)
  if(nugget){ parm <- c(mu,tauw,tauv,psi)}
  nmin <- nlm(gp_mle,parm,d=d,nugget=nugget)
  mu <- nmin$estimate[1]
  tauw <- nmin$estimate[2]
  if(nugget){
    tauv <- nmin$estimate[3]
    psi <- nmin$estimate[4:length(parm)]
  } else {
    tauv <- 0
    psi <- nmin$estimate[3:length(parm)]
  }
  if(method == "MLE"){
    return(list(method=method,tauw=tauw,tauv=tauv,mu=mu,psi=psi,nugget=nugget,isotropic=isotropic,d=d,x.id=x.id,x.compact=x.compact,y=y))
  }

  
  ##Gibbs loop
  for(g in 1:ngibbs){
    
    cc <- 1
    ##draw W
    if(nugget){
      Sinv <- solve(S)
      if(n.unique == n){
        M <- solve(Tinv+Sinv)
        m <- Tinv \%*\% (y-mu)
        W <- rmvnorm(1,M \%*\% m, M)
        W.full <- W
      } else {
        ##method 1, draw W's individually
        ##for(i in 1:n.unique){
        ##  sel <- nseq[nseq != i]
        ##  u <- which(x.id == i)
        ##  mubar <- S[i,sel] \%*\% solve(S[sel,sel]) \%*\% W[sel]
        ##  Sbar <- S[i,i]-S[i,sel] \%*\% solve(S[sel,sel]) \%*\% S[sel,i]
        ##  M <- 1/(length(u)/tauv+Sbar)
        ##  m <- sum(y[u]-mu)/tauv
        ##  W[i] <- rnorm(1,M*m,M)
        ##}
        ##method 2, aggregate Y's
        yagg <- tapply(y-mu,x.id,sum)
        M <- solve(id.count*diag(1/tauv,n.unique) + Sinv)
        m <- diag(1/tauv,n.unique) \%*\% yagg
        W <- rmvnorm(1,M\%*\%m,M)
        W.full <- W[x.id]
      }
    } else {
      ##no nugget -- deterministic
      W <- W.full <- y-mu
    }
    
    cc <- 2
    ##draw psi
    psistar <- exp(rnorm(dim,log(psi),p(psigibbs)))
    Sstar <- calcSpatialCov(d,psistar,tauw)
    anum <- try(sum(log(dinvgamma(psistar,ap,bp))) + dmvnorm(as.vector(W),rep(0,n.unique),Sstar,log=TRUE),TRUE)
    aden <- sum(log(dinvgamma(psi,ap,bp))) + dmvnorm(as.vector(W),rep(0,n.unique),S,log=TRUE)
    if(is.numeric(anum) && exp(anum-aden) > runif(1)) {
      psi <- psistar
      S <- Sstar
    }
    psigibbs <- update(psigibbs,psi)
  
    cc <- 3
    ##draw tauw
    taustar <- exp(rnorm(1,log(tauw),p(tauwgibbs)))
    Sstar <- calcSpatialCov(d,psi,taustar)
    anum <- try(log(dinvgamma(taustar,aw,bw)) + dmvnorm(as.vector(W),rep(0,n.unique),Sstar,log=TRUE))
    aden <- log(dinvgamma(tauw,aw,bw)) + dmvnorm(as.vector(W),rep(0,n.unique),S,log=TRUE)
    if(is.numeric(anum) && exp(anum-aden) > runif(1)) {
      tauw <- taustar
      S <- Sstar
    }
    tauwgibbs <- update(tauwgibbs,tauw)
    
    cc <- 4
    ##draw tauv
    if(nugget){
      tauv <- rinvgamma(1,av + n/2, bv + 0.5*sum((y-rep(mu,n)-W.full)^2))
      Tinv <- diag(1/tauv,n)
    }
  
    cc <- 5
    ##draw mu
    if(nugget){
      M <- 1/(n/tauv+1/mu.V0)
      m <- sum(y-W.full)/tauv
      mu <- rnorm(1,M*m,M)
    } else {
      Sinv <- solve(S)
      M <- solve(t(X) \%*\% Sinv \%*\% X + 1/mu.V0)
      m <- t(X) \%*\% Sinv \%*\% y ##[1:10]
      mu <- rnorm(1,M*m,M)
    }
    
    cc <- 6
    ##store
    if(g  \%in\% samp){
      i <- which(samp == g)
      mugibbs[i] <- mu
      ## psigibbs[i] <- psi
      ## tauwgibbs[i] <- tauw
      if(nugget){
        tauvgibbs[i] <- tauv
        Wgibbs[i,] <- W
      }
    }
    if(haveTime) prevTime <- progressBar(g/ngibbs,prevTime)
  }
  if(haveTime) progressBar(1.1,prevTime);
  
  return(list(method=method,tauw = tauwgibbs,psi=psigibbs,mu=mugibbs,tauv=tauvgibbs,W=Wgibbs,nugget=nugget,isotropic=isotropic,d=d,samp=samp,x.id=x.id,x.compact=x.compact,y=y))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
