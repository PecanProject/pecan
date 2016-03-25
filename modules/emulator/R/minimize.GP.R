##' @name minimize.GP
##' @title minimize.GP
##' @export
##'
##' @param gp
##' @param rng 
##' @param splinefuns
##' 
##' @author Michael Dietze
`minimize.GP` <-
function(gp,rng,x0,splinefuns = NULL){
  
  require("coda")
  isotropic <- gp$isotropic
  x.id <- gp$x.id
  ey = 0
  
  if(gp$method == "bayes"){
    samp <- gp$samp
    tauw <- mcmc(gp$tauw[samp,])
    psi <- mcmc(gp$psi[samp,])
    mu <- mcmc(gp$mu)
    tauv <- W <- NULL
  } else {
    ##MLE
    psi <- gp$psi
    mu <- gp$mu
    tauw <- gp$tauw
    tauv <- gp$tauv
  }
  
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
  S <- calcSpatialCov(gp$d,psibar,tauwbar)
#     S12 <- Sprime[1:(npred*dim),(npred*dim+1):(n.unique+npred*dim)]
#     S22 <- Sprime[(npred*dim+1):(n.unique+npred*dim),(npred*dim+1):(n.unique+npred*dim)]
  S22inv <- solve(S)
  if(gp$zeroMean){
    ey <- 0
  } else {
    ey <- max(mu)#mean(y) 
  }
  ybar <- tapply(gp$y,gp$x.id,mean)
  k <- S22inv %*% (ybar-ey)
  
  nlm(gpeval,x0,k=k,mu=ey,tau=tauwbar,psi=psibar,x=gp$x.compact,rng=rng,splinefcns = splinefcns)
}


##' Calculates the probability of a set of parameter values, given by xnew
##'
##' @name gpeval
##' @title gpeval
##' @export
##'
##' @param xnew
##' @param k
##' @param mu
##' @param tau
##' @param psi
##' @param x
##' @param rng range
##' @param splinefcns
##' 
##' @author Michael Dietze 
gpeval <- function(xnew,k,mu,tau,psi,x,rng,splinefcns){
    
  ## second calc value
  S12 <- sapply(1:length(k), function(i){
        tau*exp(-sum(psi*(xnew-x[i,])^2))
      })
  yprime <- mu + sum(S12*k)

  
  if(!is.null(splinefcns)){
    ## add trend surface back on
    y0 = splinefuns[[length(xnew)+1]]
    f <- sapply(1:length(xnew), function(j){
          splinefuns[[j]](xnew[j])
        })
    y.trend =  y0 + sum(f-y0)
    yprime  = yprime + ytrend 
  }
  
  return(yprime)
}


##' @name ddist
##' @title ddist
##' @export
ddist<- function(x, prior){
  eval(parse(text=paste('d', prior$distn, sep='')))(x, prior$parama, prior$paramb)
}


# calculate.prior <- function(samples, priors){
#   traits <- names(samples)
#   joint <- sum(sapply(1:nrow(priors), 
#           function(i) -log(ddist(samples[[i]], priors[i,]))))
#   #note: this is within the negative log domain
#   return(joint)
# }

##' @name calculate.prior
##' @title calculate.prior
##' @export
calculate.prior <- function(samples, priors){
  joint <- sum(sapply(1:length(priors), 
          function(i) eval(priors[[i]], list(x=samples[[i]])) 
          ))
  return(joint)
}

##' @name get.y
##' @title get.y
##' @export
get.y <- function(gp, xnew, priors, ...){
  likelihood <- predict(gp, xnew)
  prior.prob <- calculate.prior(xnew, priors)
  return(likelihood + prior.prob)
}

# is.accepted <- function(ycurr, ynew, format='lin'){
#   z <- exp(ycurr-ynew)
#   acceptance <- z>runif(1)
#   return(acceptance)
# }

##' @name is.accepted
##' @title is.accepted
##' @export
is.accepted <- function(ycurr, ynew, format='lin'){
  a <- exp(ynew - ycurr)
  acceptance <- a>runif(1)
  return(acceptance)
}

##' Function to sample from a GP model
##' that is assumed to be a -lnLikelihood surface
##' with flat priors and bounded region
##'
##' @name mcmc.GP
##' @title mcmc.GP
##' @export
##'
##' @param gp
##' @param x0 
##' @param rng
##' @param format lin = lnlike fcn, log = log(lnlike)
##' @param mix each = jump each dim. independently, joint = jump all at once 
##' @param splinefcns
##' @param jmp0
##' @param ar.target
##' @param priors
##' 
##' @author Michael Dietze
mcmc.GP <- function(gp,x0,nmcmc,rng,format="lin",mix, splinefcns=NULL, 
    jmp0=0.35*(rng[,2]-rng[,1]), ar.target=0.5, priors=NA){
  
  haveTime <- FALSE #require("time")

  ## storage
  ycurr <- get.y(gp, x0, priors)

  xcurr <- x0
  dim <- length(x0)
  jmp <- mvjump(ic=jmp0,rate=ar.target, nc=dim)
  samp <- matrix(NA,nmcmc,dim)
  
  ## loop
  prevTime<- NULL; if(haveTime) prevTime <- progressBar();
  for(g in 1:nmcmc){

    if(mix == "joint"){
      ## propose new
      xnew <- xcurr
      for(i in 1:dim){
        xnew[i] <- rnorm(1,xcurr[[i]],p(jmp)[i])
      }
      #if(bounded(xnew,rng)){
        ynew <- get.y(gp, xnew, priors)
        if(is.accepted(ycurr,ynew)){
          xcurr <- xnew
          ycurr <- ynew
        }
      #}
    } else {  ## mix = each
      for(i in 1:dim){
        ## propose new
        xnew <- xcurr
        xnew[i] <- rnorm(1,xcurr[[i]],p(jmp)[i])
        #if(bounded(xnew,rng)){
          ynew <- get.y(gp, xnew, priors)
          if(is.accepted(ycurr,ynew)){
            xcurr <- xnew
            ycurr <- ynew
          }
        #}
      }
    }
    samp[g,] <- unlist(xcurr)
    #print(p(jmp))
    jmp <- update(jmp,samp)

    if(haveTime) prevTime <- progressBar(g/nmcmc,prevTime)
  }
  if(haveTime) progressBar(1.1,prevTime);
  
  return(list(mcmc=samp,jump=jmp))
##    xnew <- gpeval,x0,k=k,mu=ey,tau=tauwbar,psi=psibar,x=gp$x.compact,rng=rng)
  
###################   IN PROGRESS ##############
}

##' @name bounded
##' @title bounded
##' @export
bounded <- function(xnew,rng)
{
  xnew <- as.vector(as.matrix(xnew))
  down <- xnew > rng[,1]
  up <- xnew < rng[,2]
  return(all(up & down))
}


##' @name plot.mvjump
##' @title plot.mvjump
##' @export
##'
##' @param jmp
##' 
##' @author Michael Dietze

`plot.mvjump` <-
function(jmp){
  par(mfrow=c(1,2))
  plot(attr(jmp,"history")[,1],ylab="Jump Parameter",main="Jump Parameter")
  abline(h=mean(attr(jmp,"history")[,1],na.rm=TRUE))
  text(0.9*length(attr(jmp,"history")[,1]),
       min(attr(jmp,"history")[,1])+0.8*(max(attr(jmp,"history")[,1])-min(attr(jmp,"history")[,1])),
       paste("mean=",mean(attr(jmp,"history")[,1])))
  plot(attr(jmp,"arate"),ylab="Acceptance Rate",main="Acceptance Rate",ylim=c(0,1))
  abline(h=attr(jmp,"target"))
  abline(h=mean(attr(jmp,"arate"),na.rm=TRUE),col=2)
}
