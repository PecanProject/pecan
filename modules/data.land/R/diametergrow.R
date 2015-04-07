## Tree dbh & increment model
## Original code from Jim Clark
## Clark et al. 2007 Ecol Appl.
## modified by Mike Dietze

diametergrow <- function(diameters,increment,survival = NULL){

#### data structures:
##
## diameters <- list of tree X year DBH census data matrices by site
## increment <- list of tree X year increment core growth data matrices by site
##
## both matrices have lots of NAs -- includes ALL years and trees, not just measured
##
##

plotend <- function(fname){dev.off()}
plotstart <- function(fname){pdf(fname)}

#####################################################################################
tnorm <- function(n,lo,hi,mu,sig){   #normal truncated lo and hi
  
  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo,length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi,length(mu))
  
  z <- runif(n,pnorm(lo,mu,sig),pnorm(hi,mu,sig))
  z <- qnorm(z,mu,sig)
  z[z == Inf]  <- lo[z == Inf]
  z[z == -Inf] <- hi[z == -Inf]
  z
}
###########################################################

diamint <- function(){     # initialize diameters
  
  diamt <- matrix(NA,n,nt)  #true values, initialized here
  d0    <- matrix(0,n,2)   #range of values in yr 1
  first <- rep(0,n)        #first observation yr
  
  bginc <- mean(sapply(increment,mean,na.rm=TRUE),na.rm=TRUE)
  
  for(i in 1:n){
    
    wf <- min(c(1:nt)[is.finite(surv[i,]) & surv[i,] > 0],na.rm=T)
    wl <- max(c(1:nt)[is.finite(surv[i,]) & surv[i,] > 0],na.rm=T)
    first[i] <- wf
    
    wi <- which(is.finite(dcens[i,]),arr.ind=T)
    xi <- time[wi] - wf + 1  # recenter to first year
    yi <- dcens[i,wi]
    intercept <- mean(yi) - mean(xi)*( cov(xi,yi)/var(xi) )
    
    ## modification: if only one census, assume mean increment
    if(length(xi)==1) intercept <- yi - bginc*xi
    
    if(!is.finite(intercept))intercept <- min(yi,na.rm=T)
    if(intercept < 1)intercept <- 0.001 #max(.1,(min(yi) - 5) )
    slope <- (mean(xi*yi) - intercept*mean(xi) )/mean(xi^2)
    if(slope < .001){
      slope     <- .001
      intercept <- mean(yi) - slope*mean(xi)
    }
    
    dfit <- intercept + slope*(time - wf + 1)
    diamt[i,wf:wl] <- dfit[wf:wl]
    d0[i,] <- c((diamt[i,wf] - 2),(diamt[i,wf] + 2))
  }
  
  d0[d0 < .1] <- .1
  
  list(d0 = d0, diamt = diamt, firstyri = first)
}
#############################################

f.update <- function(){  # sample fixed effects
  
  alpha   <- numeric(0)
  allvars <- sig + sigd + sigp
  nn <- max(nyr) - 1
  # ky <- c(c(1:max(nn))[ntt == 0],nn)   #this sets last year effect to zero
  ky <- c(1:nn)[ntt == 0]
  
  betaMat <- matrix(0,ny,(nt-1))
  muVec   <- rep(0,ny)
  
  if(COVARIATES){
    
    v     <- crossprod(X,(dgrow[aincr] - teffect[aincr]))/allvars + prior.IVm %*% prior.mu
    V     <- solve(crossprod(X)/allvars + prior.IVm)
    alpha <- matrix(rmvnorm(1,V%*%v,V),(ncovars+1),1)
    
    mumat[aincr] <- X %*% alpha
    
    v     <- apply((dgrow - mumat),2,sum,na.rm=T)/allvars
    V     <- 1/(ntt/allvars + 1/prior.Vmu)
    beta  <- rnorm(length(v),(V*v),sqrt(V))
    mb     <- mean(beta[ntt > 0])                 #extract mean
    beta.t <- beta - mb                  
    beta.t[ntt == 0] <- 0
    mu    <- mumat
  }
  
  if(!COVARIATES){
    
    for(m in 1:ny){
      im <- which(nindex == m)
      
      if(length(im) == 1){
        nmm <- dgrow[im,]*0 + 1
        nmm[!is.finite(nmm)] <- 0
        v  <- dgrow[im,]/allvars + prior.mu/prior.Vmu
        v[!is.finite(v)] <- 0
        V  <- 1/(nmm/allvars + 1/prior.Vmu)
      }
      if(length(im) > 1){
        nmm <- t(apply((dgrow[im,]*0 + 1),2,sum,na.rm=T))
        v  <- t(apply((dgrow[im,]),2,sum,na.rm=T))/allvars + prior.mu/prior.Vmu
        V  <- 1/(nmm/allvars + 1/prior.Vmu)
      }
      if(length(im) >= 1){
        bt <- tnorm(length(v),0,4,(V*v),sqrt(V))
        bt[nmm == 0] <- 0
        mu     <- mean(bt) 
        beta.t <- bt - mu   
        beta.t[nmm == 0] <- 0
        if(length(ky) > 0){
          mu     <- mean(beta.t[-ky])                  #extract mean
          beta.t[-ky] <- beta.t[-ky] - mu           
        }       
      }
      beta.t[ky] <- 0
      betaMat[m,] <- beta.t
      muVec[m]    <- mu
    }
    
  }
  
  list(mu = mu, betaMat = betaMat, alpha = alpha, muVec = muVec)
  
}

in.update <- function(){   #sample individual effects
  
  v <- t(apply((dgrow - teffect - peffect - mumat),1,sum,na.rm=T))/sig
  V <- 1/(nti/sig + 1/sigd)
  ind <- tnorm(n,-1,1,V*v,sqrt(V))
  ind[!is.finite(ind)] <- 0
  ind
  
}

p.update <- function(){   #sample individual effects
  
  p <- apply((dgrow - teffect - ieffect - mumat),1,sum,na.rm=T)
  pmat[pindex] <- p
  v  <- apply(pmat,2,sum,na.rm=T)/sig
  V  <- 1/(ntp/sig + 1/sigp)
  p  <- tnorm(mplot,-.7,.7,V*v,sqrt(V))
  p[nm == 0] <- 0
  p
  
}

di.update_new <- function(){  # sample diameters
  
  #first yr 
  
  dtmp   <- dgrow
  dtmp[is.na(dtmp)] <- 0
  
  delta  <- dgrow
  delta[is.na(delta)] <- 0
  delta  <- t(apply(delta,1,cumsum))
  delta  <- cbind(rep(0,n),delta)
  
  dmu <- delta*0
  dmu[dobs] <- dcens[dobs] - delta[dobs]
  dmu[dmu == 0] <- NA
  nid <- dmu
  nid[nid == 0] <- NA
  nid <- apply((nid*0+1),1,sum,na.rm=T)
  dmu <- apply(dmu,1,mean,na.rm=T)
  
  d00 <- tnorm(n,d0[,1],d0[,2],dmu,sqrt(w.error/nid))
  
  diam.t <- cbind(rep(0,n),dgrow)
  diam.t[cbind(c(1:n),firstyr.i)] <- d00
  
  diam.t[!is.finite(diam.t)] <- 0
  diam.t <- t(apply(diam.t,1,cumsum))
  dgrow  <- t(apply(diam.t,1,diff,na.rm=T)) 
  dgrow[,firstyr.i] <- 0
  #  diam.t[surv != 1] <- NA
  
  #direct sample increments
  
  ddobs <- dcens[,-1] - diam.t[,-nt]
  
  lreg <- mu + ieffect + teffect + peffect   #regression
  lreg[is.na(dgrow)] <- NA
  
  V <- dgrow*0 + 1/sig
  v <- dgrow*0 + lreg/sig
  
  if(length(iobs) > 0){
    V[iobs] <- V[iobs] + 1/v.error
    v[iobs] <- v[iobs] + dincr[iobs]/v.error
  }
  
  V[is.finite(ddobs)] <- V[is.finite(ddobs)] + 1/w.error
  v[is.finite(ddobs)] <- v[is.finite(ddobs)] + ddobs[is.finite(ddobs)]/w.error
  
  V <- 1/V
  
  dgrow[aincr] <- tnorm(nrow(aincr),mindinc[aincr],maxdinc[aincr],(V*v)[aincr],sqrt(V[aincr]))
  
  diam.t <- cbind(rep(0,n),dgrow)
  diam.t[cbind(c(1:n),firstyr.i)] <- d00
  diam.t[!is.finite(diam.t)] <- 0
  diam.t <- t(apply(diam.t,1,cumsum))
  
  #errors:
  
  ss <- sum((dcens[dobs] - diam.t[dobs])^2,na.rm=T)   #diameter error
  sw <- 1/(rgamma(1,(w1 + ndobs/2),(w2 + .5*ss) )) 
  
  sv <- 0
  if(length(iobs) > 0){
    ss <- sum( (dgrow[iobs] - dincr[iobs])^2,na.rm=T)   #growth error
    sv <- 1/(rgamma(1,(v11 + .5*niobs),(v22 + .5*ss) ))
  }
  
  list(diam.t = diam.t, sw = sw, sv = sv, ad = ad, aa = aa)
}




di.update <- function(){  # sample diameters
  
  djump <- .02
  diam.t[is.na(surv) | surv < 1] <- NA          #yr not in data set are NA
  dgrow <- t(apply(diam.t,1,diff,na.rm=T)) 
  dnew  <- matrix(0,n,(nt-1))
  aa    <- rep(0,n)    #acceptance counter
  
  #propose diameters and increments
  dstart      <- tnorm(n,d0[,1],d0[,2],diam.t[cbind(c(1:n),firstyr.i)],.05)
  dnew[aincr] <- tnorm(nrow(aincr),mindinc[aincr],maxdinc[aincr],dgrow[aincr],djump)
  
  diamnew <- cbind(rep(0,n),dnew)
  diamnew[cbind(c(1:n),firstyr.i)] <- dstart
  diamnew <- t(apply(diamnew,1,cumsum))      #proposed diameters
  
  dnew[is.na(surv[,-nt]) | surv[,-nt] < 1] <- NA
  diamnew[is.na(surv) | surv < 1] <- NA
  
  lreg <- mumat + ieffect + teffect + peffect   #regression
  lreg[is.na(dgrow)] <- NA
  
  pnow <- diam.t*0
  pnew <- pnow
  
  #diameter data
  pnow[dobs] <- pnow[dobs] + dnorm(dcens[dobs],diam.t[dobs],sqrt(w.error),log=T)
  pnew[dobs] <- pnew[dobs] + dnorm(dcens[dobs],diamnew[dobs],sqrt(w.error),log=T)
  
  #regression
  pnow[,-1] <- pnow[,-1] + dnorm(dgrow,lreg,sqrt(sig),log=T)
  pnew[,-1] <- pnew[,-1] + dnorm(dnew,lreg,sqrt(sig),log=T)
  
  #increment data
  if(length(iobs) > 0){
    pnow[iobs] <- pnow[iobs] + dnorm(dincr[iobs],dgrow[iobs],sqrt(v.error),log=T)
    pnew[iobs] <- pnew[iobs] + dnorm(dincr[iobs],dnew[iobs],sqrt(v.error),log=T)
  }
  
  pnow <- apply(pnow,1,sum,na.rm=T)
  pnew <- apply(pnew,1,sum,na.rm=T)
  
  a <- exp(pnew - pnow)
  z <- runif(n,0,1)
  cindex <- which(z < a,arr.ind=T)         #accept tree-by-tree
  ad <- sum(diam.t[cindex,]*0 + 1,na.rm=T)
  aa[cindex] <- aa[cindex] + 1
  diam.t[cindex,] <- diamnew[cindex,]
  dgrow[cindex,]  <- dnew[cindex,]
  
  #errors:
  
  ss <- sum((dcens[dobs] - diam.t[dobs])^2,na.rm=T)   #diameter error
  sw <- 1/(rgamma(1,(w1 + ndobs/2),(w2 + .5*ss) )) 
  
  ss <- sum( (dgrow[iobs] - dincr[iobs])^2,na.rm=T)   #growth error
  sv <- 1/(rgamma(1,(v11 + .5*niobs),(v22 + .5*ss) ))
  if(length(iobs) == 0)sv <- 0
  
  list(diam.t = diam.t, sw = sw, sv = sv, ad = ad, aa = aa)
}

sd.update <- function(){    #variance on random effects
  
  1/rgamma(1,(vi1 + n/2),(vi2 + .5*sum(beta.i^2) ))
  
}

sp.update <- function(){    #variance on random plot effects
  
  1/rgamma(1,(pi1 + mplot/2),(pi2 + .5*sum(beta.p^2) ))
  
}

se.update <- function(){   #process error
  
  ss <- sum(  (dgrow - mumat - ieffect - teffect - peffect)^2,na.rm=T)
  1/(rgamma(1,(s1 + .5*sum(nti)),(s2 + .5*ss) ))
  
}


#####################################################################################

###############################################
###                                         ###
###             initialization              ###
###                                         ###
###############################################

### set up input/output folders
REMOTE  <- FALSE    ## if true produced graphics; if false, assumes running on node w/o graphics
INCREMENTS <- FALSE ## if true, plots increment data 
outfolder <- settings$outdir  ## output folder for saving files

### set up years
yrvec <- as.numeric(colnames(diameters[[1]]))
nyr     <- length(yrvec)
beginyr <- yrvec[1]
endyr   <- yrvec[nyr]

ierr <- .1           #width (in cm) of error window for increments
mind    <- .005
maxd    <- 3

#all plots
mplot   <- length(diameters)   ## number of sites
dcens   <- numeric(0)          ## combined diameter data
dincr   <- numeric(0)          ## combined increment data
surv    <- numeric(0)          ## combined survival data
ijindex <- numeric(0)          ## indexing for combined data

## if not provided, assume all trees survive
if(is.null(survival)){
  survival <- list()
  for(i in 1:mplot){
    if(length(diameters[[i]]) == 0) next
    survival[[i]] <- matrix(TRUE,nrow(diameters[[i]]),nyr)
  }
}

#stack data from all plots
for(j in 1:mplot){    
  if(length(survival[[j]]) == 0)next

  wc    <- match(colnames(diameters[[j]]),yrvec)
  nr    <- nrow(diameters[[j]])

  dc    <- matrix(NA,nr,length(yrvec))
  dc[,wc] <- diameters[[j]]
  dcens <- rbind(dcens,dc)

  di    <- matrix(NA,nr,(length(yrvec)-1))
  di[,wc[-length(wc)]] <- increment[[j]]
  dincr <- rbind(dincr,di)

  sv <- matrix(NA,nr,length(yrvec))
  sv[,wc] <- survival[[j]]
  surv <- rbind(surv,sv)

  ijindex <- rbind(ijindex,cbind(rep(j,nr),c(1:nr)))

}
dincr[dincr < 0] = NA
mtree <- sapply(diameters,nrow)

n       <- nrow(dcens)    ## number of trees
nt      <- ncol(dcens)   ## number of years 
dobs    <- which(is.finite(dcens),arr.ind=T)   #diameter obs
nod     <- which(!is.finite(dcens) & surv == 1,arr.ind=T) #no diam obs
ndobs   <- nrow(dobs)
iobs    <- which(is.finite(dincr),arr.ind=T)   #incr obs
noi     <- which(!is.finite(dincr) & surv[,-nt] == 1,arr.ind=T) #no diam obs
niobs   <- nrow(iobs)
  if(length(niobs) == 0)niobs <- 0

time    <- c(1:length(yrvec))
tindex  <- time[-nt]          #time index for plot 2


mindinc <- matrix(mind,nrow(dincr),ncol(dincr))  ## minimum incerment
maxdinc <- maxd + mindinc*0                      ## maximum increment
mindinc[iobs] <- dincr[iobs] - ierr
maxdinc[iobs] <- dincr[iobs] + ierr
mindinc[mindinc < mind] <- mind
maxdinc[maxdinc < mindinc] <- maxd  

  dtmp      <- diamint()
  diam.t    <- dtmp$diamt
  d0        <- dtmp$d0
  firstyr.i <- dtmp$firstyri
  dgrow     <- t(apply(diam.t,1,diff,na.rm=T))       #diameter increment

  dplot   <- matrix(rep(ijindex[,1],each=(nt-1)),n,(nt-1),byrow=T)
  dplot[is.na(dgrow)] <- NA
  ntt     <- t(apply((dgrow*0 + 1),2,sum,na.rm=T)) #values per yr
  nti     <- t(apply((dgrow*0 + 1),1,sum,na.rm=T)) #values per individual
  ntmp    <- table(dplot,deparse.level=0)          #values per plot
  ntp  <- rep(0,mplot)
  pii  <- match(as.numeric(unlist(dimnames(ntmp))),c(1:mplot))
  ntp[pii] <- ntmp
  
  nm      <- table(cut(ijindex[,1],c(0:mplot)))    #trees per plot

pmat    <- matrix(0,max(nm),mplot)              #matrix to hope plot values
pindex  <- numeric(0)
for(j in 1:mplot){
   if(nm[j] == 0)next
   jj     <- rep(j,nm[j])
   pindex <- rbind(pindex,cbind(c(1:nm[j]),jj))
}

aincr   <- which(is.finite(dgrow) & is.finite(surv[,-nt]) & 
           surv[,-nt] == 1,arr.ind=T)    #intervals that individual is alive

plotyear <- rep(1,mplot)
#plotyear[plotnames %in% c('218','318','427')] <- 2
#plotyear[plotnames == 527] <- 3
#plotyear[plotnames %in% c('LG','UG')] <- 4
#plotyear[plotnames %in% c('MHP','MHF')] <- 5
#plotyear[plotnames %in% c('BW','HW','EW')] <- 6

ny <- max(plotyear)
nindex <- rep(plotyear,times=nm)



##################  COVARIATES ######################

COVARIATES <- FALSE     # T if there are covariates
ncovars    <- 0
#covariates
if(COVARIATES){
  ncovars   <- 1  #number of covariates
  X         <- matrix(1,nrow(aincr),(ncovars+1))
  nx        <- nrow(X)
  X[,2]     <- rnorm(nx,(dgrow[aincr]*.5 + 1),.1)  #simulated data
  prior.mu  <- rep(0,(1+ncovars))
  prior.Vmu <- rep(10,(1+ncovars))
  prior.IVm <- solve(diag(prior.Vmu))

  tmat  <- matrix(0,nrow(X),(nt-1))               #matrix for year effects
  tvector  <- matrix(c(1:(nt-1)),nrow(dgrow),(nt-1),byrow=T)[aincr]
  tind     <- cbind(c(1:nx),tvector)
  tmat[tind] <- tvector

}

############   PRIORS   #################
prior.mu  <- .3      #prior mean variance for mean growth rate
prior.Vmu <- 10

#lo.t <- rep(-4,(nyr-1))
#hi.t <- lo.t*0 + 4
#lo.t[nyr-1] <- -.0001
#hi.t[nyr-1] <- .0001

mui  <- .4           #individual effects var
vi1  <- 2
vi2  <- mui*(vi1 - 1)

mup  <- .4           #plot effects var
pi1  <- 2
pi2  <- mup*(pi1 - 1)

mus  <- .1^2            #process error
s1   <- 10
s2   <- mus*(s1 - 1)

muw  <- .2^2            #diameter error
w1   <- ndobs*2
w2   <- muw*(w1 - 1)

muv  <- .02^2        #increment error
v11  <- niobs
v22  <- muv*(v11 - 1)
  if(niobs == 0){v11 <- 0; v22 <- 0}

#############  initial values ################
mu      <- tnorm(1,0,1,prior.mu,1)
sig     <- 1/rgamma(1,s1,s2)
sigd    <- 1/rgamma(1,vi1,vi2)
sigp    <- 1/rgamma(1,pi1,pi2)
w.error <- 1/rgamma(1,w1,w2)
v.error <- 1/rgamma(1,vi1,vi2)
beta.i  <- rep(0,n)             #individual random effects
beta.p  <- rep(0,mplot)         #plot random effects
beta.t  <- rep(0,(nt-1))        #fixed year effects
  peffect <- matrix( rep(beta.p,times=(nm*(nt-1))),nrow=n,byrow=T)
  mumat   <- peffect*0
  teffect <- matrix(rep(beta.t,each=n),nrow=n,byrow=F)

ng <- 5000    ## length of MCMC
nrep <- 100
burnin <- 400  ## length of burn-in
printseq <- seq(10,ng,by=20)

####### Storage #########
if(COVARIATES){
  agibbs <- matrix(NA,ng,(ncovars+1))
  colnames(agibbs) <- paste('a',c(1:(ncovars+1)),sep='-')
}
mgibbs  <- rep(0,ng)
bigibbs <- rep(0,n)
bpgibbs <- rep(0,mplot)
bp2gibbs <- rep(0,mplot)
ingibbs <- matrix(0,ng,4)  #a sample of individuals
  ins  <- sort(sample(c(1:n),4))
sgibbs  <- matrix(0,ng,5)
  colnames(sgibbs) <- c('proc','ind','plot','diam','incr')
tgibbs  <- matrix(0,ng,(nt-1)); colnames(tgibbs) <- yrvec[-nt]
dgibbs  <- diam.t*0         #sums and sums of squares, diam
d2gibbs <- diam.t*0
ggibbs  <- diam.t[,-nt]*0   #sums and sums of squares, growth
g2gibbs <- diam.t[,-nt]*0
ad <- rep(0,ng)            #acceptance counter by step
ai <- rep(0,n)             #acceptance counter by tree


#log scale
ldgibbs  <- diam.t*0         #sums and sums of squares, diam
ld2gibbs <- diam.t*0
lggibbs  <- diam.t[,-nt]*0   #sums and sums of squares, growth
lg2gibbs <- diam.t[,-nt]*0

printseq <- seq(10,ng,by=100)

##full data
to.save <- floor(seq(burnin,ng,length=nrep))
full.dia <- list()
  

######################################################
####                                              ####
####                   MCMC                       ####
####                                              ####
######################################################
for(g in 1:ng){

    ftmp <- f.update()
    mu      <- ftmp$mu
 #   beta.t  <- ftmp$beta.t
    alpha   <- ftmp$alpha

    muVec   <- ftmp$muVec
    betaMat <- ftmp$betaMat

    teffect <- betaMat[nindex,]

    mumat   <- mu
    if(length(mu) == 1)mumat <- matrix(mu,n,(nt-1))
    if(!COVARIATES)mumat <- matrix(muVec[nindex],n,(nt-1))

    beta.i <- in.update()
    ieffect <- matrix(rep(beta.i,each=(nt-1)),nrow=n,byrow=T)

  if(mplot > 1) {
     beta.p <- p.update()
     sigp <- sp.update()  #plots
  }
  if(mplot == 1){
    beta.p <- 0
    sigp   <- 0
  }
  peffect <- matrix( rep(beta.p,times=(nm*(nt-1))),nrow=n,byrow=T)

    sig  <- se.update()  #proc
    sigd <- sd.update()  #individuals

     dtmp <- di.update()
     diam.t  <- dtmp$diam.t
     w.error <- dtmp$sw  #error diam
     v.error <- dtmp$sv  #error growth

  if(g < 100)v.error <- muv

     ad[g]   <- dtmp$ad
     ai      <- ai + dtmp$aa
     dtmp    <- diam.t
     dtmp[is.na(surv) | surv != 1] <- NA
 #    dtmp[cbind(c(1:n),firstyr.i)] <- 0
     dgrow   <- t(apply(dtmp,1,diff,na.rm=T))   #diameter increment

  if(!COVARIATES)mgibbs[g]  <- mu
  if(COVARIATES) agibbs[g,] <- alpha

  ingibbs[g,] <- beta.i[ins]
  tgibbs[g,]  <- beta.t
  sgibbs[g,]  <- c(sig,sigd,sigp,w.error,v.error)
  if(g > burnin){

    bigibbs     <- bigibbs + beta.i
    bpgibbs     <- bpgibbs + beta.p
    bp2gibbs     <- bp2gibbs + beta.p^2
    dgibbs      <- dgibbs + diam.t
    d2gibbs     <- d2gibbs + diam.t^2
    ggibbs      <- ggibbs + dgrow
    g2gibbs     <- g2gibbs + dgrow^2
    ldgibbs     <- ldgibbs + log(diam.t)
    ld2gibbs    <- ld2gibbs + log(diam.t)^2
    lggibbs     <- lggibbs + log(dgrow)
    lg2gibbs    <- lg2gibbs + log(dgrow)^2
  } 
    if(g %in% printseq){
#     print(g)
#     print(betaMat)#[5:6,])
  }
  if(g %in% to.save){
    print(g)
    full.dia[[which(to.save==g)]] <- diam.t    
    print(is.null(full.dia[[which(to.save==g)]]))
  }
}
########### END MCMC ##########

print('after diameter gibbs')

keep <- c(burnin:g)
nk   <- length(keep)

print('after diameter gibbs')

mdiam <- dgibbs/nk
sdiam <- sqrt(d2gibbs/nk - mdiam^2)
mgrow <- ggibbs/nk
sgrow <- sqrt(g2gibbs/nk - mgrow^2)
peff  <- bpgibbs/nk
#names(peff) <- plotnames
sdp   <- sqrt(bp2gibbs/nk - peff^2)
pci   <- cbind((peff-1.96*sdp),(peff+1.96*sdp))
#  rownames(pci) <- plotnames
  pci[mtree == 0,] <- NA

mldiam <- ldgibbs/nk
sldiam <- sqrt(ld2gibbs/nk - mldiam^2)
mlgrow <- lggibbs/nk
slgrow <- sqrt(lg2gibbs/nk - mlgrow^2)

#priors and posteriors

nall <- sum(nti)
vp <- matrix(c(s1,s2,mus,vi1,vi2,mui,pi1,pi2,mup,w1,w2,muw,v11,v22,muv),5,3,byrow=T)
  rownames(vp) <- c('process','ind effect','plot effect','diameter','growth')

if(COVARIATES)mgibbs <- agibbs

estimate <- c(apply(cbind(mgibbs,sgibbs,tgibbs)[keep,],2,mean),peff)
std_err  <- c(apply(cbind(mgibbs,sgibbs,tgibbs)[keep,],2,sd),sdp)
p3       <- t(apply(cbind(mgibbs,sgibbs,tgibbs)[keep,],2,quantile,c(.025,.975)))
p3       <- rbind(p3,pci)
nn       <- c(rep(nall,(ncovars+2)),n,mplot,ndobs,niobs,ntt,ntp)
p3       <- cbind(nn,estimate,std_err,p3)


pvals <- cbind(prior.mu,prior.Vmu,prior.mu)
pvals <- rbind(pvals,vp)

diampars <- matrix(NA,nrow(p3),(ncol(p3)+ncol(pvals)))
diampars[,1:5] <- p3
diampars[1:nrow(pvals),6:8] <- pvals
rownames(diampars) <- names(estimate)
colnames(diampars) <- c(colnames(p3),'par1','par2','prior mean')

outfile <- paste(outfolder,'diampars.txt',sep='')
  write.table(signif(diampars,3),outfile,row.names = TRUE,col.names=TRUE,quote=FALSE)

#determine posterior means and sd's for diameter, growth, and other columns in treemat

treeindex <- cumsum(mtree)
treeindex <- cbind(c(1,(1+treeindex[1:(mplot-1)])),treeindex)
treeindex[mtree == 0,2] <- treeindex[mtree == 0,1]
if(mplot == 1)treeindex <- matrix(treeindex[1,],1,2)

#tre
meandiam <- numeric(0)  #log diameter
sddiam   <- numeric(0)
meangrow <- numeric(0)
sdgrow   <- numeric(0)
mg       <- numeric(0)
lgdiam   <- numeric(0)
lgdinc   <- numeric(0)
lgdisd   <- numeric(0)
icol     <- numeric(0)
jcol     <- numeric(0)
tcol     <- numeric(0)
ticol    <- numeric(0)
surv     <- numeric(0)

if(length(beginyr) < mplot) beginyr <- rep(beginyr,mplot)
if(length(endyr) < mplot) endyr <- rep(endyr,mplot)
if(length(nyr) < mplot) nyr <- rep(nyr,mplot)

for(j in 1:mplot){
  if(mtree[j] == 0){
     meandiam <- append(meandiam,list(numeric(0)))
     sddiam   <- append(sddiam,list(numeric(0)))
     meangrow <- append(meangrow,list(numeric(0)))
     sdgrow   <- append(sdgrow,list(numeric(0)))
     next
  }
     yj <- c(beginyr[j]:endyr[j])
     wy <- which(yrvec %in% yj,arr.ind=T)
     yjvec <- yrvec[wy]
     wj <- which(ijindex[,1] == j,arr.ind=T)
     ny <- length(yjvec)
     ng <- ny - 1
     md <- matrix(mldiam[wj,wy],length(wj),ny)
     sd <- matrix(sldiam[wj,wy],length(wj),ny)
     mg <- matrix(mlgrow[wj,wy[-length(wy)]],length(wj),ng)
     sg <- matrix(slgrow[wj,wy[-length(wy)]],length(wj),ng)
     colnames(md) <- yjvec
     colnames(sd) <- yjvec
     colnames(mg) <- yjvec[-length(yjvec)]
     colnames(sg) <- yjvec[-length(yjvec)]

    #remove NA's, extra column for growth rates
     mgna     <- which(is.na(mg),arr.ind=T)
     if(length(mgna) > 0){
       meanmg   <- apply(mg,1,mean,na.rm=T)
       mg[mgna] <- meanmg[mgna[,1]]
     }
       mg       <- cbind(mg,mg[,(nyr[j]-1)])

     growmat <- exp(mg)
     cumgrow <- t(apply(growmat,1,cumsum))
     wna    <- which(is.na(md),arr.ind=T)
     if(length(wna) > 0)wna    <- sort(unique(wna[,1]))
     yjvec  <- c(1:nyr[j])

     for(w in wna){
       lfit    <- lm(md[w,] ~ yjvec)
       newvals <- predict.lm(lfit,newdata=data.frame(yjvec))
       md[w,is.na(md[w,])] <- newvals[is.na(md[w,])]

       check <- diff(md[w,])
       if(min(check) < 0){
         check[check < 0] <- .00001
         dnew <- cumsum(c(md[w,1],check))
         md[w,] <- dnew
       }
     }

     sdna     <- which(is.na(sd),arr.ind=T)
     sd[sdna] <- 1
     sgna     <- which(is.na(sg),arr.ind=T)
     sg[sgna] <- 1
     sg       <- cbind(sg,rep(1,nrow(sg)))

     meandiam <- append(meandiam,list(md))
     sddiam   <- append(sddiam,list(sd))
     meangrow <- append(meangrow,list(mg))
     sdgrow   <- append(sdgrow,list(sg))

     lgdiam  <- c(lgdiam,as.vector(t(md)))
     lgdinc  <- c(lgdinc,as.vector(t(mg)))
     lgdisd  <- c(lgdisd,as.vector(t(sg)))

     yb    <- match(beginyr[j],yrvec)
     ye    <- match(endyr[j],yrvec)
     tcol  <- c(tcol,rep(c(yb:ye),mtree[j]))
     jcol  <- c(jcol,rep(j,(nyr[j]*mtree[j])))
     icol  <- c(icol,rep(c(1:mtree[j]),each=(ye - yb + 1)))
     ticol <- c(ticol,rep(c(treeindex[j,1]:treeindex[j,2]),each=(ye - yb + 1)))
     surv  <- c(surv,as.vector(t(survival[[j]])))
}



##plot treerings


if(INCREMENTS){

plotfile <- paste(outfolder,"incrementdata.ps",sep="")
plotstart(plotfile)

par(mfrow=c(6,2),mar=c(1,1,2,1),bty='n')
 for(j in 1:mplot){

  if(mtree[j] == 0)next
  yindex <- c(1:nyr[j])
  tindex <- yindex + beginyr[j] - yrvec[1]

  yjvec <- yrvec[tindex]
  rframe <- NULL#intreering(j,yjvec[1],yjvec[nyr[j]],order[[j]])
  if(length(rframe) == 0)next
  if(nrow(rframe) == 0)next

  ordring <- sort(unique(rframe[,'order']))
  r1 <- rframe[rframe[,'order'] == ordring[1],]
  plot(r1[,'yr'],r1[,'cm'],type='l',xlim=c(1990,2010),ylim=c(0,2))
  for(i in 1:length(ordring)){
    r1 <- rframe[rframe[,'order'] == ordring[i],]
    lines(r1[,'yr'],r1[,'cm'])
  }
#  title(plotnames[j])

  }

plotend(plotfile)

}

#plotfile <- paste(outfolder,spgroup,'diamchains.ps',sep='')
#plotstart(plotfile)

#  par(mfrow=c(3,3))
#  for(j in 1:5){
#    plot(sgibbs[,j],type='l')
#    title(colnames(sgibbs)[j])
#  }
#  kkk <- 1
#  if(!is.matrix(mgibbs))plot(mgibbs,type='l')
#  if(is.matrix(mgibbs))for(k in 1:ncol(mgibbs))plot(mgibbs[,k],type='l')
#  plot(ad/sum(ntt),type='l'); title('acceptance rate')
#  aii <- ai/g
#  wlo <- which(aii < .3,arr.ind=T)  #which trees have low acceptance rates?
#  hist(aii,probability=T,main='acceptance by tree')

#plotend(plotfile)


plotfile <- paste(outfolder,'diampost.ps',sep='')
plotstart(plotfile)

vs <- seq(.00001,1.2,length=200)

prior.mu  <- .3      #prior mean variance for mean growth rate
prior.Vmu <- 10

par(mfrow=c(3,2))
for(j in 1:5){    
  mj <- vp[j,2]/(vp[j,1] - 1)
  if(max(sgibbs[keep,j],na.rm=T) == 0)next
  dj  <- density(sgibbs[keep,j])
  xm  <- 1.5*max(dj$x)
  xl  <- .5*min(dj$x)
  vss <- seq(xl,xm,length=200)
  plot(dj$x,dj$y,type='l',lwd=2,xlim=c(xl,xm),xlab='Parameter value',ylab='Density')
  lines(vss,vss^(-2)*dgamma(1/vss,vp[j,1],vp[j,2]),col='darkgreen',lwd=2) #include Jacobian
  title(colnames(sgibbs)[j])
}
vt <- seq(-.3,.3,length=100)
plot(vt,dnorm(vt,0,sqrt(prior.Vmu)),col='darkgreen',type='l',lwd=2,ylim=c(0,60),
  xlab='Parameter value',ylab='Density')
title('yr effects')
for(j in 1:(nt-1)){
  dj <- density(tgibbs[keep,j])
  lines(dj$x,dj$y,type='l',lwd=2)
}

plotend(plotfile)


plotfile <- paste(outfolder,'diamvars.ps',sep='')
plotstart(plotfile)

#var comparison
  sdi <- apply(tgibbs,1,sd) 

par(mfrow=c(2,1))
meanyr <- apply(tgibbs[keep,],2,mean)

plot(yrvec[-nt],log10(mgrow[1,]),ylim=c(-2,.5),type='l',
   xlab="Year",ylab="Diameter increment (log cm)")
  for(i in 1:n){
    lines(yrvec[-nt],log10(mgrow[i,]))
  }

#dj <- density(log10(sdi))
#plot(dj$x,dj$y,type='l',xlim=c(-2,0),ylim=c(0,240),
#     ylab='Density',xlab="Standard deviation (log scale)")
#  text(mean(log10(sdi)),(20+max(dj$y)),'yr')
#  abline(h=0)

  dj <- density(log10(sqrt(sgibbs[keep,'ind']+sgibbs[keep,'plot'])))
plot(dj$x,dj$y,type='l',
     ylab='Density',xlab="Standard deviation (log scale)")
  text(mean(log10(sdi)),(20+max(dj$y)),'yr')
  abline(h=0)
#  lines(dj$x,dj$y,lwd=2,col='red')
  text(mean(log10(sqrt(sgibbs[keep,'ind']+sgibbs[keep,'plot']))),
     (50+max(dj$y)),'population',col='red')

for(j in 1:5){
  if(min(sgibbs[keep,j]) == max(sgibbs[keep,j]))next
  dj <- density(log10(sqrt(sgibbs[keep,j])))
  lines(dj$x,dj$y)
  text(mean(log10(sqrt(sgibbs[keep,j]))),(20+max(dj$y)),colnames(sgibbs)[j])
}

plotend(plotfile)

#plotfile <- paste(outfolder,spgroup,'diamyr.ps',sep='')
#plotstart(plotfile)

#par(mfrow=c(4,4))
#for(j in 1:(nt-1)){
#  plot(tgibbs[keep,j],type='l',ylim=c(-.4,.4))
#  title(colnames(tgibbs)[j])
#  abline(h=0,lty=2)
#}

#plotend(plotfile)

#plotfile <- paste(outfolder,spgroup,'diam_ind.ps',sep='')
#plotstart(plotfile)

#par(mfrow=c(2,2))
#for(j in 1:4){
#  plot(ingibbs[keep,j],type='l',ylim=c(-.3,.3))
#  title(ins[j])
#  abline(h=0,lty=2)
#}

#plotend(plotfile)

if(COVARIATES){
  
  plotfile <- paste(outfolder,'diam_ind.ps',sep='')
  plotstart(plotfile)

  par(mfrow=c(2,2))
  for(j in 1:ncol(mgibbs)){
    plot(mgibbs[keep,j],type='l')
 #   title(ins[j])
    abline(h=0,lty=2)
  }

  plotend(plotfile)
}


plotfile <- paste(outfolder,'diam_fit.ps',sep='')  #large file
plotstart(plotfile)

#diameters and growth rates

##  par(mfrow=c(2,2))
  par(mfrow=c(1,1))

  if(length(iobs) > 0){
  lo <- mgrow[iobs] - 1.96*sgrow[iobs]
  hi <- mgrow[iobs] + 1.96*sgrow[iobs]
  xx <- dincr[iobs]
  plot(xx,mgrow[iobs],xlab='observed',ylab='predicted')
  abline(-ierr,1,lty=2)
  abline(ierr,1,lty=2)
  for(i in 1:nrow(iobs)){
    lines(c(xx[i],xx[i]),c(lo[i],hi[i]))
  }
  }

  lo <- mdiam[dobs] - 1.96*sdiam[dobs]
  hi <- mdiam[dobs] + 1.96*sdiam[dobs]
  xx <- dcens[dobs]
  plot(dcens[dobs],mdiam[dobs],xlab='observed',ylab='predicted')
  abline(0,1,lty=2)
  for(i in 1:nrow(dobs)){
    coll <- 'black'
    if(dobs[i,2] == max(nyr))coll <- 'red'
    lines(c(xx[i],xx[i]),c(lo[i],hi[i]),col=coll)
  }

plotend(plotfile)

if(!REMOTE){

  jj <- 1

  jj <- jj + 1
  iplot <- sort(sample(c(1:n),5))
  par(mfrow=c(5,2))
  par(mar=c(3,2,2,1))

  for(j in 1:5){
    md  <- exp(mldiam[iplot[j],])
    lsd <- exp(mldiam[iplot[j],] - 1.96*sldiam[iplot[j],])
    hsd <- exp(mldiam[iplot[j],] + 1.96*sldiam[iplot[j],])
    mg  <- exp(mlgrow[iplot[j],])
    lsg <- exp(mlgrow[iplot[j],] - 1.96*slgrow[iplot[j],])
    hsg <- exp(mlgrow[iplot[j],] + 1.96*slgrow[iplot[j],])
  #  y1 <- max(0,(mean(md,na.rm=T) - 6))
  #  y2 <- y1 + 12
    y1 <- min(lsd,na.rm=TRUE)
    y2 <- max(hsd,na.rm=TRUE)
    id <- paste(ijindex[iplot[j],1],ijindex[iplot[j],2],iplot[j],sep=', ')
    id <- ijindex[iplot[j],2]

    plot(yrvec,md,type='l',ylim=range(c(y1,y2,dcens[iplot[j],]),na.rm=TRUE),xlab=' ',ylab=' ')
    lines(yrvec,lsd,lty=2)
    lines(yrvec,hsd,lty=2)
    lines(yrvec,mdiam[iplot[j],],col=3)
    points(yrvec,dcens[iplot[j],])
    title(id)
    plot(yrvec[-nt],mg,type='l',ylim=c(0,max(hsg,na.rm=TRUE)),xlab=' ',ylab=' ')
    lines(yrvec[-nt],lsg,lty=2)
    lines(yrvec[-nt],hsg,lty=2)
    lines(yrvec[-nt],mgrow[iplot[j],],col=3)
    points(yrvec[-nt],dincr[iplot[j],])
  }

#outfile <- paste(outfolder,spgroup,'diam_pred',jj,'.ps',sep='')
#dev.print(device=postscript,file=outfile,width=6, height=8, horizontal=FALSE)

}

#columns for treemat

 diam      <- signif(exp(lgdiam),4)

 #we now have the following columns for treemat: tindex,j,i,t,surv,diam,lgdiam,lgdinc,lgdisd
 treematindex <- cbind(ticol,jcol,icol,tcol)
 colnames(treematindex) <- c('tindex','j','i','t')

# if( length(grep('ordvec',objects())) == 0 )order <- tindex
# if( length(grep('ordvec',objects())) > 0 )order  <- ordvec
# ordlist <- order



print('after diameter analysis')

#################    FINAL OUTPUT     ##################
save.image(paste(outfolder,"DBH.Rdata",sep=""))
save(yrvec,mdiam,sdiam,mgrow,sgrow,full.dia,ijindex,mplot,file=paste(outfolder,"DBH_summary.Rdata",sep=""))
## mdiam -- modeled diameter mean
## sdiam -- modeled diameter s.d.
## mgrow -- modeled growth mean
## sgrow -- modeled growth s.d.

## equivalents exist for log-transformed values as well
print('after save')
}
