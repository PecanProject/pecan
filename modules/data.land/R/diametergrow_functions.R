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

  for(i in 1:n){

    wf <- min(c(1:nt)[is.finite(surv[i,]) & surv[i,] > 0],na.rm=T)
    wl <- max(c(1:nt)[is.finite(surv[i,]) & surv[i,] > 0],na.rm=T)
    first[i] <- wf

    wi <- which(is.finite(dcens[i,]),arr.ind=T)
    xi <- time[wi]
    yi <- dcens[i,wi]
    intercept <- mean(yi) - mean(xi)*( cov(xi,yi)/var(xi) )
    if(!is.finite(intercept))intercept <- min(yi,na.rm=T)
     if(intercept < .1)intercept <- max(.1,(min(yi) - 5) )
     slope <- (mean(xi*yi) - intercept*mean(xi) )/mean(xi^2)
     if(slope < .001){
        slope     <- .001
        intercept <- mean(yi) - slope*mean(xi)
     }
     dfit <- intercept + slope*time
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


#all plots
mplot   <- length(diameters)
dcens   <- numeric(0)
dincr   <- numeric(0)
surv    <- numeric(0)
ijindex <- numeric(0)

for(j in 1:mplot){    #stack data from all plots

  if(length(surviv[[j]]) == 0)next

  wc    <- match(colnames(diameters[[j]]),yrvec)
  nr    <- nrow(diameters[[j]])

  dc    <- matrix(NA,nr,length(yrvec))
  dc[,wc] <- diameters[[j]]
  dcens <- rbind(dcens,dc)

  di    <- matrix(NA,nr,(length(yrvec)-1))
  di[,wc[-length(wc)]] <- increment[[j]]
  dincr <- rbind(dincr,di)

  sv <- matrix(NA,nr,length(yrvec))
  sv[,wc] <- surviv[[j]]
  surv <- rbind(surv,sv)

  ijindex <- rbind(ijindex,cbind(rep(j,nr),c(1:nr)))

}




