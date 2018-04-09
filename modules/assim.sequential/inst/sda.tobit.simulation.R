##' @title sda.tobit.simulation
##' @name  sda.tobit.simulation
##' @author Ann Raiho \email{araiho@nd.edu}
##' 
##' @description This is a script for testing the tobit sda model with simulated data
##' 
##' @return NONE
##' @export
##' 
library(nimble)
library(mvtnorm)

sampler_toggle <- nimbleFunction(
  contains = sampler_BASE,
  setup = function(model, mvSaved, target, control) {
    type <- control$type
    nested_sampler_name <- paste0('sampler_', type)
    control_new <- nimbleOptions('MCMCcontrolDefaultList')
    control_new[[names(control)]] <- control
    nested_sampler_list <- nimbleFunctionList(sampler_BASE)
    nested_sampler_list[[1]] <- do.call(nested_sampler_name, list(model, mvSaved, target, control_new))
    toggle <- 1
  },
  run = function() {
    if(toggle == 1)
      nested_sampler_list[[1]]$run()
  },
  methods = list(
    reset = function()
      nested_sampler_list[[1]]$reset()
  )
)

tobit.model <- nimbleCode({ 
  
  q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
  Q[1:N,1:N] <- inverse(q[1:N,1:N])
  X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
  
  ## add process error
  X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])
  
  #agb linear
  #y_star[1:N,1:N] <- X[1:N,1:N] #[choose]
  
  #f.comp non linear
  #y_star <- X[1:9] / sum(X[1:9])
  
  ## Analysis
  y.censored[1:N] ~ dmnorm(X[1:N], prec = r[1:N,1:N])
  
  #don't flag y.censored as data, y.censored in inits
  #remove y.censored samplers and only assign univariate samplers on NAs
  
  for(i in 1:N){
    y.ind[i] ~ dconstraint(y.censored[i] > 0)
  }
  
})

library(mvtnorm)

ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}

wish.df <- function(Om,X,i,j,col){
  n = (Om[i,j]^2 + Om[i,i]*Om[j,j])/var(X[,col])
  return(n)
}

#### Simulate some random walk data

nt = 50
m = c(1.03,.9)
model = matrix(0,nt,2) ; Y.dat = model ; y.ind = model ; y.censored = model
model[1,] = c(0,10)
q = diag(2) #process variance
r = diag(2)*2#observation error #the lower you make this the better the convergence on the covariance matrix

for(t in 2:nt){
  model[t,] = rmvnorm(1,m*model[t-1,],q)
}

for(t in 1:nt){
  Y.dat[t,] = rmvnorm(1,model[t,],r)
  y.ind[t,] <- as.numeric(Y.dat[t,]>0)
  y.censored[t,] <- as.numeric(ifelse(Y.dat[t,]>=0, Y.dat[t,], 0))
}

#### Plot data
plot(Y.dat[,1],ylim=range(Y.dat),pch=19)
lines(model[,1],lwd=2)
points(Y.dat[,2],col="blue",pch=18)
lines(model[,2],col="blue",lwd=2)

#### Storage arrays
aqq = array(0,dim=c(2,2,nt+1)); Sbar.save = aqq; Pf.save = aqq; q.bar.save = aqq; Pa.save = aqq
bqq = numeric(nt+1);
Sbar.CI = array(0,dim=c(3,4,nt)); q.bar.CI = Sbar.CI
dat.save = array(0,dim=c(501,8,nt))
CI.X1 <- matrix(0,3,nt) ; CI.X2 = CI.X1

#### initial conditions
mu.f = t(rmvnorm(1,m*model[1,],q))
Pf   = solve(cov(model))

bqq[1] <- 2
aqq[,,1] <- solve(q)/bqq[1]

t = 1

#y.obs = Y.dat[1,]
constants.tobit = list(N = 2) #, nc = 1
dimensions.tobit = list(X = 2, X.mod = 2, Q = c(2,2)) #  b = dim(inits.pred$b),

data.tobit = list(muf = as.vector(mu.f), pf = Pf, aq = aqq[,,t], bq = bqq[t],
                  y.ind = y.ind[t,],
                  y.censored = y.censored[t,],
                  r = solve(r))
inits.pred = list(q = diag(2), X.mod = as.vector(mu.f), X = rnorm(2,0,1)) #
#set.seed(0)
#ptm <- proc.time()
model_pred <- nimbleModel(tobit.model, data = data.tobit, dimensions = dimensions.tobit,
                          constants = constants.tobit, inits = inits.pred)
## Adding X.mod,q,r as data for building model.
conf <- configureMCMC(model_pred, print=TRUE)
conf$addMonitors(c("X","q","Q")) 
## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
## important!
## this is needed for correct indexing later
samplerNumberOffset <- length(conf$getSamplers())

for(i in 1:2) {
  node <- paste0('y.censored[',i,']')
  conf$addSampler(node, 'toggle', control=list(type='RW'))
  ## could instead use slice samplers, or any combination thereof, e.g.:
  ##conf$addSampler(node, 'toggle', control=list(type='slice'))
}

conf$printSamplers()

## can monitor y.censored, if you wish, to verify correct behaviour
conf$addMonitors('y.censored')

Rmcmc <- buildMCMC(conf)

Cmodel <- compileNimble(model_pred)
Cmcmc <- compileNimble(Rmcmc, project = model_pred)

#ptm <- proc.time()
for(t in 1:nt){
  
  Cmodel$y.ind <- y.ind[t,]
  Cmodel$y.censored <- y.censored[t,]
  Cmodel$aq <- aqq[,,t]
  Cmodel$bq <- bqq[t]
  Cmodel$muf <- mu.f
  Cmodel$pf <- Pf
  
  for(i in 1:2) {
    ## ironically, here we have to "toggle" the value of y.ind[i]
    ## this specifies that when y.ind[i] = 1,
    ## indicator variable is set to 0, which specifies *not* to sample
    valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[t,i])
  }
  
  set.seed(0)
  dat <- runMCMC(Cmcmc, niter = 10000, progressBar=FALSE)
  
  dat = dat[500:1000,]
  #dat.save[,,t] = dat
  mu.a  = colMeans(dat[,5:6]) #X
  Pa  = cov(dat[,5:6]) #cov(X)
  Pa.save[,,t] = Pa
  Pa[is.na(Pa)]<- 0 
  
  CI.X1[,t] = quantile(dat[,5],c(0.025,0.5,0.975))#X[1]
  CI.X2[,t] = quantile(dat[,6],c(0.025,0.5,0.975))#X[2]
  
  mq = dat[,1:4] #Sigma, Variance #Q
  mq1 = dat[,9:12] #Omega, Precision #q
  
  Sbar = matrix(apply(mq,2,mean),2,2) #Mean Sigma, Variance
  q.bar = matrix(apply(mq1,2,mean),2,2) #Mean Omega, Precision
  
  col = matrix(1:4,2,2)
  WV = matrix(0,2,2)
  for(i in 1:2){
    for(j in 1:2){
      WV[i,j] <- wish.df(q.bar, X = mq1, i=i, j=j, col=col[i,j])
    }
  }
  
  n = mean(WV) #n + 1
  if(n < 2) n = 2
  V = solve(q.bar) * n
  
  aqq[,,t+1] = V
  bqq[t+1] = n
  
  plot(bqq[1:t+1],pch=19)
  
  q.bar.save[,,t] = q.bar
  q.bar.CI[,,t] = apply(mq1,2,quantile,c(0.025,0.5,0.975))
  Sbar.save[,,t] = Sbar
  Sbar.CI[,,t] = apply(mq,2,quantile,c(0.025,0.5,0.975))
  
  ## Ensemble forward simulation
  Xf = rmvnorm(1000,m*mu.a,Pa)
  mu.f = t(colMeans(Xf))
  Pf = solve(cov(Xf))
  Pf.save[,,t] = Pf
}
#proc.time() - ptm

### degrees of freedom over time -> should be increasing because we are always getting more data
plot(bqq,xlab="Time",ylab="Degrees of Freedom of Wishart",pch=16)

### Data assimilation time series
plot(Y.dat[,1],ylim=range(Y.dat)+c(0,20),pch=19,xlab="Time",ylab="Xs")
lines(model[,1],lwd=2)
col=col2rgb("darkgrey")
col1=rgb(col[1],col[2],col[3],0.4*256,maxColorValue=256)
ciEnvelope(1:nt,CI.X1[1,],CI.X1[3,],col=col1)

points(Y.dat[,2],col="blue",pch=18)
lines(model[,2],col="blue",lwd=2)
col=col2rgb("lightblue")
col1=rgb(col[1],col[2],col[3],0.4*256,maxColorValue=256)
ciEnvelope(1:nt,CI.X2[1,],CI.X2[3,],col=col1)

## how well are we estimating process error (Q) ---> should be diag(2)
par(mfrow=c(2,2))
plot(Sbar.save[1,1,],ylim=range(Sbar.CI[,1,]))
abline(h=q[1,1])
ciEnvelope(1:nt,Sbar.CI[1,1,],Sbar.CI[3,1,],col=col1)

plot(Sbar.save[1,2,],ylim=range(Sbar.CI[,2,]))
abline(h=q[1,2])
ciEnvelope(1:nt,Sbar.CI[1,2,],Sbar.CI[3,2,],col=col1)

plot(Sbar.save[2,2,],ylim=range(Sbar.CI[,4,]))
abline(h=q[2,2])
ciEnvelope(1:nt,Sbar.CI[1,4,],Sbar.CI[3,4,],col=col1)

plot(Sbar.save[2,1,],ylim=range(Sbar.CI[,3,]))
abline(h=q[2,1])
ciEnvelope(1:nt,Sbar.CI[1,3,],Sbar.CI[3,3,],col=col1)

#### Looking for autocorrelation between process covariance and forecast covariance
par(mfrow=c(2,2))
plot(Pa.save[1,1,seq(2,50,2)],Sbar.save[1,1,seq(2,50,2)],pch=16,xlab="Pa",ylab="Sbar",main="Element [1,1]",ylim=c(-1,9),xlim=c(0,1.5))
points(Pa.save[1,1,seq(1,50,2)],Sbar.save[1,1,seq(1,50,2)],col="blue",pch=16)
abline(h=1)
abline(0,1)
plot(Pa.save[1,2,],Sbar.save[1,2,],pch=16,xlab="Pa",ylab="Sbar",main="Element [1,2]")
abline(h=0)
plot(Pa.save[2,1,],Sbar.save[2,1,],pch=16,xlab="Pa",ylab="Sbar",main="Element [2,1]")
abline(h=0)
plot(Pa.save[2,2,],Sbar.save[2,2,],pch=16,xlab="Pa",ylab="Sbar",main="Element [2,2]")
abline(h=1)
