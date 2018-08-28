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
library(PEcAn.assim.sequential)


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

# `row[i] ~ dcat(weights)` and then loop over the data as `X[row[j],]`

tobit.model <- nimbleCode({ 
  
  q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
  Q[1:N,1:N] <- inverse(q[1:N,1:N])
  X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
  
  ## add process error
  X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])
  
  #observation operator
  y_star[1:YN] <- X[1:N]#y_star_create(X[1:YN])
  
  ## Analysis
  y.censored[1:YN] ~ dmnorm(y_star[1:YN], prec = r[1:YN,1:YN]) #is it an okay assumpution to just have X and Y in the same order?

  
  #don't flag y.censored as data, y.censored in inits
  #remove y.censored samplers and only assign univariate samplers on NAs
  
  for(i in 1:YN){
    y.ind[i] ~ dinterval(y.censored[i], 0)
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


## Simulate Forecast Data
nens <- 50
X <- matrix(0,nens,2)
X[,1] <- rnorm(nens,mean = 3, 1)
X[,2]<- rnorm(nens,mean = 5, 1)

#X[1:49,2] <- 0
intervalX <- matrix(c(0,0,10000,10000),2,2)

x.ind <- x.censored <- matrix(NA, ncol=ncol(X), nrow=nrow(X))

mu.f <- colMeans(X)
Pf <- cov(X)

J <- ncol(X)
N <- nrow(X)

for(j in 1:J){
  for(n in 1:N){
    x.ind[n,j] <- as.numeric(X[n,j] > 0)
    x.censored[n,j] <- as.numeric(ifelse(X[n,j] > intervalX[j,2], 0, X[n,j]))
  }
}

# samples has rows as iterations and columns as variables, you'll have
# to manually remove a burn-in period
# set up the "d" function for the distribution
ddirchmulti <- nimbleFunction(
  run = function(x = double(1), alpha = double(1), size = double(0), log = integer(0)){
    returnType(double(0))
    logProb <- lgamma(sum(alpha)) - sum(lgamma(alpha)) + sum(lgamma(alpha + x)) - lgamma(sum(alpha) + size)
    
    if(log) {
      return(logProb)
    } else {
      return(exp(logProb))
    }
    
  }
)

# set up the "r" function
rdirchmulti <- nimbleFunction(
  run = function(n = integer(0), alpha = double(1), size = double(0)) {
    returnType(double(1))
    if(n != 1) nimPrint("rdirchmulti only allows n = 1; using n = 1.")
    p <- rdirch(1, alpha)
    return(rmulti(1, size = size, prob = p))
  })

# tell NIMBLE about the newly available distribution
registerDistributions(list(ddirchmulti = list(BUGSdist = "ddirchmulti(alpha, size)", 
                                              types = c('value = double(1)', 'alpha = double(1)'))))

dwtmnorm <- nimbleFunction(
  run = function(x = double(1), mean = double(1), cov = double(2), wt = double(0), log = integer(0)){
    returnType(double(0))
    Prob <- dmnorm_chol(x, mean, chol(cov), prec_param = FALSE) * wt
    return(Prob)
  }
)
rwtmnorm <- nimbleFunction(
  run = function(n = integer(0), mean = double(1), cov = double(2), wt = double(0)){
    returnType(double(1))
    if(n != 1) nimPrint("rdirchmulti only allows n = 1; using n = 1.")
    Prob <- rmnorm_chol(n, mean, chol(cov), prec_param = FALSE) * wt
    return(Prob)
  }
)

dwtmnorm <- nimbleFunction(
  run = function(x = double(1), mean = double(1), cov = double(2), wt = double(0), log = integer(0)){
    returnType(double(0))
    n <- nrow(cov)
    logProb <- n * log(2 * pi) + log(det(cov)) + (x-mean) %*% inverse(cov) %*% t(x-mean) 
    logProb <- -1/2 * wt * Prob
    
    if(log) return(logProb) else return(exp(logProb))
  }
)
registerDistributions(list(dwtmnorm = list(BUGSdist = "dwtmnorm(mean, cov, wt)", 
                                              types = c('value = double(1)','mean = double(1)', 'cov = double(2)', 'wt = double(0)'))))


tobit2space.model <- nimbleCode({
  
  for(i in 1:N){
    #row[i] ~ dcat(wts[1:N])
    #s[i] ~ dcat(wts[1:N])
    #d[i] <- myCalculation(grid[1:N], s[i])
    y.censored[i,1:J] ~ dwtmnorm(mean = muf[1:J], cov = pf[1:J,1:J], wt = wts[i])
    for(j in 1:J){
      y.ind[i,j] ~ dinterval(y.censored[i,j], 0)
    }
  }
  
  #weighting the likelihood
  #row[i] ~ dcat(weights)
  #X[row[j],]
  
  muf[1:J] ~ dmnorm(mean = mu_0[1:J], cov = pf[1:J,1:J])
  
  Sigma[1:J,1:J] <- lambda_0[1:J,1:J]/nu_0
  pf[1:J,1:J] ~ dinvwish(S = Sigma[1:J,1:J], df = J)
  
})

x.ind <- x.censored <- matrix(NA, ncol=ncol(X), nrow=nrow(X))
for(j in seq_along(mu.f)){
  for(n in seq_len(nrow(X))){
    x.ind[n,j] <- as.numeric(X[n,j] > 0)
    x.censored[n,j] <- as.numeric(ifelse(X[n,j] > intervalX[j,2], 0, X[n,j])) #
  }
}


wts <- matrix(NA,nens,nens)
for(i in 1:nens){
  wts[i,] <- sample(size = nens,x=1:nens,replace = F)
}

wts <- rep(1,nens)

constants.tobit2space = list(N = nrow(X),
                             J = length(mu.f))

data.tobit2space = list(y.ind = x.ind,
                        y.censored = x.censored,
                        mu_0 = rep(0,length(mu.f)),
                        lambda_0 = diag(10,length(mu.f)),
                        nu_0 = 3,
                        wts = wts)#some measure of prior obs

inits.tobit2space = list(pf = Pf, muf = colMeans(X)) 
#set.seed(0)
#ptm <- proc.time()
tobit2space_pred <- nimbleModel(tobit2space.model, data = data.tobit2space,
                                constants = constants.tobit2space, inits = inits.tobit2space,
                                name = 'space')
## Adding X.mod,q,r as data for building model.
conf_tobit2space <- configureMCMC(tobit2space_pred, thin = 10, print=TRUE)
conf_tobit2space$addMonitors(c("pf", "muf","y.censored")) 
## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
## important!
## this is needed for correct indexing later
samplerNumberOffset_tobit2space <- length(conf_tobit2space$getSamplers())

for(j in seq_along(mu.f)){
  for(n in seq_len(nrow(X))){
    node <- paste0('y.censored[',n,',',j,']')
    conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW'))
    ## could instead use slice samplers, or any combination thereof, e.g.:
    ##conf$addSampler(node, 'toggle', control=list(type='slice'))
  }
}

#conf_tobit2space$printSamplers()

Rmcmc_tobit2space <- buildMCMC(conf_tobit2space)

Cmodel_tobit2space <- compileNimble(tobit2space_pred)

Cmcmc_tobit2space <- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)

for(i in seq_along(X)) {
  ## ironically, here we have to "toggle" the value of y.ind[i]
  ## this specifies that when y.ind[i] = 1,
  ## indicator variable is set to 0, which specifies *not* to sample
  valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
}

set.seed(0)
dat.tobit2space <- runMCMC(Cmcmc_tobit2space, niter = 50000, progressBar=TRUE)

#pdf(file.path(outdir,paste0('assessParams',t,'.pdf')))
assessParams(dat = dat.tobit2space[1000:5000,], Xt = X)
#dev.off()

## update parameters
dat.tobit2space  <- dat.tobit2space[1000:5000, ]
imuf   <- grep("muf", colnames(dat.tobit2space))
mu.f <- colMeans(dat.tobit2space[, imuf])
iPf   <- grep("pf", colnames(dat.tobit2space))
Pf <- matrix(colMeans(dat.tobit2space[, iPf]),ncol(X),ncol(X))

iycens <- grep("y.censored",colnames(dat.tobit2space))

# Why does cov(X.new) != Pf ?
X.new <- matrix(colMeans(dat.tobit2space[,iycens]),nrow(X),ncol(X))
#Pf <- cov(X.new)

nt = 50
m = c(1.03,.9)
model = matrix(0,nt,2) ; Y.dat = model ; y.ind = model ; y.censored = model
model[1,] = c(0,10)
q = diag(2) #process variance
R = r = diag(2)*2 #observation error #the lower you make this the better the convergence on the covariance matrix


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
bqq[1]     <- length(mu.f)
if(is.null(aqq)){
  aqq      <- array(0, dim = c(nt,ncol(X),ncol(X)))
}else{
  if(ncol(X)!=dim(aqq)[2]|ncol(X)!=dim(aqq)[3]){
    print('error: X has changed dimensions')
  }
}
aqq[1, , ] <- diag(length(mu.f)) * bqq[1] #Q

t = 1

constants.tobit = list(N = ncol(X), YN = length(y.ind[t,])) #doing y.ind[t,] because we only want one obs at a time. obs at t.
dimensions.tobit = list(X = length(mu.f), X.mod = ncol(X),
                        Q = c(length(mu.f),length(mu.f)))

data.tobit = list(muf = as.vector(mu.f),
                  pf = solve(Pf), 
                  aq = aqq[t,,], bq = bqq[t],
                  y.ind = y.ind[t,],
                  y.censored = y.censored[t,],
                  r = solve(R))
inits.pred = list(q = diag(length(mu.f)), X.mod = as.vector(mu.f),
                  X = rnorm(length(mu.f),0,1)) 

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

for(i in 1:length(y.ind[t,])) {
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

for(i in 1:length(y.ind[t,])) {
  ## ironically, here we have to "toggle" the value of y.ind[i]
  ## this specifies that when y.ind[i] = 1,
  ## indicator variable is set to 0, which specifies *not* to sample
  valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
}

set.seed(0)
dat <- runMCMC(Cmcmc, niter = 50000)

## update parameters
dat  <- dat[10000:50000, ]
iq   <- grep("q", colnames(dat))
iX   <- grep("X[", colnames(dat), fixed = TRUE)
mu.a <- colMeans(dat[, iX])
Pa   <- cov(dat[, iX])
Pa[is.na(Pa)] <- 0

CI.X1[, t] <- quantile(dat[, iX[1]], c(0.025, 0.5, 0.975))
CI.X2[, t] <- quantile(dat[, iX[2]], c(0.025, 0.5, 0.975))

mq <- dat[, iq]  # Omega, Precision
q.bar <- matrix(apply(mq, 2, mean), length(mu.f), length(mu.f))  # Mean Omega, Precision

col <- matrix(1:length(mu.f) ^ 2, length(mu.f), length(mu.f))
WV  <- matrix(0, length(mu.f), length(mu.f))
for (i in seq_along(mu.f)) {
  for (j in seq_along(mu.f)) {
    WV[i, j] <- wish.df(q.bar, X = mq, i = i, j = j, col = col[i, j])
  }
}

n <- mean(WV)
if (n < length(mu.f)) {
  n <- length(mu.f)
}
V <- solve(q.bar) * n

aqq[t + 1, , ]   <- V
bqq[t + 1]       <- n

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
