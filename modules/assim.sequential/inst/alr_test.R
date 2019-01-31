library(nimble)
library(mvtnorm)

sampler_toggle <<- nimbleFunction(
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

alr <<-  nimbleFunction(
  run = function(y = double(1)) {
    returnType(double(1))
    
    y[y<0] <- .000000001
    y_alr <- log(y[1:(length(y)-1)] / y[length(y)])
    
    return(y_alr)
  })

inv.alr <<-  nimbleFunction(
  run = function(alr = double(1)) {
    returnType(double(1))
    
    y = exp(c(alr, 0)) / sum(exp(c(alr, 0)))
    
    return(y)
  })

tobit.model <<- nimbleCode({ 
  
  q[1:N,1:N]  ~ dwish(R = aq[1:N,1:N], df = bq) ## aq and bq are estimated over time
  Q[1:N,1:N] <- inverse(q[1:N,1:N])
  X.mod[1:N] ~ dmnorm(muf[1:N], prec = pf[1:N,1:N]) ## Model Forecast ##muf and pf are assigned from ensembles
  
  ## add process error
  X[1:N]  ~ dmnorm(X.mod[1:N], prec = q[1:N,1:N])

  y_star[X_fcomp_start:X_fcomp_end] <- alr(X[X_fcomp_model_start:X_fcomp_model_end])
  
  y.censored[X_fcomp_start:X_fcomp_end] ~ dmnorm(y_star[X_fcomp_start:X_fcomp_end],
                                                 prec = r[X_fcomp_start:X_fcomp_end, 
                                                          X_fcomp_start:X_fcomp_end])
  for(i in 1:YN){
    y.ind[i] ~ dinterval(y.censored[i], 0)
  }
  
})

library(nimble)

n.state <- 4
nens <- 100000
sig <- diag(n.state) * c(500,600,50,200)
#sig[1,2] <- sig[2,1] <- 420
#sig[1,3] <- sig[3,1] <- 100
#sig[1,4] <- sig[4,1] <- 240

#sig[2,4] <- sig[4,2] <- 170
#sig[3,4] <- sig[4,3] <- 710
mu_X <- c(4000, 6000, 3000, 7000)
mu_y <- c(0.2, 0.1, 0.4, 0.3)
X <- rmvnorm(n = nens,rnorm(4,mu_X,1000),sigma = sig)
if(any(X<0)) stop()
mu.f <- colMeans(X)
Pf <- cov(X) 

set.seed(0)
take <- sample(x = 1:nens,size = 100)
x.prop.table <- prop.table(X,margin = 1)

alr.x <- t(apply(x.prop.table,1,alr))

# y.censored <- colMeans(alr.x[take,])
y.censored <- alr(mu_y)
y.ind <- rep(1,n.state-1)

library(MCMCpack)
#R <- rwish(20, diag(3)*100)
R <- solve(cov(alr.x[take,]))

X_direct_start <- X_direct_end <- X_pft2total_start <- X_pft2total_end <- X_pft2total_model <- 0
X_fcomp_start <- 1
X_fcomp_end <- 3
X_fcomp_model <- 1:4

alr_last <- 4

direct_TRUE <- pft2total_TRUE <- FALSE
fcomp_TRUE <- TRUE

constants.tobit <<- list(N = ncol(X), YN = length(y.ind),
                         X_fcomp_start = X_fcomp_start, X_fcomp_end = X_fcomp_end,
                         X_fcomp_model_start=X_fcomp_model[1],
                         X_fcomp_model_end=X_fcomp_model[length(X_fcomp_model)],
                         alr_last = alr_last)

dimensions.tobit = list(X = length(mu.f), X.mod = ncol(X),
                        Q = c(length(mu.f),length(mu.f)),
                        y_star = length(y.censored))

data.tobit = list(muf = as.vector(mu.f),
                  pf = solve(Pf), 
                  aq = diag(n.state+1)*(n.state+1), 
                  bq = (n.state+1),
                  y.ind = rep(1,n.state),
                  r = (R),
                  y.censored = y.censored) #precision

inits.pred = list(q = diag(length(mu.f))*(n.state + 1),
                  X.mod = rnorm(length(mu.f),mu.f,100),
                  X = rnorm(length(mu.f),mu.f,100), 
                  y_star = rnorm(n.state-1,0,1)) #

save(constants.tobit,dimensions.tobit,data.tobit,inits.pred, file='use_this.Rdata')

if(!exists('Cmcmc')){
  model_pred <- nimbleModel(tobit.model, data = data.tobit, 
                            dimensions = dimensions.tobit,
                            constants = constants.tobit,
                            inits = inits.pred,
                            name = 'pred')
  ## Adding X.mod,q,r as data for building model.
  conf <- configureMCMC(model_pred, print=TRUE)
  
  conf$addMonitors(c("X","q","Q", "y_star","y.censored"))
  ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
  ## important!
  ## this is needed for correct indexing later
  x.char <- paste0('X[1:',ncol(X),']')
  
  #### setting custom proposal distribution
  # conf$removeSamplers(x.char)
  # propCov.means <- signif(diag(solve(Pf)),1)#mean(unlist(lapply(obs.cov,FUN = function(x){diag(x)})))[choose]#c(rep(max(diag(Pf)),ncol(X)))#
  # if(length(propCov.means)!=ncol(X)) propCov.means <- c(propCov.means,rep(1,ncol(X)-length(Y)))
  # conf$addSampler(target =c(x.char),
  #                 control <- list(propCov = diag(ncol(X))*propCov.means),
  #                 type='RW_block')
  
  samplerNumberOffset <<- length(conf$getSamplers())
  
  ## sampler needs to be disabled
  for(i in 1:length(y.ind)) {
    node <- paste0('y.censored[',i,']')
    conf$addSampler(node, 'toggle', control=list(type='RW'))
    ## could instead use slice samplers, or any combination thereof, e.g.:
    ##conf$addSampler(node, 'toggle', control=list(type='slice'))
  }
  
  conf$printSamplers()
  
  ## can monitor y.censored, if you wish, to verify correct behaviour
  #conf$addMonitors('y.censored')
  
  Rmcmc <<- buildMCMC(conf)
  
  Cmodel <<- compileNimble(model_pred)
  Cmcmc <<- compileNimble(Rmcmc, project = model_pred)
  
  for(i in 1:length(y.ind)) {
    # ironically, here we have to "toggle" the value of y.ind[i]
    # this specifies that when y.ind[i] = 1,
    # indicator variable is set to 0, which specifies *not* to sample
    valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
  }
}

diff.comp <- c(900,100,10,1)

Cmodel$y.censored <- alr(diff.comp)#test different composition data
Cmodel$muf <- mu.f
Cmodel$pf <- solve(Pf)
Cmodel$r <- rwish(20, diag(3)*1000) #precision

inits.pred = list(q = diag(length(mu.f))*(length(mu.f)+1),
                  X.mod = rnorm(length(mu.f),rep(100, 4),100),
                  X = rnorm(length(mu.f),mu.f,100), 
                  y_star = rnorm(n.state-1,0,1)) #
# X.mod = rnorm(length(mu.f),mu.f,100),
#                   X = rnorm(length(mu.f),mu.f,100)) #
Cmodel$setInits(inits.pred)
for(i in 1:length(y.ind)) {
  # ironically, here we have to "toggle" the value of y.ind[i]
  # this specifies that when y.ind[i] = 1,
  # indicator variable is set to 0, which specifies *not* to sample
  valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
}


dat <- runMCMC(Cmcmc, niter = 50000, nburnin=10000) #need to make sure you run for awhile to avoid autocorrelation problems

plot(dat[,grep('y.censored',colnames(dat))[3]]) #should be a line

## update parameters
iystar <- grep("y_star", colnames(dat), fixed = TRUE)
iq   <- grep("q", colnames(dat))
iQ   <- grep("Q", colnames(dat))
iX   <- grep("X[", colnames(dat), fixed = TRUE)
iX.mod   <- grep("X.mod", colnames(dat), fixed = TRUE)

X.a <- colMeans(dat[, iX])
Xmod.a <- colMeans(dat[, iX.mod])
q.a <- matrix(colMeans(dat[, iq]), 4, 4)
mu.a <- colMeans(dat[burnin:nrow(dat), iX])
ystar.a <- colMeans(dat[, iystar])
Pa   <- cov(dat[, iX])
Pa[is.na(Pa)] <- 0

burnin <- .2*nrow(dat)

rbind(mu.a/sum(mu.a),mu.f/sum(mu.f))
layout(matrix(1:4, 2, 2))
for (i in 10:7) {
  plot(dat[burnin:nrow(dat),iX[i]], type = 'l')
}


rbind(ystar.a,y.censored)
layout(matrix(1:4, 2, 2))
for (i in 1:3) {
  plot(dat[,iystar[i]], type = 'l')
  abline(h=alr(mu.a)[i],col='red')
}

layout(matrix(1:4, 2, 2))
for (i in 1:4) {
  plot(dat[,iX.mod[i]], type = 'l')
  abline(h=mu.f[i],col='red')
}

# layout(matrix(1:16, 4, 4))
# for (i in 1:16) {
#   plot(dat[,iq[i]], type = 'l')
# }
# 
# 
# layout(matrix(1:16, 4, 4))
# for (i in 1:16) {
#   plot(dat[,iQ[i]], type = 'l')
# }
