##' @title Analysis.sda
##' @name  Analysis.sda
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param FUN   A Function for performing the analysis step. Two available options are: 1-EnKF and 2-GEF.
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H is a mtrix of 1's and 0's specifying which observations go with which variables.
##' @param extraArg This argument is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list. See Details.
##' @param ... Extra argument sent to the analysis function. In case you're using the `GEF` function, this function requires nt, obs.mean, obs.cov, which are the total number of steps, list of observed means and list of observed cov respectively.
##’ @details
##’  
##' 
##' @description This functions uses the FUN to perform the analysis. EnKF function is developed inside the PEcAn.assim.sequential package which can be sent to this function to perform the Ensemble Kalman Filter. 
##' The other option is GEF function inside the same package allowing to perform Generalized Ensemble kalman Filter.
##' 
##' If you're using an arbitrary function you can use the ... to send any other variables to your desired analysis function.
##' 
##' @return Returns whatever the FUN is returning. In case of EnKF and GEF, this function returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export

Analysis.sda<-function(settings,
                       FUN,
                       Forecast=list(Pf=NULL,mu.f=NULL,Q=NULL,X=NULL),
                       Observed=list(R=NULL,Y=NULL),
                       H,
                       extraArg,
                       ...
){
  
  if (is.null(FUN)) PEcAn.logger::logger.severe('Analysis function needs to be defined !')
  FUN(settings, Forecast, Observed, H, extraArg ,...)
  
}

##' @title EnKF
##' @name  EnKF
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H is a mtrix of 1's and 0's specifying which observations go with which variables.
##' @param extraArg This argument is NOT used inside this function but it is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list. See Details.
##' @param ... Extra argument sent to the analysis function.
##’ @details
##’  
##' 
##' @description Given the Forecast and Observed this function performs the Ensemble Kalamn Filter. 
##' 
##' @return It returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export
EnKF<-function(setting, Forecast, Observed, H, extraArg=NULL, ...){
  
  #------------------------------Setup
  #-- reading the dots and exposing them to the inside of the function
  dots<-list(...)
  if (length(dots)>0) lapply(names(dots),function(name){assign(name,dots[[name]])})
  for(i in seq_along(dots)) assign(names(dots)[i],dots[[names(dots)[i]]])
  
  #Forecast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  #Observed inputs
  R <- Observed$R
  Y <- Observed$Y
  # Enkf---------------------------------------------------
  mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE))
  Pf <- cov(X)
  
  
  diag(Pf)[which(diag(Pf) == 0)] <- 0.1 ## hack for zero variance
  # for those elements with zero value
  if (length(Y) > 1) {
    
    PEcAn.logger::logger.info("The zero variances in R and Pf is being replaced by half and one fifth of the minimum variance in those matrices respectively.")
    diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
    diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5
  }
  
  ## process error
  if (!is.null(Q)) {
    Pf <- Pf + Q
  }
  
  ## Kalman Gain
  K <- Pf %*% t(H) %*% solve((R + H %*% Pf %*% t(H)))
  # Analysis
  mu.a <- mu.f + K %*% (Y - H %*% mu.f)
  Pa   <- (diag(ncol(X)) - K %*% H) %*% Pf
  return(list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa))
}

##' @title GEF
##' @name  GEF
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param extraArg This argument is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the process covariance and t gives the time in terms of index of obs.list. See Details.
##' @param nitr Number of iterations to run each MCMC chain.
##' @param nburnin 	Number of initial, pre-thinning, MCMC iterations to discard.
##' @param ... This function requires nt, obs.mean, obs.cov, which are the total number of steps, list of observed means and list of observed cov respectively.
##’ @details 
##’  
##' 
##' @description Given the Forecast and Observed this function performs the Generalized Ensemble Kalamn Filter. The generalized ensemble filter follows generally the three steps of sequential state data assimilation. But, in the generalized ensemble filter we add a latent state vector that accounts for added process variance. Furthermore, instead of solving the analysis analytically like the EnKF, we have to estimate the mean analysis vector and covariance matrix with MCMC.
##' 
##' @return It returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export
GEF<-function(settings, Forecast, Observed, H, extraArg, nitr=50000, nburnin=10000, ...){
  #------------------------------Setup
  #-- reading the dots and exposing them to the inside of the function
  dots<-list(...)
  if (length(dots)>0) lapply(names(dots),function(name){assign(name,dots[[name]], pos=1 )})
  #General
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  input.vars <- sapply(settings$state.data.assimilation$inputs, '[[', "variable.name")
  operators <- sapply(settings$state.data.assimilation$inputs, '[[', "operator")
  #Loading nimbles functions
  PEcAn.assim.sequential:::load_nimble()
  #load_nimble()
  #Forecast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  Pf = cov(X) # Cov Forecast - Goes into tobit2space as initial condition but is re-estimated in tobit space
  
  mu.f <- colMeans(X) #mean Forecast - This is used as an initial condition
  #Observed inputs
  R <- try(solve(Observed$R), silent = F) #putting solve() here so if not invertible error is before compiling tobit2space
  Y <- Observed$Y
  wish.df <- function(Om, X, i, j, col) {
    (Om[i, j]^2 + Om[i, i] * Om[j, j]) / var(X[, col])
  }
  #----------------------------------- GEF-----------------------------------------------------
  # Taking care of censored data ------------------------------    
  ### create matrix the describes the support for each observed state variable at time t
  path.to.models <- file.path(settings$outdir,"SDA","GEF")
  aqq <- extraArg$aqq
  bqq <- extraArg$bqq
  interval <- NULL
  t <- extraArg$t
  intervalX <- matrix(NA, ncol(X), 2)
  rownames(intervalX) <- colnames(X)
  outdir     <- settings$modeloutdir
  
  ###Snow no snow hack
  for(ii in 1:ncol(X)){
    try(if( sum(X[,ii],na.rm=T)==0 ) X[sample(x = 1:nrow(X),size = .2*nrow(X)),ii] <- .00000001)
  }
  
  ####getting ready to calculate y.ind and x.ind
  for(i in 1:length(var.names)){
    intervalX[which(startsWith(rownames(intervalX),
                               var.names[i])), ] <- matrix(c(as.numeric(settings$state.data.assimilation$state.variables[[i]]$min_value),
                                                             as.numeric(settings$state.data.assimilation$state.variables[[i]]$max_value)),
                                                           length(which(startsWith(rownames(intervalX),
                                                                                   var.names[i]))),2,byrow = TRUE)
  }
  
  #### These vectors are used to categorize data based on censoring from the interval matrix
  x.ind <- x.censored <- matrix(NA, ncol=ncol(X), nrow=nrow(X))
  for(j in seq_along(mu.f)){
    for(n in seq_len(nrow(X))){
      x.ind[n,j] <- as.numeric(X[n,j] > 0)
      x.censored[n,j] <- as.numeric(ifelse(X[n,j] > intervalX[j,2], 0, X[n,j])) #
    }
  }
  
  recompile <- extraArg$recompile
  #browser()
  ###TO DO: needs to recompile if there are new data sources
  if(t == 1 | recompile){
    #The purpose of this step is to impute data for mu.f 
    #where there are zero values so that 
    #mu.f is in 'tobit space' in the full model
    constants.tobit2space = list(N = nrow(X),
                                 J = length(mu.f))
    
    data.tobit2space = list(y.ind = x.ind,
                            y.censored = x.censored,
                            mu_0 = rep(0,length(mu.f)),
                            lambda_0 = diag(length(mu.f),length(mu.f)+1),
                            nu_0 = 3)#some measure of prior obs
    
    inits.tobit2space <<- list(pf = cov(X), muf = colMeans(X)) #pf = cov(X)
    #set.seed(0)
    #ptm <- proc.time()
    tobit2space_pred <<- nimbleModel(tobit2space.model, data = data.tobit2space,
                                     constants = constants.tobit2space, inits = inits.tobit2space,
                                     name = 'space')
    ## Adding X.mod,q,r as data for building model.
    conf_tobit2space <<- configureMCMC(tobit2space_pred, thin = 10, print=TRUE)
    conf_tobit2space$addMonitors(c("pf", "muf","y.censored")) 
    ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
    ## important!
    ## this is needed for correct indexing later
    samplerNumberOffset_tobit2space <<- length(conf_tobit2space$getSamplers())
    
    for(j in seq_along(mu.f)){
      for(n in seq_len(nrow(X))){
        node <- paste0('y.censored[',n,',',j,']')
        conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW'))
        ## could instead use slice samplers, or any combination thereof, e.g.:
        ##conf$addSampler(node, 'toggle', control=list(type='slice'))
      }
    }
    
    #conf_tobit2space$printSamplers()
    
    Rmcmc_tobit2space <<- buildMCMC(conf_tobit2space)
    
    Cmodel_tobit2space <<- compileNimble(tobit2space_pred)
    Cmcmc_tobit2space <<- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
    
    for(i in seq_along(X)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
    }
    
  }else{
    
    Cmodel_tobit2space$y.ind <- x.ind
    Cmodel_tobit2space$y.censored <- x.censored
    
    inits.tobit2space = list(pf = Pf, muf = colMeans(X))
    Cmodel_tobit2space$setInits(inits.tobit2space)
    
    for(i in seq_along(X)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
    }
    
  }
  
  if(file.exists(file.path(outdir, paste0('dat.tobit2space',t,'.Rdata')))){
    load(file.path(outdir, paste0('dat.tobit2space',t,'.Rdata')))
  }else{
    dat.tobit2space <- runMCMC(Cmcmc_tobit2space, niter = nitr, nburnin=nburnin,  progressBar=TRUE)
    save(dat.tobit2space, file = file.path(outdir, paste0('dat.tobit2space',t,'.Rdata')))
  }
  
  pdf(file.path(outdir,paste0('assessParams',t,'.pdf')))
  try(assessParams(dat = dat.tobit2space[1000:4000,], Xt = X))
  dev.off()
  
  ## TO DO Add MCMC Diagnostics, how do we do it for pecan meta-analysis?
  
  ## update parameters
  #dat.tobit2space  <- dat.tobit2space[1000:5000, ]
  imuf   <- grep("muf", colnames(dat.tobit2space))
  mu.f <- colMeans(dat.tobit2space[, imuf])
  iPf   <- grep("pf", colnames(dat.tobit2space))
  Pf <- matrix(colMeans(dat.tobit2space[, iPf]),ncol(X),ncol(X))
  #--- This is where the localization needs to happen - After imputing Pf
  
  iycens <- grep("y.censored",colnames(dat.tobit2space))
  X.new <- matrix(colMeans(dat.tobit2space[,iycens]),nrow(X),ncol(X))
  
  # if(sum(diag(Pf)-diag(cov(X))) > 10 | sum(diag(Pf)-diag(cov(X))) < -10) logger.severe('Increase Sample Size')
  
  ###-------------------------------------------------------------------###
  # Generalized Ensemble Filter                                       ###-----
  ###-------------------------------------------------------------------###
  
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
  
  ### create matrix the describes the support for each observed state variable at time t
  interval <- matrix(NA, length(obs.mean[[t]]), 2)
  rownames(interval) <- names(obs.mean[[t]])
  for(i in 1:length(input.vars)){
    interval[grep(x=rownames(interval),
                  pattern=input.vars[i]), ] <- matrix(c(as.numeric(settings$state.data.assimilation$inputs[[i]]$min_value),
                                                        as.numeric(settings$state.data.assimilation$inputs[[i]]$max_value)),
                                                      length(grep(x=rownames(interval),pattern=input.vars[i])),2,byrow = TRUE)
  }
  #### These vectors are used to categorize data based on censoring 
  #### from the interval matrix
  y.ind <- as.numeric(Y > interval[,1])
  y.censored <- as.numeric(ifelse(Y > interval[,1], Y, 0))
  
  #which type of observation do we have at this time point?
  input.order <- lapply(input.vars, grep, x=names(obs.mean[[t]])) # not going to work if AbvGrnWood is given in two different ways like tree rings and refab
  names(input.order) <- operators
  data_available <- unlist(input.order)
  
  #browser()
  
  if(any(grep(names(data_available),pattern = 'direct'))){
    which_direct <- data_available[grep(names(data_available),pattern = 'direct')]
    X_direct_start <- which_direct[1]
    X_direct_end <- which_direct[length(which_direct)]
    direct_TRUE = TRUE
  }else{
    X_direct_start <- 0
    X_direct_end <- 0
    direct_TRUE = FALSE
  }
  
  if(any(grep(names(data_available),pattern = 'ALR'))){
    
    # browser()
    which_fcomp <- grep(names(data_available),pattern = 'ALR')
    X_fcomp_start <- which_fcomp[1]
    X_fcomp_end <- which_fcomp[length(which_fcomp)]
    X_fcomp_model <- grep(colnames(X),pattern = 'AGB.pft')
  
    fcomp_TRUE = TRUE
  }else{
    X_fcomp_start <- 0
    X_fcomp_end <- 0
    X_fcomp_model <- 0
    fcomp_TRUE = FALSE
  }
  
  if(any(grep(names(data_available),pattern = 'pft2total'))){
    which_pft2total <- grep(names(data_available),pattern = 'pft2total')
    X_pft2total_start <- which_pft2total[1]
    X_pft2total_end <- which_pft2total[length(which_pft2total)]
    X_pft2total_model <- grep(colnames(X),pattern = 'AbvGrndWood')
    pft2total_TRUE = TRUE
  }else{
    X_pft2total_start <- 0
    X_pft2total_end <- 0
    X_pft2total_model <- 0
    pft2total_TRUE = FALSE
  }
  
  
  if(t > 1 & recompile != TRUE){
    if(X_direct_start != constants.tobit$X_direct_start) recompile = TRUE
    if(X_direct_end != constants.tobit$X_direct_end) recompile = TRUE
    if(X_fcomp_end != constants.tobit$X_fcomp_end) recompile = TRUE
    if(X_fcomp_start != constants.tobit$X_fcomp_start) recompile = TRUE
  }
  
  if(t == 1 | recompile){ 
    constants.tobit <<- list(
      N = ncol(X),
      YN = length(y.ind),
      X_direct_start = X_direct_start,
      X_direct_end = X_direct_end,
      X_fcomp_start = X_fcomp_start,
      X_fcomp_end = X_fcomp_end,
      X_fcomp_model_start = X_fcomp_model[1],
      X_fcomp_model_end = X_fcomp_model[length(X_fcomp_model)],
      X_pft2total_start = X_pft2total_start,
      X_pft2total_model_start = X_pft2total_model[1],
      X_pft2total_model_end = X_pft2total_model[length(X_pft2total_model)],
      direct_TRUE = direct_TRUE,
      fcomp_TRUE = fcomp_TRUE,
      pft2total_TRUE = pft2total_TRUE
    )
    
    dimensions.tobit <<- list(X = length(mu.f), X.mod = ncol(X),
                            Q = c(length(mu.f),length(mu.f)),
                            y_star = (length(y.censored)))
    
    data.tobit <<- list(muf = as.vector(mu.f),
                      pf = solve(Pf), 
                      aq = aqq[t,,], bq = bqq[t],
                      y.ind = y.ind,
                      y.censored = y.censored,
                      r = R) #precision
    
    inits.pred <<- list(q = diag(length(mu.f))*(length(mu.f)+1),
                      X.mod = rnorm(length(mu.f),mu.f,10),
                      X = rnorm(length(mu.f),2.5,10),
                      y_star = rnorm(length(y.censored),0,10))

model_pred <- nimbleModel(tobit.model, data = data.tobit, dimensions = dimensions.tobit,
                          constants = constants.tobit, inits = inits.pred,
                          name = 'base')

model_pred$initializeInfo()
## Adding X.mod,q,r as data for building model.
conf <- configureMCMC(model_pred, print=TRUE)
conf$addMonitors(c("X","X.mod","q","Q", "y_star","y.censored")) 
## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]


if(FALSE){ ### Need this for when the state variables are on different scales like NPP and AGB
  x.char <- paste0('X[1:',ncol(X),']')
  conf$removeSampler(x.char)
  propCov.means <- c(rep(1,9),1000)#signif(diag(Pf),1)#mean(unlist(lapply(obs.cov,FUN = function(x){diag(x)})))[choose]#c(rep(max(diag(Pf)),ncol(X)))#
  if(length(propCov.means)!=ncol(X)) propCov.means <- c(propCov.means,rep(1,ncol(X)-length(Y)))
  conf$addSampler(target =c(x.char),
                  control <- list(propCov = diag(ncol(X))*propCov.means),
                  type='RW_block')
}

## important!
## this is needed for correct indexing later
samplerNumberOffset <<- length(conf$getSamplers())

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
  ## ironically, here we have to "toggle" the value of y.ind[i]
  ## this specifies that when y.ind[i] = 1,
  ## indicator variable is set to 0, which specifies *not* to sample
  valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
}

  }else{
    Cmodel$y.ind <- y.ind
    Cmodel$y.censored <- y.censored
    Cmodel$aq <- aqq[t,,]
    Cmodel$bq <- bqq[t]
    Cmodel$muf <- mu.f
    Cmodel$pf <- solve(Pf)
    Cmodel$r <- (R) #precision
    
    inits.pred = list(q = diag(length(mu.f))*(length(mu.f)+1),
                      X.mod = rnorm(length(mu.f),2.5,10),
                      X = rnorm(ncol(X),mu.f,10),
                      y_star = rnorm(length(y.censored),mu.f,10)) #
    
    Cmodel$setInits(inits.pred)
    
    for(i in 1:length(y.ind)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
    }
    
  }
  
  dat <- runMCMC(Cmcmc, niter = 100000, nburnin=20000)
  dat_save <- dat[(nrow(dat)-250):nrow(dat),]
  save(dat_save, file = file.path(outdir, paste0('dat',t,'.Rdata')))
  
  ## update parameters
  iq   <- grep("q", colnames(dat))
  iX   <- grep("X[", colnames(dat), fixed = TRUE)
  iystar   <- grep("y_star", colnames(dat), fixed = TRUE)
  iX.mod <- grep("X.mod", colnames(dat), fixed = TRUE)
  
  mu.a <- colMeans(dat[, iX])
  Pa   <- cov(dat[, iX])
  Pa[is.na(Pa)] <- 0
  
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
  
  if (t<nt){
    aqq[t + 1, , ]   <- V
    bqq[t + 1]       <- n
  }
  
  pdf(file.path(outdir, paste0('dat_plot', t, '.pdf')))
  par(mfrow = c(2, 2))
  for (rr in 1:length(iX)) {
    plot(dat[, iX[rr]], typ = 'l')
  }
  for (i in 1:length(iystar)) {
    plot(dat[,iystar[i]], type = 'l')
    abline(h=alr(mu.a)[i],col='red')
  }
  for (i in 1:4) {
    plot(dat[,iX.mod[i]], type = 'l')
    abline(h=mu.f[i],col='red')
  }
  dev.off()
  
  return(list(mu.f = mu.f,
              Pf = Pf,
              mu.a = mu.a,
              Pa = Pa,
              q.bar = q.bar,
              n = n,
              X.new=X.new,
              aqq=aqq,
              bqq=bqq,
              y.censored=y.censored,
              R=R
  )
  )
}



##' @title Construc_H
##' @name  Construc_H
##' @author Hamze Dokoohaki
##' 
##' @param choose  a vector of observations indices oredered based on their appearances in the list of state variable names.
##' @param Y vector of observations
##' @param X Dataframe or matrix of forecast state variables for different ensembles.
##’ @details
##’  
##' 
##' @description This function creates a mtrix mapping obsereved data to their forecast state variable.
##' 
##' @return This returns a mtrix specifying which observation go with which state variables.
##' @export
Construct_H <- function(choose, Y, X){
  ## design matrix
  H <- matrix(0, length(Y), ncol(X)) #H maps true state to observed data
  #linear
  for (i in choose) {
    H[i, i] <- 1
  }
  
  return(H)
}
