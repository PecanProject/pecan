##' @title Analysis.sda
##' @name  Analysis.sda
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param FUN   A Function for performing the analysis step. Two available options are: 1-EnKF and 2-GEF.
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H is a matrix of 1's and 0's specifying which observations go with which variables.
##' @param extraArg This argument is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list. See Details.
##' @param ... Extra argument sent to the analysis function. In case you're using the `GEF` function, this function requires nt, obs.mean, obs.cov, which are the total number of steps, list of observed means and list of observed cov respectively.
##’ @details
##’  
##' 
##' @description This functions uses the FUN to perform the analysis. EnKF function is developed inside the PEcAnAssimSequential package which can be sent to this function to perform the Ensemble Kalman Filter. 
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
##' @param H is a matrix of 1's and 0's specifying which observations go with which variables.
##' @param extraArg This argument is NOT used inside this function but it is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list. See Details.
##' @param ... Extra argument sent to the analysis function.
##’ @details
##’  
##' 
##' @description Given the Forecast and Observed this function performs the Ensemble Kalamn Filter. 
##' 
##' @return It returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export
EnKF<-function(settings, Forecast, Observed, H, extraArg=NULL, ...){
  
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
  Pf <- stats::cov(X)
  
  
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
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecast state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H not used
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
  #PEcAnAssimSequential::load_nimble()
  
  #Forecast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  Pf = stats::cov(X) # Cov Forecast - Goes into tobit2space as initial condition but is re-estimated in tobit space
  mu.f <- colMeans(X) #mean Forecast - This is used as an initial condition
  
  diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5 #fixing det(Pf)==0
  
  #Observed inputs
  R <- try(solve(Observed$R), silent = F) #putting solve() here so if not invertible error is before compiling tobit2space #sfsmisc::posdefify(
  Y <- Observed$Y
  wish.df <- function(Om, X, i, j, col) {
    (Om[i, j]^2 + Om[i, i] * Om[j, j]) / stats::var(X[, col])
  }
  #----------------------------------- GEF-----------------------------------------------------
  # Taking care of censored data ------------------------------    
  ### create matrix the describes the support for each observed state variable at time t
  path.to.models <- file.path(settings$outdir,"SDA","GEF")
  aqq <- extraArg$aqq
  bqq <- extraArg$bqq
  wts <- extraArg$wts/sum(extraArg$wts)
  
  if(any(is.na(wts))){
    PEcAn.logger::logger.warn(paste('We found an NA in the wts for the ensemble members. Is this what you want? For now, we will change the NA to a zero.'))
    wts[is.na(wts)] <- 0
  }
  if(sum(wts==0)){
    wts <- rep(1,nrow(X))/nrow(X)
  }
  interval <- NULL
  t <- extraArg$t
  intervalX <- matrix(NA, ncol(X), 2)
  rownames(intervalX) <- colnames(X)
  outdir     <- settings$modeloutdir
  
  ###Snow no snow hack
  for(ii in 1:ncol(X)){
    try(if( sum(X[,ii],na.rm=T)==0 ) X[sample(x = 1:nrow(X),size = .2*nrow(X)),ii] <- .001)
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
    
    recompileTobit <- extraArg$recompileTobit
    recompileGEF <- extraArg$recompileGEF
    
    ###TO DO: needs to recompile if there are new data sources
    if(TRUE){ #any(X==0,na.rm=T)
      #
    if(t == 1 | recompileTobit){
      #The purpose of this step is to impute data for mu.f 
      #where there are zero values so that 
      #mu.f is in 'tobit space' in the full model
      constants.tobit2space <- list(N = nrow(X),
                                     J = length(mu.f))
      
      data.tobit2space <- list(y.ind = x.ind,
                                y.censored = x.censored,
                                mu_0 = rep(0,length(mu.f)),
                                lambda_0 = diag(1000,length(mu.f)), #can try solve
                                nu_0 = ncol(X)+1,
                                wts = wts*nrow(X), #sigma x2 max Y
                                Sigma_0 = solve(diag(1000,length(mu.f))))#some measure of prior obs
    
      inits.tobit2space <- function() list(muf = rmnorm_chol(1,colMeans(X),
                                                             chol(diag(ncol(X))*100)),
                                           pf = rwish_chol(1,df = ncol(X)+1,
                                                           cholesky = chol(solve(Pf))))
      #ptm <- proc.time()
      
      tobit2space_pred <- nimbleModel(tobit2space.model, data = data.tobit2space,
                                       constants = constants.tobit2space, inits = inits.tobit2space(),
                                       name = 'space')
      
      try(logprob_y_tobit2space <- tobit2space_pred$calculate('y.censored'))
      if(is.na(logprob_y_tobit2space)) PEcAn.logger::logger.warn('We cannot calculate a logprobability for your data in the tobit2space model. Check data.tobit2space variable in the global environment to make sure its what you want.')
      if(logprob_y_tobit2space < -1000000) PEcAn.logger::logger.warn(paste('Log probability very low for y in tobit2space model during time',t,'. Check initial conditions.'))
      
      ## Adding X.mod,q,r as data for building model.
      conf_tobit2space <- configureMCMC(tobit2space_pred, thin = 10, print=TRUE)
      conf_tobit2space$addMonitors(c("pf", "muf","y.censored"))
      
      conf_tobit2space$removeSampler('pf')
      conf_tobit2space$addSampler('pf','conj_wt_wishart_sampler')
      
      samplerNumberOffset_tobit2space <- length(conf_tobit2space$getSamplers())
      
      for(j in seq_along(mu.f)){
        for(n in seq_len(nrow(X))){
          node <- paste0('y.censored[',n,',',j,']')
          conf_tobit2space$addSampler(node, 'toggle', control=list(type='RW'))
          ## could instead use slice samplers, or any combination thereof, e.g.:
          ##conf$addSampler(node, 'toggle', control=list(type='slice'))
        }
      }
      
      Rmcmc_tobit2space <- buildMCMC(conf_tobit2space)
      
      #restarting at good initial conditions is somewhat important here
      Cmodel_tobit2space <- compileNimble(tobit2space_pred)
      Cmcmc_tobit2space <- compileNimble(Rmcmc_tobit2space, project = tobit2space_pred)
      
      for(i in seq_along(X)) {
        ## ironically, here we have to "toggle" the value of y.ind[i]
        ## this specifies that when y.ind[i] = 1,
        ## indicator variable is set to 0, which specifies *not* to sample
        valueInCompiledNimbleFunction(Cmcmc_tobit2space$samplerFunctions[[samplerNumberOffset_tobit2space+i]], 'toggle', 1-x.ind[i])
      }
      
    }else{
      
      Cmodel_tobit2space$wts <- wts*nrow(X)
      
      Cmodel_tobit2space$y.ind <- x.ind
      Cmodel_tobit2space$y.censored <- x.censored
      
      inits.tobit2space <- function() list(muf = rmnorm_chol(1,colMeans(X),chol(diag(ncol(X))*100)),
                                           pf = rwish_chol(1,df = ncol(X)+1,cholesky = chol(solve(stats::cov(X)))))
      
      Cmodel_tobit2space$setInits(inits.tobit2space())
      
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
      
      dat.tobit2space.nchains <-
        runMCMC(
          Cmcmc_tobit2space,
          niter = nitr,
          nburnin = nburnin,
          nchains = 3,
          inits = inits.tobit2space(),
          progressBar = TRUE
        )
      dat.tobit2space <- do.call(rbind,dat.tobit2space.nchains)
      
      which_f <- grep(c("f"), colnames(dat.tobit2space.nchains[[1]]))
      gelman.keep.tobit2space <- numeric(length(which_f))
      for(ff in seq_along(which_f)){
        mcmc.check <- list()
        
        mcmc.check[[1]] <- coda::mcmc(dat.tobit2space.nchains[[1]][,which_f[ff]])
        mcmc.check[[2]] <- coda::mcmc(dat.tobit2space.nchains[[2]][,which_f[ff]])
        mcmc.check[[3]] <- coda::mcmc(dat.tobit2space.nchains[[3]][,which_f[ff]])
        
        
        gelman.keep.tobit2space[ff] <- try(coda::gelman.diag(mcmc.check,transform = T)$psrf[1])
      }
      print(gelman.keep.tobit2space)
      
      if(any(gelman.keep.tobit2space > 1.5)) PEcAn.logger::logger.warn(paste('Gelman value > 1.5 for tobit2space model. Re-assess time point', t))

      save(dat.tobit2space, file = file.path(outdir, paste0('dat.tobit2space',t,'.Rdata')))
    }
    
    grDevices::pdf(file.path(outdir,paste0('assessParams',t,'.pdf')))
    set.seed(t)
    try(assessParams(dat = dat.tobit2space[sample(x = 10:nrow(dat.tobit2space),size = 500,replace = F),],wts=wts, Xt = X))
    grDevices::dev.off()
    
    ## TO DO Add MCMC Diagnostics, how do we do it for pecan meta-analysis?
    
    ## update parameters
    #dat.tobit2space  <- dat.tobit2space[1000:5000, ]
    imuf   <- grep("muf", colnames(dat.tobit2space))
    mu.f <- colMeans(dat.tobit2space[, imuf])
    iPf   <- grep("pf", colnames(dat.tobit2space))
    Pf <- solve(matrix(colMeans(dat.tobit2space[, iPf]),ncol(X),ncol(X)))
    #--- This is where the localization needs to happen - After imputing Pf
    
    iycens <- grep("y.censored",colnames(dat.tobit2space))
    X.new <- matrix(colMeans(dat.tobit2space[,iycens]),nrow(X),ncol(X))
  }else{
    ## IDEA not sure if it's a good one
    mu.f <- apply(X,2,weighted.mean,wts)
    Pf <- stats::cov.wt(X,wts)$cov
    X.new <- X
    gelman.keep.tobit2space <- NA
  }

  ###-------------------------------------------------------------------###
  # Generalized Ensemble Filter                                       ###-----
  ###-------------------------------------------------------------------###
  #### initial conditions
  
  if(length(aqq)==0){
    aqq      <- list() #array(0, dim = c(nt,ncol(X),ncol(X)))
  }else{
    if(ncol(X)!=dim(aqq)[1]|ncol(X)!=dim(aqq)[2]){
      print('error: X has changed dimensions')
    }
  }
  if(t == 1){
    bqq    <- length(mu.f)
    aqq <- diag(length(mu.f)) * bqq #Q
  }
  
  ### create matrix the describes the support for each observed state variable at time t
  interval <- matrix(NA, length(obs.mean[[t]]), 2)
  # Each observe variable needs to have its own file tag under inputs
  interval <-settings$state.data.assimilation$inputs %>%
    purrr::map_dfr( ~ data.frame(
      .x$'min_value' %>% as.numeric(),.x$'max_value' %>% as.numeric()
    )) %>%
    as.matrix()
  
  rownames(interval) <- names(input.vars)
  
  #### These vectors are used to categorize data based on censoring 
  #### from the interval matrix
  y.ind <- as.numeric(Y > interval[,1])
  y.censored <- as.numeric(ifelse(Y > interval[,1], Y, 0))
  
  if(sum(y.censored,na.rm=T)==0){
    PEcAn.logger::logger.warn('NO DATA. Check y.censored in Analysis_sda.R')
  }
  
  #which type of observation do we have at this time point?
  input.order <- lapply(input.vars, grep, x=names(obs.mean[[t]][[1]])) # not going to work if AbvGrnWood is given in two different ways like tree rings and refab
  names(input.order) <- operators
  data_available <- unlist(input.order)
  
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
    
    which_fcomp <- grep(names(data_available),pattern = 'ALR')
    X_fcomp_start <- which_fcomp[1]
    X_fcomp_end <- which_fcomp[length(which_fcomp)]
    X_fcomp_model <- grep(colnames(X),pattern = '*.pft.*')
    
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
    X_pft2total_model <- grep(colnames(X),pattern = '*.pft.*')
    pft2total_TRUE = TRUE
  }else{
    X_pft2total_start <- 0
    X_pft2total_end <- 0
    X_pft2total_model <- 0
    pft2total_TRUE = FALSE
  }
  
  
  if(t > 1 & recompileGEF != TRUE){
    if(X_direct_start != constants.tobit$X_direct_start) recompileGEF = TRUE
    if(X_direct_end != constants.tobit$X_direct_end) recompileGEF = TRUE
    if(X_fcomp_end != constants.tobit$X_fcomp_end) recompileGEF = TRUE
    if(X_fcomp_start != constants.tobit$X_fcomp_start) recompileGEF = TRUE
  }
  
  if(t == 1 | recompileGEF){ 
    constants.tobit <- list(
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
    
    dimensions.tobit <- list(X = length(mu.f), X.mod = ncol(X),
                              Q = c(length(mu.f),length(mu.f)),
                              y_star = (length(y.censored)))
    
    data.tobit <- list(muf = as.vector(mu.f),
                        pf = solve(Pf), 
                        aq = aqq, bq = bqq,
                        y.ind = y.ind,
                        y.censored = y.censored,
                        r = R) #precision
    
    inits.pred <-  function(){
      list(
          q = diag(ncol(X)) * stats::runif(1, length(mu.f), length(mu.f) + 1),
          X.mod = stats::rnorm(length(mu.f), mu.f, 1),
          X = stats::rnorm(length(mu.f), mu.f, .1),
          y_star = stats::rnorm(length(y.censored), 0, 1)
      )
      }
    
    # 
    # inits.pred <- list(q = diag(length(mu.f))*(length(mu.f)+1),
    #                   X.mod = rnorm(length(mu.f),mu.f,1),
    #                   X = rnorm(length(mu.f),mu.f,1),
    #                   y_star = rnorm(length(y.censored),0,1))
    
    model_pred <- nimbleModel(tobit.model, data = data.tobit, dimensions = dimensions.tobit,
                              constants = constants.tobit, inits = inits.pred(),
                              name = 'base')
    
    model_pred$initializeInfo()
    ## Adding X.mod,q,r as data for building model.
    conf <- configureMCMC(model_pred, print=TRUE,thin = 10)
    conf$addMonitors(c("X","X.mod","q","Q", "y_star","y.censored")) 
    
    if(ncol(X) > length(y.censored)){
      print('Adding a different sampler to X nodes to improve MCMC mixing')
      conf$removeSampler('X')
      #consider individual slice samplers
      #conf$addSampler(paste0('X[1:12]'),'AF_slice')
      for(ss in 1:ncol(X)){
        conf$addSampler(paste0('X[',ss,']'),'slice')
      }
      conf$printSamplers()
    }
    
    if(FALSE){ ### Need this for when the state variables are on different scales like NPP and AGB
      x.char <- paste0('X[1:',ncol(X),']')
      conf$removeSampler(x.char)
      propCov.means <- c(rep(1,ncol(X)),1000)#signif(diag(Pf),1)#mean(unlist(lapply(obs.cov,FUN = function(x){diag(x)})))[choose]#c(rep(max(diag(Pf)),ncol(X)))#
      if(length(propCov.means)!=ncol(X)) propCov.means <- c(propCov.means,rep(1,ncol(X)-length(Y)))
      conf$addSampler(target =c(x.char),
                      control <- list(propCov = diag(ncol(X))*propCov.means),
                      type='RW_block')
    }
    
    ## important!
    ## this is needed for correct indexing later
    samplerNumberOffset <- length(conf$getSamplers())
    
    for(i in 1:length(y.ind)) {
      node <- paste0('y.censored[',i,']')
      conf$addSampler(node, 'toggle', control=list(type='RW'))
      ## could instead use slice samplers, or any combination thereof, e.g.:
      ##conf$addSampler(node, 'toggle', control=list(type='slice'))
    }
    
    conf$printSamplers()
    ## can monitor y.censored, if you wish, to verify correct behaviour
    #conf$addMonitors('y.censored')
    
    Rmcmc <- buildMCMC(conf)
    
    Cmodel <- compileNimble(model_pred)
    Cmcmc <- compileNimble(Rmcmc, project = model_pred)
    
    for(i in 1:length(y.ind)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
    }
    
  
    
  }else{
    Cmodel$y.ind <- y.ind
    Cmodel$y.censored <- y.censored
    Cmodel$aq <- aqq
    Cmodel$bq <- bqq
    Cmodel$muf <- mu.f
    Cmodel$pf <- solve(Pf)
    Cmodel$r <- (R) #precision
    
    # inits.pred = list(q = diag(length(mu.f))*(length(mu.f)+1),
    #                   X.mod = rnorm(length(mu.f),mu.f,1),
    #                   X = rnorm(ncol(X),mu.f,1),
    #                   y_star = rnorm(length(y.censored),mu.f,1)) #
    # 
    # Cmodel$setInits(inits.pred())
    
    for(i in 1:length(y.ind)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
    }
    
  }

  dat.nchains <-
    runMCMC(
      Cmcmc,
      niter = nitr,
      nburnin = nburnin,
      nchains = 3
    )
  
  
  
  which_f <- seq(1,ncol(dat.nchains[[1]]))[-grep(c("y"), colnames(dat.nchains[[1]]))]
  gelman.keep <- numeric(length(which_f))
  for(ff in which_f){
    mcmc.check <- list()
    
    mcmc.check[[1]] <- coda::mcmc(dat.nchains[[1]][,ff])
    mcmc.check[[2]] <- coda::mcmc(dat.nchains[[2]][,ff])
    mcmc.check[[3]] <- coda::mcmc(dat.nchains[[3]][,ff])
    
    
    gelman.keep[ff] <- try(coda::gelman.diag(mcmc.check,transform = T)$psrf[1])
  }
  print(gelman.keep)
  if(any(gelman.keep > 1.5,na.rm = T)) PEcAn.logger::logger.warn(paste('Gelman value > 1.5 for GEF model. Re-assess time point', t))
  
  try(save(gelman.keep.tobit2space,gelman.keep,file = file.path(outdir, paste0('gelman.diag',t,'.Rdata'))))
  
  dat <- do.call(rbind,dat.nchains)
  set.seed(t)
  dat_save <- dat[sample(1:nrow(dat),size=500,replace = F),]
  save(dat_save, file = file.path(outdir, paste0('dat',t,'.Rdata')))
  
  ## update parameters
  iq   <- grep("q", colnames(dat))
  iX   <- grep("X[", colnames(dat), fixed = TRUE)
  iystar   <- grep("y_star", colnames(dat), fixed = TRUE)
  iX.mod <- grep("X.mod", colnames(dat), fixed = TRUE)
  
  mu.a <- colMeans(dat[, iX])
  
  ystar.a <- colMeans(dat[, iystar])
  Pa   <- stats::cov(dat[, iX])
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
  
  aqq   <- V
  bqq   <- n
  
  grDevices::pdf(file.path(outdir, paste0('dat_plot', t, '.pdf')))
  graphics::par(mfrow = c(2, 3))
  
  for(rr in 1:length(iX)){
    graphics::plot(dat_save[,iX[rr]],typ = 'l',main = paste('X',rr))
    abline(h=mu.f[rr],col='blue')
  }
  
  for (rr in 1:length(iystar)) {
    graphics::plot(dat_save[,iystar[rr]], type = 'l', main = paste('iystar',rr))
    abline(h=(mu.a)[rr],col='red')
  }
  
  for (rr in 1:length(iX)) {
    graphics::plot(dat_save[,iX.mod[rr]], type = 'l', main = paste('iX.mod',rr))
    abline(h=mu.f[rr],col='blue')
  }
  eigen_save <- matrix(NA,nrow=nrow(dat_save),ncol=ncol(X))
  for(rr in 1:nrow(dat_save)) {
    eigen_save[rr,] <- eigen((matrix(dat_save[rr, iq],ncol(X),ncol(X))))$values
  }
  apply(eigen_save,2,graphics::plot,typ='l')
  grDevices::dev.off()
  
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
##' @description This function creates a matrix mapping obsereved data to their forecast state variable.
##' 
##' @return This returns a matrix specifying which observation go with which state variables.
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

