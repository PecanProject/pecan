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
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecast state variables for different ensemble)
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
GEF<-function(setting,Forecast,Observed, H, extraArg, nitr=50000, nburnin=10000, ...){
  #------------------------------Setup
  #-- reading the dots and exposing them to the inside of the function
  dots<-list(...)
  if (length(dots)>0) lapply(names(dots),function(name){assign(name,dots[[name]], pos=1 )})
  #General
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  #Loading nimbles functions
  PEcAn.assim.sequential:::load_nimble()
  #load_nimble()
  #Forecast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  Pf = cov(X) # Cov Forecast - This is used as an initial condition
  mu.f <- colMeans(X) #mean Forecast - This is used as an initial condition
  #Observed inputs
  R <- Observed$R
  Y <- Observed$Y
  wish.df <- function(Om, X, i, j, col) {
    (Om[i, j]^2 + Om[i, i] * Om[j, j]) / var(X[, col])
  }
  interval <- NULL
  X.new <- NULL
  # Reading the extra arguments
  aqq <- extraArg$aqq
  bqq <- extraArg$bqq
  t <- extraArg$t
  nitr.GEF<-extraArg$nitr.GEF
  nthin<-extraArg$nthin
  nburnin <- extraArg$nburnin
  censored.data <- extraArg$censored.data
  #----------------------------------- GEF-----------------------------------------------------
  # Taking care of censored data ------------------------------    
  ### create matrix the describes the support for each observed state variable at time t
  ###-------------------------------------------------------------------###
  # if we had censored data
  ###-------------------------------------------------------------------###----
  if (censored.data) {
    out.cens<-tobit_model_censored (settings, X, var.names, mu.f, Pf, t)
    mu.f <- out.cens$mu.f
    Pf <- out.cens$Pf
    iycens <- out.cens$iycens
    X.new <- out.cens$X.new
  } # end of if we have censored data
  

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
  rownames(interval) <- names(obs.mean[[t]][[1]]) # I have a one here because 
  for(i in seq_len(length(var.names))){
    interval[which(startsWith(rownames(interval),
                              var.names[i])),] <-
      matrix(c(
        as.numeric(
          settings$state.data.assimilation$state.variables[[i]]$min_value
        ),
        as.numeric(
          settings$state.data.assimilation$state.variables[[i]]$max_value
        )
      ),
      length(which(startsWith(
        rownames(interval),
        var.names[i]
      ))), 2, byrow = TRUE)
  }
  #### These vectors are used to categorize data based on censoring 
  #### from the interval matrix
  y.ind <- as.numeric(Y > interval[,1])
  y.censored <- as.numeric(ifelse(Y > interval[,1], Y, 0))
  
  if(t == 1){ #TO DO need to make something that works to pick whether to compile or not
    # Contants defined in the model

    constants.tobit <- list(N = ncol(X),
                            YN = length(y.ind)
                           )
    
    dimensions.tobit <- list(X = length(mu.f),
                            X.mod = ncol(X),
                            Q = c(length(mu.f),
                                  length(mu.f))
                            )
    # Data need to be used in the model
    if (length(y.ind)==1) {
      RR <- as.numeric(chol(solve(R)))
    }else{
      RR <-chol(solve(R))
    }
    
    data.tobit = list(
      muf = as.vector(mu.f),
      pf = solve(Pf),
      aq = aqq[t, , ],
      bq = bqq[t] %>% as.vector(),
      y.ind = y.ind ,
      y.censored = y.censored ,
      r = RR
    )
    #initial values
    inits.pred = list(q = diag(length(mu.f)),
                      X.mod = as.vector(mu.f),
                      X = as.vector(mu.f)
                      ) # This was this before rnorm(length(mu.f),0,1), I thought the mu.f would be a better IC for something like abv ground biomass than something close to zero.) #
    
    model_pred <-
      nimbleModel(
        tobit.model,
        data = data.tobit,
        dimensions = dimensions.tobit,
        constants = constants.tobit,
        inits = inits.pred,
        name = 'base'
      )
    ## Adding X.mod,q,r as data for building model.
    conf <- configureMCMC(model_pred, print=TRUE)
    conf$addMonitors(c("X","q","Q")) 
    ## [1] conjugate_dmnorm_dmnorm sampler: X[1:5]
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
    Cmodel$r <- solve(R)
    
    inits.pred = list(q = diag(length(mu.f)), X.mod = as.vector(mu.f),
                      X = rnorm(ncol(X),0,1)) #
    Cmodel$setInits(inits.pred)
    
    for(i in 1:length(y.ind)) {
      ## ironically, here we have to "toggle" the value of y.ind[i]
      ## this specifies that when y.ind[i] = 1,
      ## indicator variable is set to 0, which specifies *not* to sample
      valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-y.ind[i])
    }
    
  }
  
  dat <- runMCMC(Cmcmc, niter = nitr, nburnin=nburnin)
  
  ## update parameters
  iq   <- grep("q", colnames(dat))
  iX   <- grep("X[", colnames(dat), fixed = TRUE)
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
  
  return(list(mu.f = mu.f,
              Pf = Pf,
              mu.a = mu.a,
              Pa = Pa,
              q.bar = q.bar,
              n = n,
              X.new=X.new,
              aqq=aqq,
              bqq=bqq
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

