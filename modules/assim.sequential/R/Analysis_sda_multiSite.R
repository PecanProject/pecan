##' @title EnKF.MultiSite
##' @name  EnKF.MultiSite
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H is a matrix of 1's and 0's specifying which observations go with which state variables.
##' @param extraArg This argument is NOT used inside this function but it is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list.
##' @param ... Extra argument sent to the analysis function.
##' @details This function is different than `EnKF` function in terms of how it creates the Pf matrix.
##'  
##' 
##' @description Given the Forecast and Observed this function performs the Ensemble Kalamn Filter. 
##' 
##' @return It returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export
EnKF.MultiSite <- function(settings, Forecast, Observed, H, extraArg=NULL, ...){
  #------------------------------Setup
  Localization.FUN <- settings$state.data.assimilation$Localization.FUN # localization function
  scalef <- settings$state.data.assimilation$scalef %>% as.numeric() # scale factor for localization
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  site.ids <- settings %>% purrr::map(~.x[['run']] ) %>% purrr::map('site') %>% purrr::map('id') %>% unlist()
  #-- reading the dots and exposing them to the inside of the function
  dots<-list(...)
  if (length(dots)>0) lapply(names(dots),function(name){assign(name,dots[[name]])})
  for(i in seq_along(dots)) assign(names(dots)[i],dots[[names(dots)[i]]])

    #Forcast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  #Observed inputs
  R <- Observed$R
  Y <- Observed$Y

  # Enkf---------------------------------------------------
  mu.f <- as.numeric(apply(X, 2, mean, na.rm = TRUE)) %>%
    `attr<-`('Site', c(rep(site.ids, each=length(var.names))))
  # I make the Pf in a separate function
  if(length(site.ids)>1){
    # This the function makes the Pf by creating blocks in a matrix for different sites
    # We can also send a localization functions to this 
    # for extra argumnets like distance matrix for localization use elipsis
    Pf <- Contruct.Pf (site.ids, var.names, X,
                       localization.FUN=eval(parse(text = Localization.FUN)),
                       t=extraArg$t,
                       blocked.dis,
                       scalef)
  }else{
    PEcAn.logger::logger.severe("You need to send this function a multisetting object containing multiple sites/runs.")
  }
  ## process error
  if (!is.null(Q)) {
    Pf <- Pf + Q
  }
  
  if (length(Y) > 1) {
    PEcAn.logger::logger.info("The zero variances in R and Pf is being replaced by half and one fifth of the minimum variance in those matrices respectively.")
    diag(R)[which(diag(R)==0)] <- min(diag(R)[which(diag(R) != 0)])/2
    diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5
  }
  
  ## Kalman Gain
  K <- Pf %*% t(H) %*% solve((R + H %*% Pf %*% t(H)))
  # Analysis
  mu.a <- mu.f + K %*% (Y - H %*% mu.f)
  Pa   <- (diag(ncol(X)) - K %*% H) %*% Pf
  return(list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa))
}



##' @rdname GEF
##' @export
GEF.MultiSite <- function(settings, Forecast, Observed, H, extraArg,...){
  #-- reading the dots and exposing them to the inside of the function
  dots<-list(...)
  if (length(dots) > 0) lapply(names(dots),function(name){assign(name,dots[[name]], pos = 1 )})
  #General
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
  
  #Define Q type from settings.
  q.type <- toupper(settings$state.data.assimilation$q.type)
  single.q <-1
  Site.q <-2
  pft.q <-3
  if (is.null(q.type) | q.type=="SINGLE") {
    q.type <- single.q
  } else{
    q.type <- ifelse(q.type == "SITE", Site.q, pft.q)
  } 
  
  #Forecast inputs 
  Q <- Forecast$Q # process error
  X <- Forecast$X # states 
  if(!is.null(extraArg$Pf)){
    Pf <- extraArg$Pf
  }else{
    Pf <- stats::cov(X) # Cov Forecast - This is used as an initial condition
    diag(Pf)[which(diag(Pf)==0)] <- min(diag(Pf)[which(diag(Pf) != 0)])/5 #fixing det(Pf)==0
  }
  mu.f <- colMeans(X) #mean Forecast - This is used as an initial condition
  
  #Observed inputs
  R <- Observed$R
  Y <- Observed$Y
  wish.df <- function(Om, X, i, j, col) {
    (Om[i, j]^2 + Om[i, i] * Om[j, j]) / stats::var(X[, col])
  }
  #----------------------------------- GEF-----------------------------------------------------
  interval <- NULL
  #added this line in case you don't need to do censoring.
  X.new <- NULL
  # Reading the extra arguments
  aqq <- extraArg$aqq
  bqq <- extraArg$bqq
  wts <- extraArg$wts/sum(extraArg$wts)
  if(any(is.na(wts))){
    PEcAn.logger::logger.warn(
      "We found an NA in the wts for the ensemble members.",
      "Is this what you want? For now, we will change the NA to a zero.")
    wts[is.na(wts)] <- 0
  }
  if(sum(wts==0)){
    wts <- rep(1,nrow(X))/nrow(X)
  }
  t <- extraArg$t
  nitr.GEF<-extraArg$nitr.GEF
  nthin<-extraArg$nthin
  nburnin <- extraArg$nburnin
  censored.data <- extraArg$censored.data
  ###-------------------------------------------------------------------###
  # if we had censored data and we don't have pre-calculated Pf.
  ###-------------------------------------------------------------------###----
  if (censored.data && is.null(extraArg$Pf)) {
    out.cens<-tobit_model_censored (settings, X, var.names, mu.f, Pf, t)
    mu.f <- out.cens$mu.f
    Pf <- out.cens$Pf
    iycens <- out.cens$iycens
    X.new <- out.cens$X.new
  } # end of if we have censored data
  
  ###-------------------------------------------------------------------###
  # Generalized Ensemble Filter                                       ###-----
  ###-------------------------------------------------------------------###
  # if(sum(diag(Pf)-diag(cov(X))) > 10 | sum(diag(Pf)-diag(cov(X))) < -10) logger.severe('Increase Sample Size')
  #--- This is where the localization needs to happen - After imputing Pf
  elements.W.Data <- which(apply(H, 2, sum) == 1)
  if (exists('blocked.dis') & is.null(extraArg$Pf)){
    Pf <- Local.support(Pf, blocked.dis, settings$state.data.assimilation$scalef %>% as.numeric())
  }

  #### initial conditions
  ## we only calculate aqq and bqq when t=1.
  if (t == 1) {
    bqq[1] <- length(elements.W.Data)
    if (is.null(aqq)) {
      if (q.type==Site.q) { # if we wanna estimate a q per site
        aqq <-
          array(1, dim = c(length(elements.W.Data), length(elements.W.Data), nt))
        for (i in 1:nt) {
          aqq[,,i] <- diag(length(elements.W.Data))
        }
      } else if(q.type == pft.q){ # if we wanna estimate a q per PFT
        site.pfts <- settings %>%
          purrr::map( ~ .x[['run']]) %>%
          purrr::map('site') %>%
          purrr::map('site.pft') %>%
          purrr::map('pft.name') %>%
          purrr::modify(as.factor) %>%
          purrr::modify(as.numeric) %>%
          purrr::modify_if(function(x) {
            if (length(x) > 0) {
              return(FALSE)
            } else{
              return(TRUE)
            }
          },  ~ 1) %>%
          unlist()
        aqq<- array(1, dim = c(max(site.pfts), max(site.pfts), nt))
      }else{ # This is where we estimate just one q for all
        aqq<- array(1, dim = c(1, 1, nt))
      }
    } else{
      if (length(elements.W.Data) != dim(aqq)[1] |
          length(elements.W.Data) != dim(aqq)[2]) {
        PEcAn.logger::logger.warn('error: X has changed dimensions')
      }
    }
  } else{
    # if(length(elements.W.Data)==ncol(aqq[, , t])){
      if (ncol(aqq) > 1 & nrow(aqq) > 1)
        aqq[, , t] <- Local.support(
          aqq[, , t],
          distances[ceiling(elements.W.Data/length(var.names)), # finding sites with data
                    ceiling(elements.W.Data/length(var.names))],
          settings$state.data.assimilation$scalef %>% as.numeric()
        )
      }

  ### create matrix the describes the support for each observed state variable at time t
  interval <- matrix(NA, length(unlist(obs.mean[[t]])), 2)
  
  # if this function is revoked by multisite then the structure of data looks a bit different.
  if (exists('blocked.dis')){
    rownames(interval) <- obs.mean[[t]] %>% purrr::flatten() %>% names()
    
  }else{
    rownames(interval) <- names(obs.mean[[t]])
  }
  
  for (i in 1:length(var.names)) {
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
  y.ind <- as.numeric(Y > interval[, 1])
  y.censored <- as.numeric(ifelse(Y > interval[, 1], Y, 0))
  data <- list(elements.W.Data = elements.W.Data,
               X = X,
               Pf = Pf,
               aqq = aqq,
               bqq = bqq,
               mu.f = mu.f,
               q.type = q.type,
               R = R,
               y.censored = y.censored,
               y.ind = y.ind,
               nitr.GEF = extraArg$nitr.GEF,
               nburnin = extraArg$nburnin,
               nthin = extraArg$nthin,
               monitors = c("Xall", "qq"))
  outputs <- furrr::future_map(lapply(rep("data", as.numeric(settings$state.data.assimilation$chains)), get), MCMC_function)
  dat <- do.call(rbind, outputs)
  
  #---- Saving the chains
  save(dat, file=file.path(settings$outdir, paste0('dat',t,'.Rdata')))
  
  ## update parameters
  iX   <- grep("Xall[", colnames(dat), fixed = TRUE)
  mu.a <- colMeans(dat[, iX])
  Pa   <- stats::cov(dat[, iX])
  Pa[is.na(Pa)] <- 0
  mq <- dat[,  grep("q", colnames(dat))]  # Omega, Precision
  q.bar <- matrix(apply(mq, 2, mean),
                  length(elements.W.Data),
                  length(elements.W.Data)
                  )  # Mean Omega, Precision

  # Setting up the prior for the next step from the posterior of this step
  if (t < nt){
    if (q.type == single.q){ #if it's a gamma case
      qq <- dat[,  grep("qq", colnames(dat))]
      aqq[1, 1, t + 1] <- (mean(qq))^2/stats::var(qq)
      bqq[t + 1] <- mean(qq)/stats::var(qq)
    } else { # if it's a wish case
      col <- matrix(1:length(elements.W.Data) ^ 2,
                    length(elements.W.Data),
                    length(elements.W.Data))
      WV  <- matrix(0, length(elements.W.Data), length(elements.W.Data))
      for (i in seq_along(elements.W.Data)) {
        for (j in seq_along(elements.W.Data)) {
          WV[i, j] <- wish.df(q.bar, X = mq, i = i, j = j, col = col[i, j])
        }
      }
      n <- mean(WV)
      if (n < length(mu.f)) {
        n <- length(mu.f)
      }
      V <- solve(q.bar) * n
      aqq[, ,t + 1] <- V
      bqq[t + 1] <- n
    }
  }
  #---- Trying to release some of the memory back to the os 
  gc()
  #
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

##' @title MCMC_function
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho, Hamze Dokoohaki, and Dongchen Zhang.
##' @param data list containing everything needed for the MCMC sampling.
##' @details This function replace the previous code where implenmented the MCMC sampling part, which allows the MCMC sampling of multiple chains under parallel mode.
MCMC_function <- function(data){
  dimensions.tobit <- list(X = length(data$elements.W.Data),
                           X.mod = ncol(data$X),
                           Q = c(nrow(data$aqq), ncol(data$aqq))
  )
  # Contants defined in the model
  constants.tobit <-
    list(
      N = ncol(data$X),
      YN = length(data$elements.W.Data),
      nH = length(data$elements.W.Data),
      H = data$elements.W.Data,
      NotH = which(!(1:ncol(data$X) %in% data$elements.W.Data)),
      nNotH = which(!(1:ncol(data$X) %in% data$elements.W.Data)) %>% length(),
      q.type=data$q.type
    )
  # Data used for setting the likelihood and other stuff
  data.tobit <-
    list(
      muf = as.vector(data$mu.f),
      pf = data$Pf,
      aq = data$aqq[,,t],
      bq = data$bqq[t],
      y.ind = data$y.ind,
      y.censored = data$y.censored,
      r = solve(data$R)
    )
  if(constants.tobit$YN == 1){
    #add error message if trying to run SDA with 1 obs and 1 state variable no model currently exists to handle this case, need to remove for loop from GEF_singleobs_nimble for this case and save new model
    if(constants.tobit$N == 1){
      PEcAn.logger::logger.error("No model exists for assimilating 1 observation and 1 state variable, add more state variables or edit GEF_singleobs_nimble to work with 1 state variable")
      return(0)
    }
    #slight adjustment to inputs for nimble function when running with 1 obs
    inits.pred$qq <- 0.368
    dimensions.tobit$y.censored <- 1
    dimensions.tobit$y.ind <- 1
    constants.tobit$q.type <- NULL
    inits.pred <-
      list(
        X.mod = as.vector(data$mu.f),
        X = as.vector(data$mu.f)[data$elements.W.Data],
        Xall = as.vector(data$mu.f),
        Xs = as.vector(data$mu.f)[data$elements.W.Data],
        q = diag(1, length(data$elements.W.Data), length(data$elements.W.Data))
      )
    model_pred <- nimble::nimbleModel(GEF_singleobs_nimble,
                                      data = data.tobit,
                                      dimensions = dimensions.tobit,
                                      constants = constants.tobit,
                                      inits = inits.pred,
                                      name = 'base')
  }else{
    model_pred <- nimble::nimbleModel(GEF.MultiSite.Nimble,
                                      data = data.tobit,
                                      dimensions = dimensions.tobit,
                                      constants = constants.tobit,
                                      name = 'base')
  }
  ## Adding X.mod,q,r as data for building model.
  conf <- nimble::configureMCMC(model_pred, print=TRUE)
  conf$setMonitors(data$monitors) 
  samplerNumberOffset <- length(conf$getSamplers())
  
  for(i in 1:length(data$y.ind)) {
    node <- paste0('y.censored[',i,']')
    conf$addSampler(node, 'toggle', control=list(type='RW'))
  }
  #handling samplers
  samplerLists <- conf$getSamplers()
  samplerLists[[2]]$control <- list(propCov= data$Pf, adaptScaleOnly = TRUE, adaptive = TRUE)
  conf$setSamplers(samplerLists)
  
  conf$printSamplers()
  Rmcmc <- nimble::buildMCMC(conf)
  Cmodel <- nimble::compileNimble(model_pred)
  Cmcmc <- nimble::compileNimble(Rmcmc, project = model_pred, showCompilerOutput = TRUE)
  
  for(i in 1:length(data$y.ind)) {
    valueInCompiledNimbleFunction(Cmcmc$samplerFunctions[[samplerNumberOffset+i]], 'toggle', 1-data$y.ind[i])
  }
  inits <- function(){
    ind <- sample(seq_along(1:nrow(data$X)), 1)
    init_muf <- data$X[ind,]
    list(X.mod = as.vector(init_muf), 
         X = as.vector(init_muf)[data$elements.W.Data], 
         Xall = as.vector(init_muf),
         Xs = as.vector(init_muf)[data$elements.W.Data],
         q = diag(1, length(data$elements.W.Data), length(data$elements.W.Data)))
  }
  if(exists("inits.pred")){
    dat <- runMCMC(Cmcmc, niter = data$nitr.GEF, nburnin = data$nburnin, thin = data$nthin, nchains = 1)
  }else{
    dat <- runMCMC(Cmcmc, niter = data$nitr.GEF, nburnin = data$nburnin, thin = data$nthin, nchains = 1, inits = inits)
  }
  return(dat)
}