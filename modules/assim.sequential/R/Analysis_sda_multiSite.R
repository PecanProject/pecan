##' @title EnKF.MultiSite
##' @name  EnKF.MultiSite
##' @author Michael Dietze \email{dietze@@bu.edu}, Ann Raiho and Hamze Dokoohaki
##' 
##' @param settings  pecan standard settings list.  
##' @param Forecast A list containing the forecasts variables including Q (process variance) and X (a dataframe of forecasts state variables for different ensemble)
##' @param Observed A list containing the observed variables including R (cov of observed state variables) and Y (vector of estimated mean of observed state variables)
##' @param H is a mtrix of 1's and 0's specifying which observations go with which state variables.
##' @param extraArg This argument is NOT used inside this function but it is a list containing aqq, bqq and t. The aqq and bqq are shape parameters estimated over time for the proccess covariance and t gives the time in terms of index of obs.list.
##' @param ... Extra argument sent to the analysis function.
##' @details This function is different than `EnKF` function in terms of how it creates the Pf matrix.
##'  
##' 
##' @description Given the Forecast and Observed this function performs the Ensemble Kalamn Filter. 
##' 
##' @return It returns a list with estimated mean and cov matrix of forecast state variables as well as mean and cov estimated as a result of assimilation/analysis .
##' @export
EnKF.MultiSite <-function(setting, Forecast, Observed, H, extraArg=NULL, ...){
  #------------------------------Setup
  Localization.FUN <- settings$state.data.assimilation$Localization.FUN # localization function
  scalef <- settings$state.data.assimilation$scalef %>% as.numeric() # scale factor for localization
  var.names <- sapply(settings$state.data.assimilation$state.variable, '[[', "variable.name")
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
    
    #Finding the distance between the sites
    distances <- sp::spDists(site.locs, longlat=T)
    #turn that into a blocked matrix format
    blocked.dis<-block_matrix(distances %>% as.numeric(), rep(length(var.names), length(site.ids)))
    
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
