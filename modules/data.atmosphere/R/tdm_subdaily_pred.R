##' Subdaily Prediction
##' Pulls information from linear regression models to predict subdaily meteorology
# ----------------------------------- 
# Description
# -----------------------------------
##' @title subdaily_pred
##' @family tdm - Temporally Downscale Meteorology
##' @author Christy Rollinson, James Simkins
##' @description Function to pull objects created in linear regression models
##'              and are used to predict subdaily meteorology. This function is called in
##'              lm_ensemble_sims() to downscale a meteorology product.
##'              Linear regression models are created in gen.subdaily.models()
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param newdata dataframe with data to be downscaled
##' @param model.predict saved linear regression model
##' @param Rbeta matrix with Rbetas from saved linear regression model
##' @param resid.err logical, whether to include residual error or not
##' @param model.resid data.frame of model residuals
##' @param Rbeta.resid data.frame of Rbeta residuals
##' @param n.ens number of ensembles to create
##' @export
# -----------------------------------
#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
subdaily_pred <- function(newdata, model.predict, Rbeta, resid.err = FALSE, model.resid = NULL, 
                          Rbeta.resid = NULL, n.ens) {

  err.resid <- 0  # dummy residual error term; if we want to add residual error, we're modeling it by hour

  df.hr <- data.frame(hour = model.predict$xlev[[1]])
  df.hr[,"as.ordered(hour)"] <- as.ordered(df.hr$hour)
 
  piv <- as.numeric(which(!is.na(model.predict$coef)))
  
  model.predict$factors[model.predict$factors=="as.ordered(hour)"] <- "hour"
  m  <- newdata[,model.predict$factors]
  m[,"as.ordered(hour)"] <- as.ordered(m$hour)
  m$hour <- as.numeric(m$hour)

  # Adding hours to make sure prediction works okay
  # Note: This really messes with the order of things!
  if(length(unique(df.hr$hour))!= length(unique(m$hour))){
    m$ens <- newdata$ens
    
    m <- merge(m, df.hr, all=T)
    
    # Ordering the newdata in the same way as m (by hour)
    m <- m[order(m$ens, m$hour),]
    # newdata <- newdata[order(newdata$hour),]
    
    # # Fixing the ordering so that it comes back looking like newdata
    # dat.sim <- dat.sim[order(dat.sim$ens, dat.sim$hour),]
    # newdata <- newdata[order(newdata$ens, newdata$hour),]
  } 
  
  Xp <-  stats::model.matrix(eval(model.predict$formula), m, contrasts.arg=model.predict$contr)

  if (resid.err == TRUE) {
    newdata$resid <- 99999
	  resid.piv <- as.numeric(which(!is.na(model.resid$coef)))
  
  	model.resid$factors[model.resid$factors=="as.ordered(hour)"] <- "hour"
	  resid.m  <- newdata[,model.resid$factors]
	  resid.m[,"as.ordered(hour)"] <- resid.m$hour
  	if(length(df.hr$hour)!= length(resid.m$hour)) resid.m <- merge(resid.m, df.hr, all=T)
    Xp.res <- stats::model.matrix(eval(model.resid$formula), resid.m, contrasts.arg=model.resid$contr)
    err.resid <- Xp.res[, resid.piv] %*% t(Rbeta.resid)
  } # End residual error
  
  if(length(piv)==ncol(Rbeta)){
    dat.sim <- Xp[, piv] %*% t(Rbeta) + err.resid
  } else {
    # dat.sim <- Xp[,piv] %*% t(Rbeta[,piv]) + err.resid
    dat.sim <- Xp[,piv] %*% t(matrix(Rbeta[,piv], nrow=nrow(Rbeta))) + err.resid
  }
  
  
  
  return(dat.sim)
  
}