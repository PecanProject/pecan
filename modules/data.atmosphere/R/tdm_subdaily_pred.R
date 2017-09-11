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
  
  mod.terms <- terms(model.predict)
  mod.coef <- coef(model.predict)
  mod.cov <- vcov(model.predict)
  mod.resid <- resid(model.predict)
  piv <- as.numeric(which(!is.na(mod.coef)))
  
  m <- model.frame(mod.terms, newdata, xlev = model.predict$xlevels)
  Xp <- model.matrix(mod.terms, m, contrasts.arg = model.predict$contrasts)
  
  if (resid.err == TRUE) {
    newdata$resid <- 99999
	  resid.piv <- as.numeric(which(!is.na(model.resid$coef)))
  
  	model.resid$factors[model.resid$factors=="as.ordered(hour)"] <- "hour"
	  resid.m  <- newdata[,model.resid$factors]
	  resid.m[,"as.ordered(hour)"] <- resid.m$hour
  	if(length(df.hr$hour)!= length(resid.m$hour)) resid.m <- merge(resid.m, df.hr, all=T)
    Xp.res <- model.matrix(eval(model.resid$formula), resid.m, contrasts.arg=model.resid$contr)
    err.resid <- Xp.res[, resid.piv] %*% t(Rbeta.resid)
  } # End residual error
  
  dat.sim <- Xp[, piv] %*% t(Rbeta) + err.resid
  
  return(dat.sim)
  
}