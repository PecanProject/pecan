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
    resid.terms <- terms(model.resid)
    resid.coef <- coef(model.resid)
    resid.cov <- vcov(model.resid)
    resid.resid <- resid(model.resid)
    resid.piv <- as.numeric(which(!is.na(resid.coef)))
    
    m2 <- model.frame(resid.terms, newdata, xlev = model.resid$xlevels)
    Xp.res <- model.matrix(resid.terms, m2, contrasts.arg = model.resid$contrasts)
    
    err.resid <- Xp.res[, resid.piv] %*% t(Rbeta.resid)
  }
  
  dat.sim <- Xp[, piv] %*% t(Rbeta) + err.resid
  
  return(dat.sim)
  
}