#--------------------------------------------------------------------------------------------------#
##' Calculate wavelet spectra of data-model residuals
##'
##' @title Wavelet spectra of data-model residuals  
##' @param data       numeric vector
##' @param model      numeric vector
##' @param obsPerDay  used to scale time axis to days
##' @param case       how residuals are normalized (1=none, 2=post, 3=pre)
##' @return wavelet spectra
##' @export
##' @author Mike Dietze
#--------------------------------------------------------------------------------------------------#

ResidSpectra <- function(data,model=NULL,obsPerDay=1,case=3){
  require(dplR)
  
  ##make sure everything's the right type
  data <- as.vector(data)
  if(is.null(model)) model = rep(0,length(data))
  model <- as.vector(model)
  y = NULL
  
  ## option 1 - absolute residuals
  if(case == 1){
    y <- data - model ### Model error fcn
  }
  ## option 2 - normalized residuals (post)
  if(case == 2){
    y = scale(data - model)
  }
  ## option 3 - normalized residuals (pre)
  if(case == 3){
    ## normalize data
    data.norm <- as.vector(scale(data))
    
    ## normalize model
    model.norm <- as.vector(scale(model))  
    y <- data.norm-model.norm  ## calc residuals of normalized          
  }
  
  y[is.na(y)] <- 0 ## need to fill in missing values
  
  ## Calculate Morlet wavelet spectrum
  wv <- morlet(y)                
  period <- wv$period/day        ## wavelet periods
  Power <- (abs(wv$wave))^2      ## wavelet power
  for(t in 1:length(wv$Scale)){  ## bias correction
    Power[,t] = Power[,t]/wv$Scale[t]
  }
  
  ## Crop out cone of influence
  coi <- wv$coi                  ## cone of influence (valid if below value)
  for(t in 1:length(coi)){
    sel <- which(period>coi[t])
    Power[t,sel] <- NA 
  }
  wv$Power <- Power
  wv$period <- period
  
  return(wv)
}
