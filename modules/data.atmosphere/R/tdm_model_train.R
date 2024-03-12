##' TDM Model Train
##' Linear regression calculations for specific met variables
# ----------------------------------- 
# Description
# -----------------------------------
##' @title model.train
##' @family tdm - Temporally Downscale Meteorology
##' @author Christy Rollinson, James Simkins
##' @description Function to create linear regression models for specific met
##'              variables. This is used in conjunction with temporal.downscale.functions()
##'              to generate linear regression statistics and save their output to be called
##'              later in lm_ensemble_sims().
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param dat.subset data.frame containing lags, next, and downscale period data
##' @param n.beta number of betas to pull from
##' @param resids TRUE or FALSE, whether to use residuals or not
##' @param threshold NULL except for surface_downwelling_shortwave_radiation, helps with our
##'                  distinction between day and night (no shortwave without sunlight)
##' @export
# -----------------------------------
#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
model.train <- function(dat.subset, v, n.beta, resids = resids, threshold = NULL, ...) {
  dat.subset$year <- as.ordered(dat.subset$year) 
  if (v == "air_temperature") {
    
    mod.doy <- stats::lm(air_temperature ~ as.ordered(hour) * air_temperature_max.day * 
                    (lag.air_temperature + lag.air_temperature_min + air_temperature_min.day) + 
                    as.ordered(hour) * air_temperature_min.day * next.air_temperature_max - 
                    1 - as.ordered(hour) - lag.air_temperature - lag.air_temperature_min - 
                    next.air_temperature_max - air_temperature_max.day - 
                    air_temperature_min.day, data = dat.subset)  #
  }
  
  if (v == "surface_downwelling_shortwave_flux_in_air") {
    # Don't bother trying to fit hours that are completely or pretty darn
    # close to dark
    hrs.day <- unique(dat.subset[dat.subset$surface_downwelling_shortwave_flux_in_air > 
                                   threshold, "hour"])
    
    mod.doy <- stats::lm(surface_downwelling_shortwave_flux_in_air ~ 
                    as.ordered(hour) * surface_downwelling_shortwave_flux_in_air.day - 
                    1 - surface_downwelling_shortwave_flux_in_air.day - 
                    as.ordered(hour), data = dat.subset[dat.subset$hour %in% 
                                                          hrs.day, ])  ###
  }
  
  if (v == "surface_downwelling_longwave_flux_in_air") {
    mod.doy <- stats::lm(sqrt(surface_downwelling_longwave_flux_in_air) ~ 
                    as.ordered(hour) * surface_downwelling_longwave_flux_in_air.day * 
                    (lag.surface_downwelling_longwave_flux_in_air + next.surface_downwelling_longwave_flux_in_air) - 
                    as.ordered(hour) - 1 - lag.surface_downwelling_longwave_flux_in_air - 
                    next.surface_downwelling_longwave_flux_in_air - surface_downwelling_longwave_flux_in_air.day - 
                    surface_downwelling_longwave_flux_in_air.day * lag.surface_downwelling_longwave_flux_in_air - 
                    surface_downwelling_longwave_flux_in_air.day * next.surface_downwelling_longwave_flux_in_air, 
                  data = dat.subset)  ###
    
  }
  
  if (v == "precipitation_flux") {
    
    # Precip needs to be a bit different.  We're going to calculate the
    # fraction of precip occuring in each hour we're going to estimate the
    # probability distribution of rain occuring in a given hour
    dat.subset$rain.prop <- dat.subset$precipitation_flux/(dat.subset$precipitation_flux.day)
    mod.doy <- stats::lm(rain.prop ~ as.ordered(hour) - 1 , data = dat.subset)
  }
  
  if (v == "air_pressure") {
    mod.doy <- stats::lm(air_pressure ~ as.ordered(hour) * (air_pressure.day + 
                                                       lag.air_pressure + next.air_pressure) - as.ordered(hour) - 
                    1 - air_pressure.day - lag.air_pressure - next.air_pressure, 
                  data = dat.subset)
  }
  
  if (v == "specific_humidity") {
    mod.doy <- stats::lm(log(specific_humidity) ~ as.ordered(hour) * 
                    specific_humidity.day * (lag.specific_humidity + next.specific_humidity + 
                                               air_temperature_max.day) - as.ordered(hour) - 1 - air_temperature_max.day, 
                  data = dat.subset)
  }
  
  if (v == "wind_speed") {
    mod.doy <- stats::lm(sqrt(wind_speed) ~ as.ordered(hour) * wind_speed.day * 
                    (lag.wind_speed + next.wind_speed) - as.ordered(hour) - 
                    1 - wind_speed.day - lag.wind_speed - next.wind_speed - 
                    wind_speed.day * lag.wind_speed - wind_speed.day * next.wind_speed, 
                  data = dat.subset)
  }
  
  
  
  # If we can't estimate the covariance matrix, stop & increase the
  # moving window
  if (is.na(summary(mod.doy)$adj.r.squared)) {
    stop(paste0("Can not estimate covariance matrix for day of year: ", 
                unique(dat.subset$doy), ";  Increase day.window and try again"))
  }
  
  # ----- Each variable must do this Generate a bunch of random
  # coefficients that we can pull from without needing to do this step
  # every day
  if(n.beta>1){
    mod.coef <- stats::coef(mod.doy)
    mod.cov <- stats::vcov(mod.doy)
    piv <- as.numeric(which(!is.na(mod.coef)))
    Rbeta <- MASS::mvrnorm(n = n.beta, mod.coef[piv], mod.cov[piv,piv])
  } else {
    Rbeta <- matrix(stats::coef(mod.doy), nrow=1)
    colnames(Rbeta) <- names(stats::coef(mod.doy))
  }
  
  list.out <- list(model = mod.doy, betas = Rbeta)
  
  # Model residuals as a function of hour so we can increase our
  # uncertainty
  if (resids == TRUE) {
    if (v == "air_temperature") {
      dat.subset[!is.na(dat.subset$lag.air_temperature) & !is.na(dat.subset$next.air_temperature_max), 
                 "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * (air_temperature_max.day * 
                                                      air_temperature_min.day) - 1, data = dat.subset[!is.na(dat.subset$lag.air_temperature), 
                                                                                                      ])
    }
    
    if (v == "surface_downwelling_shortwave_flux_in_air") {
      dat.subset[dat.subset$hour %in% hrs.day, "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * surface_downwelling_shortwave_flux_in_air.day - 
                          1, data = dat.subset[dat.subset$hour %in% hrs.day, 
                                               ])
    }
    
    if (v == "surface_downwelling_longwave_flux_in_air") {
      dat.subset[!is.na(dat.subset$lag.surface_downwelling_longwave_flux_in_air) & 
                   !is.na(dat.subset$next.surface_downwelling_longwave_flux_in_air), 
                 "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * surface_downwelling_longwave_flux_in_air.day - 
                          1, data = dat.subset[, ])
    }
    
    if (v == "precipitation_flux") {
      dat.subset[, "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * precipitation_flux.day - 
                          1, data = dat.subset[, ])
    }
    
    if (v == "air_pressure") {
      dat.subset[!is.na(dat.subset$lag.air_pressure) & !is.na(dat.subset$next.air_pressure), 
                 "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * air_pressure.day - 
                          1, data = dat.subset[, ])
    }
    
    if (v == "specific_humidity") {
      dat.subset[!is.na(dat.subset$lag.specific_humidity) & 
                   !is.na(dat.subset$next.specific_humidity), "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * specific_humidity.day - 
                          1, data = dat.subset[, ])
    }
    
    if (v == "wind_speed") {
      dat.subset[!is.na(dat.subset$lag.wind_speed) & !is.na(dat.subset$next.wind_speed), 
                 "resid"] <- stats::resid(mod.doy)
      resid.model <- stats::lm(resid ~ as.ordered(hour) * wind_speed.day - 
                          1, data = dat.subset[, ])
    }
    
    if(n.beta>1){
      res.coef <- stats::coef(resid.model)
      res.cov <- stats::vcov(resid.model)
      res.piv <- as.numeric(which(!is.na(res.coef)))
      
      beta.resid <- MASS::mvrnorm(n = n.beta, res.coef[res.piv], res.cov)      
    } else {
      beta.resid <- matrix(stats::coef(resid.model), nrow=1)
      colnames(beta.resid) <- names(stats::coef(mod.doy))
    }

    
    list.out[["model.resid"]] <- resid.model
    list.out[["betas.resid"]] <- beta.resid
  }
  return(list.out)
}
