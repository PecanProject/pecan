##' Debias Meteorology using Multiple Linear Regression
##' Statistically debias met datasets and generate ensembles based on the observed uncertainty
# -----------------------------------
# Description
# -----------------------------------
##'
##' @title debias.met.regression
##' @family debias - Debias & Align Meteorology Datasets into continuous time series
##' @author Christy Rollinson
##' @description This script debiases one dataset (e.g. GCM, re-analysis product) given another higher 
##'              resolution product or empirical observations. It assumes input are in annual CF standard 
##'              files that are generate from the pecan extract or download funcitons.  
# -----------------------------------
# Parameters
# -----------------------------------
##' @param train.data - training data coming out of align.met
##' @param source.data - data to be bias-corrected aligned with training data (from align.met)
##' @param n.ens  - number of ensemble members to generate and save for EACH source ensemble member
##' @param vars.debias - which met variables should be debiased? if NULL, all variables in train.data
##' @param CRUNCEP - flag for if the dataset being downscaled is CRUNCEP; if TRUE, special cases triggered for 
##'                  met variables that have been naively gapfilled for certain time periods
##' @param pair.anoms - logical stating whether anomalies from the same year should be matched or not
##' @param pair.ens - logical stating whether ensembles from train and source data need to be paired together 
##'                   (for uncertainty propogation)
##' @param uncert.prop - method for error propogation if only 1 ensemble member; options=c(random, mean); *Not Implemented yet
##' @param resids - logical stating whether to pass on residual data or not *Not implemented yet
##' @param seed - specify seed so that random draws can be reproduced
##' @param outfolder - directory where the data should go
##' @param yrs.save - what years from the source data should be saved; if NULL all years of the source data will be saved
##' @param ens.name - what is the name that should be attached to the debiased ensemble
##' @param ens.mems - what labels/numbers to attach to the ensemble members so we can gradually build bigger ensembles 
##'                   without having to do do giant runs at once; if NULL will be numbered 1:n.ens
##' @param lat.in - latitude of site
##' @param lon.in - longitude of site
##' @param save.diagnostics - logical; save diagnostic plots of output?
##' @param path.diagnostics - path to where the diagnostic graphs should be saved
##' @param parallel - (experimental) logical stating whether to run temporal_downscale_functions.R in parallel *Not Implemented yet
##' @param n.cores - (experimental) how many cores to use in parallelization *Not implemented yet
##' @param overwrite - overwrite existing files?
##' @param verbose
##' @export
# -----------------------------------
# Workflow
# -----------------------------------
# The general workflow is as follows: 
# 1. read in & format data (coerce to daily format)
# 2. set up the file structures for the output
# 3. define the training window
# 4. generate the bias-correction models to adjust the seasonal cycle
# 5. Model the anomalies
# 5. apply the climatology & anomaly models with covariance to generate a daily ensemble
# 6. Save specified years to file
# -----------------------------------

#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------


debias.met.regression <- function(train.data, source.data, n.ens, vars.debias=NULL, CRUNCEP=FALSE,
                                  pair.anoms = TRUE, pair.ens = FALSE, uncert.prop="mean", resids = FALSE, seed=Sys.Date(),
                                  outfolder, yrs.save=NULL, ens.name, ens.mems=NULL, lat.in, lon.in,
                                  save.diagnostics=TRUE, path.diagnostics=NULL,
                                  parallel = FALSE, n.cores = NULL, overwrite = TRUE, verbose = FALSE) {
  library(MASS)
  library(mgcv)
  library(ggplot2)
  library(stringr)
  library(lubridate)
  library(ncdf4)
  
  set.seed(seed)
  
  if(parallel==TRUE) warning("Warning! Parallel processing not reccomended because of memory constraints")
  if(ncol(source.data[[2]])>1) warning("Feeding an ensemble of source data is currently experimental!  This could crash")
  
  # Variables need to be done in a specific order
  vars.all <- c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", "specific_humidity", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "surface_downwelling_longwave_flux_in_air", "wind_speed", "precipitation_flux")
  
  if(is.null(vars.debias)) vars.debias <- vars.all[vars.all %in% names(train.data)] # Don't try to do vars that we don't have
  if(is.null(yrs.save)) yrs.save <- unique(source.data$time$Year)
  if(is.null(ens.mems)) ens.mems <- stringr::str_pad(1:n.ens, nchar(n.ens), "left", pad="0")
  
  # Set up outputs
  vars.pred <- vector()
  dat.out <- list()
  dat.out[["time"]] <- source.data$time
  
  # Transforming zero-truncated variables where negative values are not possible (and zero is unlikely)
  # - Tried a couple different things, but the sqaure root transformation seems to be working best
  vars.transform <- c("surface_downwelling_shortwave_flux_in_air", "specific_humidity", "surface_downwelling_longwave_flux_in_air", "wind_speed")
  
  # ---------
  # Setting up some cases about how to duplicate the training data in case we don't pass through the 
  # same number of ensembles as we want in our output
  # - Referencing off of whatever the layer after "time" is
  # ---------
  # If we have fewer columns then we need, randomly duplicate some
  if(ncol(train.data[[2]])==n.ens) ens.train <- 1:n.ens
  
  if(ncol(train.data[[2]]) < n.ens){
    ens.train <- c(1:ncol(train.data[[2]]), sample(1:ncol(train.data[[2]]), n.ens-ncol(train.data[[2]]),replace=T))
  }
  
  # If we have more columns than we need, randomly subset
  if(ncol(train.data[[2]]) > n.ens) {
    ens.train <- sample(1:ncol(train.data[[2]]), ncol(train.data[[2]]),replace=T)
  }
  
  # Setting up cases for dealing with an ensemble of source data to be biased
  if(pair.ens==T & ncol(train.data[[2]]!=ncol(source.data[[2]]))){ 
    stop("Cannot pair ensembles of different size")
  } else if(pair.ens==T) {
    ens.src <- ens.train
  }
  
  if(pair.ens==F & ncol(source.data[[2]])==1 ){
    ens.src=1
  } else if(pair.ens==F & ncol(source.data[[2]]) > n.ens) {
    ens.src <- sample(1:ncol(source.data[[2]]), ncol(source.data[[2]]),replace=T)
  } else if(pair.ens==F & ncol(source.data[[2]]) < n.ens){
    ens.src <- c(1:ncol(source.data[[2]]), sample(1:ncol(source.data[[2]]), n.ens-ncol(source.data[[2]]),replace=T))
  }
  # ---------
  
  # Find the period of years to use to train the model
  # This formulation should take
  yrs.overlap <- unique(train.data$time$Year)[unique(train.data$time$Year) %in% unique(source.data$time$Year)]
  
  # If we don't have a year of overlap, take closest 20 years from each dataset
  if(length(yrs.overlap)<1){
    warning("No overlap in years, so we cannot pair the anomalies")
    yrs.overlap <- (max(min(train.data$time$Year), min(source.data$time$Year))-20):(min(max(train.data$time$Year), max(source.data$time$Year))+20)
    pair.anoms=FALSE # we can't pair the anomalies no matter what we tried to specify before
  }

  # Cut out training data down to just the calibration period
  for(v in vars.debias){
    train.data[[v]] <- matrix(train.data[[v]][which(train.data$time$Year %in% yrs.overlap),], ncol=ncol(train.data[[v]]))
  }
  train.data$time <- train.data$time[which(train.data$time$Year %in% yrs.overlap),]  
  
  
  # -------------------------------------------
  # Loop through the variables
  # -------------------------------------------
  print("")
  print("Debiasing Meteorology")
  pb <- txtProgressBar(min=0, max=length(vars.debias)*n.ens, style=3)
  pb.ind=1
  for(v in vars.debias){
    # -------------
    # If we're dealing with precip, lets keep the training data handy &
    # calculate the number of rainless time periods (days) in each year to 
    # make sure we don't get a constant drizzle
    # -------------
    if(v=="precipitation_flux"){
      # rain.train <- met.bias[met.bias$dataset==dat.train.orig,]
      rainless <- vector()
      for(y in min(train.data$time$Year):max(train.data$time$Year)){
        for(i in 1:ncol(train.data$precipitation_flux)){
          rainless <- c(rainless, length(which(train.data$time$Year==y & train.data$precipitation_flux[,i]==0)))
        }
      }
      
      # Hard-coding in some sort of max for precipitaiton
      rain.max <- max(train.data$precipitation_flux)*1.5
    }
    # -------------
    
    # -------------
    # Set up the datasets for training and prediction
    # -------------
    # -----
    # 1. Grab the training data -- this will be called "Y" in our bias correction equations
    #     -- preserving the different simulations so we can look at a distribution of potential values
    #     -- This will get aggregated right off the bat so we so we're looking at the climatic means 
    #        for the first part of bias-correction
    # -----
    met.train <- data.frame(year=train.data$time$Year,
                            doy=train.data$time$DOY,
                            Y=stack(data.frame(train.data[[v]][,ens.train]))[,1],
                            ind=rep(paste0("X", 1:n.ens), each=nrow(train.data[[v]]))
                            )

    # For precip, we want to adjust the total annual precipitation, and then calculate day of year 
    # adjustment & anomaly as fraction of total annual precipitation
    if(v == "precipitation_flux"){
      # Find total annual preciptiation
      precip.ann <- aggregate(met.train$Y, by=met.train[,c("year", "ind")], FUN=sum)
      names(precip.ann)[3] <- "Y.tot"
      
      met.train <- merge(met.train, precip.ann, all=T)
      met.train$Y <- met.train$Y/met.train$Y.tot # Y is now fraction of annual precip in each timestep
    }
    
    # Aggregate to get rid of years so that we can compare climatic means
    dat.clim <- aggregate(met.train$Y, by=met.train[,c("doy", "ind")], FUN=mean)
    names(dat.clim)[3] <- "Y"
    # -----
    
    # -----
    # 2. Pull the raw ("source") data that needs to be bias-corrected -- this will be called "X"
    #    -- this gets aggregated to the climatological mean right off the bat
    # -----
    met.src <- data.frame(year=source.data$time$Year,
                          doy=source.data$time$DOY,
                          X=stack(data.frame(source.data[[v]][,ens.src]))[,1],
                          ind.src=rep(paste0("X", 1:length(ens.src)), each=nrow(source.data[[v]]))
                          )
    
    if(v=="precipitation_flux"){
      src.ann <- aggregate(met.src$X, by=met.src[,c("year", "ind.src")], FUN=sum)
      names(src.ann)[3] <- "X.tot"
      
      met.src <- merge(met.src, src.ann, all.x=T)

      # Putting precip as fraction of the year again
      met.src$X <- met.src$X/met.src$X.tot 
      
    } 
    
    # Aggregate to get rid of years so that we can compare climatic means
    clim.src <- aggregate(met.src[met.src$year %in% yrs.overlap,"X"],
                           by=met.src[met.src$year %in% yrs.overlap,c("doy", "ind.src")],
                           FUN=mean, na.rm=T)
    names(clim.src)[3] <- "X"
    # -----
    
    # -----
    # 3. Merge the training & cource climate data together the two sets of daily means 
    #    -- this ends up pairing each daily climatological mean of the raw data with each simulation from the training data
    # -----
    dat.clim <- merge(dat.clim[,], clim.src, all=T)
    
    if(v=="precipitation_flux"){
      if(pair.anoms==F){
        dat.ann <- precip.ann
        dat.ann$X.tot <- src.ann[src.ann$year %in% yrs.overlap,"X.tot"]
      } else {
        dat.ann <- merge(precip.ann, src.ann[src.ann$year %in% yrs.overlap,], all=T)
      }
    }
    # -----
    
    # -----
    # 4. Pulling the source and training data to model the anomalies
    #    - this includes pulling the covariates from what's already been done
    # -----
    # The training data is already formatted, we just need to copy "Y" (our variable) to "X" as well
    met.train$X <- met.train$Y
      
    # Lets deal with the source data first
    # - Adding in the ensembles to be predicted
    if(length(unique(met.src$ind.src))<n.ens){
      met.src <- merge(met.src, data.frame(ind=paste0("X", 1:n.ens)), all=T)
    } else {
      met.src$ind <- met.src$ind.src
    }
    
    # Adding in the covariates from what's been done:
    for(v.pred in vars.debias[!vars.debias==v]){
      met.train[,v.pred] <- stack(data.frame(train.data[[v.pred]][,ens.train]))[,1]
      
      if(v.pred %in% names(dat.out)){
        met.src[,v.pred] <- stack(data.frame(dat.out[[v.pred]]))[,1]
      } else {
        met.src[,v.pred] <- stack(data.frame(source.data[[v.pred]][,ens.src]))[,1]
      }
    }
    
    # Zero out other predictors we'd like to use, but don't actually have data for or don't 
    # want to rely on
    met.train[,vars.all[!vars.all %in% vars.debias]] <- 0
    met.src  [,vars.all[!vars.all %in% vars.debias]] <- 0
    # -----
    
    # Transforming zero-truncated variables where negative values are not possible (and zero is unlikely)
    # - Tried a couple different things, but the sqaure root transformation seems to be working best
    if(v %in% vars.transform){
      dat.clim[,c("X", "Y")] <- sqrt(dat.clim[,c("X", "Y")])
      met.src$X <- sqrt(met.src$X)
      met.train$X <- sqrt(met.train$X)
      met.train$Y <- sqrt(met.train$Y)
    }
    # -------------
    

    
    # -------------
    # 5. Doing the bias correction by looping through the ensemble members
    #    - This is almost certainly not the most efficient way of doing it, but should fix some 
    #      issues with the prediction phase needing lots of memory for large or long ensembles
    # -------------
    sim.final <- data.frame(array(dim=c(nrow(source.data[[v]]), n.ens)))
    names(sim.final) <- paste0("X", 1:n.ens)
    
    for(ens in 1:n.ens){
      
      ind = paste0("X", ens)
      # ---------
      # Doing the climatological bias correction
      # In all variables except precip, this adjusts the climatological means closest to the splice point
      # -- because precip is relatively stochastic without a clear seasonal pattern, a zero-inflated distribution,  
      #    and low correlation with other met variables, we'll instead model potential low-frequency patterns in
      #    the data that is to be bias-corrected.  In this instance we essentially consider any daily precip to be 
      #    an anomaly
      # ---------
      mod.bias <- mgcv::gam(Y ~ s(doy, k=6) + X, data=dat.clim[dat.clim$ind == ind, ])
      # summary(mod.bias)
      
      # Saving the mean predicted & residuals
      dat.clim[dat.clim$ind == ind, "pred"]  <- predict(mod.bias)
      dat.clim[dat.clim$ind == ind, "resid"] <- resid(mod.bias)
      # summary(dat.clim)
      
      # Storing the model residuals to add in some extra error
      resid.bias <- resid(mod.bias)
      
      # # Checking the residuals to see if we can assume normality
      # plot(resid ~ pred, data=dat.clim); abline(h=0, col="red")
      # plot(resid ~ doy, data=dat.clim); abline(h=0, col="red")
      # hist(dat.clim$resid)
      met.src  [met.src  $ind == ind, "pred"] <- predict(mod.bias, newdata=met.src  [met.src  $ind == ind, ])
      met.train[met.train$ind == ind, "pred"] <- predict(mod.bias, newdata=met.train[met.train$ind == ind, ])
      
      # For Precip we need to bias-correct the total annual preciptiation + seasonal distribution
      if(v == "precipitation_flux"){
        mod.ann <- lm(Y.tot ~ X.tot , data=dat.ann[dat.ann$ind==ind,])
        # summary(mod.ann)
        
        dat.ann[dat.ann$ind==ind,"pred.ann"] <- predict(mod.ann)
        dat.ann[dat.ann$ind==ind,"resid.ann"] <- resid(mod.ann)
        
        met.src[met.src$ind==ind,"pred.ann"] <- predict(mod.ann, newdata=met.src[met.src$ind==ind,])
      }
      # ---------
      
      # ---------
      # Modeling the anomalies
      # In most cases, this is the deviation of each observation from the climatic mean for that day (estimated using a smoother)
      #  -- This is done to adjust for differences in the anomaly distributions between data products as well as adjust for seasonal
      #     biases in products such as the GCMs (which show exaggerated seasonal trends relative to CRUNCEP & LDAS products)
      #  -- Again, precipitation_flux is handled differently because to get distributions right, we consider any precipitation event to be anomalous
      #     -- One big challenge in precip was that the GCMs essentially had a summer monsoon season, even for Harvard, which is totally bogus
      #        and would cause major problems with snow effects
      # ---------

      # We want to look at anomalies relative to the raw expected seasonal pattern, so we need to fit training and data to be debiased separately
      anom.train <- mgcv::gam(X ~ s(doy, k=6) , data=met.train[met.train$ind==ind,]) 
      anom.src   <- mgcv::gam(X ~ s(doy, k=6) , data=met.src[met.src$ind==ind & met.src$year %in% yrs.overlap,])
      
      met.train[met.train$ind==ind,"anom.train"] <- resid(anom.train)
      met.src[met.src$ind==ind, "anom.raw"] <- met.src[met.src$ind==ind, "X"] - predict(anom.src, newdata=met.src[met.src$ind==ind, ])
      # par(mfrow=c(2,1))
      # plot(anom.train~doy, data=met.train)
      # plot(anom.raw~doy, data=met.src[met.src$year %in% yrs.overlap,])
      # par(mfrow=c(1,1))
      
      
      # Modeling the anomalies of the other predictors 
      #  -- note: the downscaling & bias-correction of precip should have removed the monsoonal trend if there is no empirical basis for it
      #     so this should be pretty straight-forward now
      for(j in vars.all[vars.all!=v]){
        met.train[met.train$ind==ind, "Q"] <- met.train[met.train$ind==ind,j]
        met.src[met.src$ind==ind, "Q"] <- met.src[met.src$ind==ind,j]
        
        # Generating the predicted seasonal cycle for each variable
        anom.train2 <- mgcv::gam(Q ~ s(doy, k=6), data=met.train[met.train$ind==ind,]) 
        anom.src2   <- mgcv::gam(Q ~ s(doy, k=6), data=met.src[met.src$year %in% yrs.overlap & met.src$ind==ind,]) 
        
        met.train[met.train$ind==ind, paste0(j, ".anom")] <- resid(anom.train2)
        met.src[met.src$ind==ind, paste0(j, ".anom")] <- met.src[met.src$ind==ind,"Q"] - predict(anom.src2, newdata=met.src[met.src$ind==ind,])
        
        rm(anom.train2, anom.src2)
      }
      
      # CRUNCEP has a few variables that assume a constant pattern from 1901-1950; 
      # so we don't want to use their anomaly as a predictor otherwise we will perpetuate that less than ideal situation
      if(CRUNCEP==T & v %in% c("surface_downwelling_longwave_flux_in_air", "air_pressure", "wind_speed")) met.src$anom.raw <- 0
      
      # Actually Modeling the anomalies
      #  -- If we have empirical data, we can pair the anomalies to find a way to bias-correct those
      #  -- If one of our datasets is a GCM, the patterns observed are just what underly the climate signal and no actual
      #     event is "real".  In this case we just want to leverage use the covariance our other met drivers to try and get
      #     the right distribution of anomalies
      if(pair.anoms==TRUE){ 
        # if it's empirical we can, pair the anomalies for best estimation
        # Note: Pull the covariates from the training data to get any uncertainty &/or try to correct covariances
        #        -- this makes it mroe consistent with the GCM calculations
        dat.anom <- merge(met.src  [met.src$ind==ind & met.src$year %in% yrs.overlap, c("year", "doy", "ind", "X", "anom.raw")], 
                          met.train[met.train$ind==ind,c("year", "doy", "anom.train", "ind", vars.all[vars.all!=v], paste0(vars.all[vars.all!=v], ".anom"))])
        
        k=round(length(unique(met.src$year))/50,0)
        k=max(k, 4) # we can't have less than 4 knots
        
        # plot(anom.train ~ anom.raw, data=dat.anom)
        # abline(a=0, b=1, col="blue")
        # abline(lm(anom.train ~ anom.raw, data=dat.anom), col="red", lty="dashed")
        
        # Modeling in the predicted value from mod.bias
        dat.anom$pred <- predict(mod.bias, newdata=dat.anom)
        
        if (v %in% c("air_temperature", "air_temperature_maximum", "air_temperature_minimum")){
          # ** We want to make sure we do these first **
          # These are the variables that have quasi-observed values for their whole time period, 
          # so we can use the the seasonsal trend, and the observed anaomalies
          # Note: because we can directly model the anomalies, the inherent long-term trend should be preserved
          mod.anom <- mgcv::gam(anom.train ~ s(doy, k=6) + anom.raw -1, data=dat.anom)
        } else if(v %in% c("surface_downwelling_shortwave_flux_in_air", "specific_humidity")){
          # CRUNCEP surface_downwelling_shortwave_flux_in_air and specific_humidity have been vary hard to fit to NLDAS because it has a different variance for some reason, 
          # and the only way I've been able to fix it is to model the temporal pattern seen in the dataset based on 
          # its own anomalies (not ideal, but it works)
          mod.anom <- mgcv::gam(anom.raw ~ s(doy, k=6) + s(year, k=k) + air_temperature_maximum.anom*air_temperature_minimum.anom -1 , data=met.src[met.src$ind==ind,])
        } else if(v=="precipitation_flux"){
          # Precip is really only different from the others in that I deliberately chose a more rigid seasonal pattern and we need to force the intercept
          # through 0 so we can try and reduce the likelihood of evenly distributed precipitation events
          # k=round(length(met.src$year)/(25*366),0)
          # k=max(k, 4) # we can't have less than 4 knots
          
          # mod.anom <- mgcv::gam(anom.raw ~ s(year, k=k) + (air_temperature_maximum.anom + air_temperature_minimum.anom + surface_downwelling_shortwave_flux_in_air.anom + surface_downwelling_longwave_flux_in_air.anom + specific_humidity.anom) -1, data=met.src[met.src$ind==ind,])
          mod.anom <- mgcv::gam(anom.train ~ s(doy, k=6) + (air_temperature_maximum.anom + air_temperature_minimum.anom + surface_downwelling_shortwave_flux_in_air.anom + surface_downwelling_longwave_flux_in_air.anom + specific_humidity.anom) -1, data=met.train[met.train$ind==ind,])
        } else if(v %in% c("wind_speed", "air_pressure", "surface_downwelling_longwave_flux_in_air")) {
          # These variables are constant in CRU pre-1950.  
          # This means that we can not use information about the long term trend OR the actual annomalies 
          # -- they must be inferred from the other met we have
          mod.anom <- mgcv::gam(anom.train ~ s(doy, k=6) + (air_temperature_minimum.anom*air_temperature_maximum.anom + surface_downwelling_shortwave_flux_in_air.anom + specific_humidity.anom) -1, data=met.train[met.train$ind==ind,])
        }      
      } else { 
        # If we're dealing with non-empirical datasets, we can't pair anomalies to come up with a direct adjustment 
        # In this case we have 2 options:
        #   1) If we've already done at least one variable, we can leverage the covariance of the met drivers we've already downscaled 
        #      to come up with a relationship that we an use to predict the new set of anomalies
        #   2) If we don't have any other variables to leverage (i.e. this is our first met variable), we incorporate both the seasonal
        #      trend (doy spline) and potential low-frequency trends in the data (year spline)
        k=round(length(unique(met.src$year))/50,0)
        k=max(k, 4) # we can't have less than 4 knots
        
        # vars.debias <- c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", "specific_humidity", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "surface_downwelling_longwave_flux_in_air", "wind_speed")
        # Vars that are at daily and we just need to adjust the variance
        # We have some other anomaly to use! that helps a lot. -- use that to try and get low-frequency trends in the past
        if(v %in% c("air_temperature_maximum", "air_temperature_minimum")){
          # If we haven't already done another met product, our best shot is to just model the existing variance 
          # and preserve as much of the low-frequency cylce as possible
          mod.anom <- mgcv::gam(anom.raw ~ s(year, k=k) -1, data=met.src[met.src$ind==ind,]) 
        } else if(v=="precipitation_flux"){ 
          # If we're working with precipitation_flux, need to make the intercept 0 so that we have plenty of days with little/no rain
          mod.anom <- mgcv::gam(anom.raw ~  s(year, k=k) + (air_temperature_maximum.anom*air_temperature_minimum.anom + surface_downwelling_shortwave_flux_in_air.anom + surface_downwelling_longwave_flux_in_air.anom + specific_humidity.anom) -1, data=met.src[met.src$ind==ind,])  
        } else if(v %in% c("surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air")){
          # See if we have some other anomaly that we can use to get the anomaly covariance & temporal trends right
          # This relies on the assumption that the low-frequency trends are in proportion to the other met variables
          # (this doesn't seem unreasonable, but that doesn't mean it's right)
          mod.anom <- mgcv::gam(anom.train ~ s(doy, k=4) + (air_temperature_maximum.anom*air_temperature_minimum.anom + specific_humidity.anom + air_pressure.anom + wind_speed.anom) -1, data=met.train[met.train$ind==ind,])
        } else {
          # If we have some info
          # THis should be specific_humidity, air_pressure, wind_speed
          mod.anom <- mgcv::gam(anom.raw ~ s(doy, k=6) + s(year, k=k) + (air_temperature_maximum.anom*air_temperature_minimum.anom)-1, data=met.src[met.src$ind==ind,])
        }
      }
      # summary(mod.anom)
      # plot(mod.anom, pages=1)
      
      resid.anom <- resid(mod.anom)
      # ---------
      
      # --------
      # Predicting a bunch of potential posteriors over the full dataset
      # --------
      # Get the model coefficients
      coef.gam <- coef(mod.bias)
      coef.anom <- coef(mod.anom)
      if(v == "precipitation_flux") coef.ann <- coef(mod.ann)
      
      # Generate a random distribution of betas using the covariance matrix
      Rbeta <- MASS::mvrnorm(n=n.ens, coef(mod.bias), vcov(mod.bias))
      Rbeta.anom <- MASS::mvrnorm(n=n.ens, coef(mod.anom), vcov(mod.anom))
      if(v == "precipitation_flux") Rbeta.ann <- MASS::mvrnorm(n=n.ens, coef(mod.ann), vcov(mod.ann))
      
      # Create the prediction matrix
      Xp <- predict(mod.bias, newdata=met.src[met.src$ind==ind,], type="lpmatrix")
      Xp.anom <- predict(mod.anom, newdata=met.src[met.src$ind==ind,], type="lpmatrix")
      if(v == "precipitation_flux"){
        # Linear models have a bit of a difference in how we get the info out
        # Xp.ann <- predict(mod.ann, newdata=met.src, type="lpmatrix")
        
        met.src[met.src$ind==ind,"Y.tot"] <- met.src[met.src$ind==ind,"pred.ann"]
        mod.terms <- terms(mod.ann)
        m <- model.frame(mod.terms, met.src[met.src$ind==ind,], xlev=mod.ann$xlevels)
        Xp.ann <- model.matrix(mod.terms, m, constrasts.arg <- mod.ann$contrasts)
      } 
      
      # -----
      # Simulate predicted met variables & add in some residual error
      # NOTE: Here we're assuming normal distribution of the errors, which looked pretty valid
      #       in the tests I ran when doing the intial code development
      # We do have a couple options for how to add the residual error/uncertainty back in
      # -----
      # Options for adding in residual error
      # # Option 1: Adding a constant error per time series
      #    -- This is currently used for the climatological bias-correction because we're going to assume
      #       that we've biased the mean offset in the climatology (the seasonal bias is encorporated in the 
      #       spline estimation)
      #    -- Note: Precipitation doesn't get residual error added here because that sort of bias is funneled into
      #             the anomaly model.  The error in the Rbetas should adequately represent the uncertainty in the 
      #             low-frequency trends in the data
      # # Option 2: Adding a random error to each observation
      #    -- This is used for the anomalies because they are by definition stochastic, highly unpredictable
      #    -- Note: this option currently ignores potential autocorrelation in anomalies (i.e. if 1 Jan was 
      #             unseasonably warm, odds are that the days around it weren't record-breaking cold)
      #              -- I'm rolling with this for now and will smooth some of these over in the downscaling to
      #                 subdaily data
      # # Option 3: explicitly modeling the errors in some way
      #    -- I tried this and it made my brain hurt
      # -----
      # Default option: no residual error; all error from the downscaling parameters
      sim1a <- Xp %*% t(Rbeta)  # Seasonal Climate component with uncertainty
      sim1b <- Xp.anom %*% t(Rbeta.anom) # Weather component with uncertainty
      if(v == "precipitation_flux") sim1c <- Xp.ann %*% t(Rbeta.ann) # Mean annual precip uncertainty
      
      # If we're dealing with the temperatures where there's basically no anomaly, 
      # we'll get the uncertainty subtract the multi-decadal trend out of the anomalies; not a perfect solution, but it will increase the variability
      if(pair.anoms==F & (v %in% c("air_temperature_maximum", "air_temperature_minimum"))){
        sim1b.norm <- apply(sim1b, 1, mean) 
        sim1b <- sim1b - sim1b.norm + as.vector(met.src[met.src$ind==ind,"anom.raw"]) # Get the range around that medium-frequency trend 
      }
      
      
      
      # Option 1: Adding a constant error per time series for the cliamte correction 
      #             (otherwise we're just doubling anomalies)
      # sim1a <- sweep(sim1a, 2, rnorm(n, mean(resid.bias), sd(resid.bias)), FUN="+")
      # if(v!="precipitation_flux") sim1a <- sweep(sim1a, 2, rnorm(n, mean(resid.bias), sd(resid.bias)), FUN="+") # Only apply if not working with precipitation_flux
      # sim1b <- sweep(sim1b, 2, rnorm(n, mean(resid.anom), sd(resid.anom)), FUN="+")
      
      # # # Option 2: Adding a random error to each observation (anomaly error)
      # if(v!="precipitation_flux") sim1a <- sim1a + rnorm(length(sim1a), mean(resid.bias), sd(resid.bias))
      # sim1b <- sim1b + rnorm(length(sim1b), mean(resid.anom), sd(resid.anom))
      
      # # Option 3: explicitly modeling the errors in some way
      # -----
      
      # Adding climate and anomaly together
      sim1 <- sim1a + sim1b # climate + weather = met driver!!
      
      # If we're dealing with precip, transform proportions of rain back to actual precip
      if(v == "precipitation_flux"){ 
        sim1 <- sim1*sim1c
        # met.src$X <- met.src$X*met.src$X.tot
        # met.src$anom.raw <- met.src$anom.raw*met.src$X.tot
      }
      
      # Un-transform variables where we encounter zero-truncation issues
      # NOTE: Need to do this *before* we sum the components!! 
      #if(v %in% vars.transform){
      #  sim1 <- sim1^2
      #  # met.src[met.src$ind==ind,"X"] <- met.src[met.src$ind==ind,"X"]^2
      #} 
      
      
      # For preciptiation, we need to make sure we don't have constant drizzel and have 
      # at least some dry days.  To deal with this, I make the assumption that there hasn't
      # been a trend in number of rainless days over the past 1000 years and use the mean & 
      # sd of rainless days in the training data to randomly distribute the rain in the past
      if(v=="precipitation_flux"){
        for(j in 1:ncol(sim1)){
          for(y in min(met.src[met.src$ind==ind, "year"]):max(met.src[met.src$ind==ind, "year"])){
            # Figure out which rows belong to this particular year
            rows.yr <- which(met.src[met.src$ind==ind, "year"]==y)
            
            # Before adjusting rainless days, make sure we get rid of our negative days first
            dry <- rows.yr[which(sim1[rows.yr,j] < 0)]
            while(length(dry)>0){ # until we have our water year balanced
              for(r in 1:length(dry)){
                # Pick a year with some rain and take the rain from it
                #  -- this *should* make sure we don't get an infinite loop by making one rainless day have negative rain
                row.steal <- sample(rows.yr[which(sim1[rows.yr,j]>0)], 1) # The row we're stealing precip out of to balance the budget
                sim1[row.steal,j] <- sim1[row.steal,j] + sim1[dry[r],j]
                sim1[dry[r],j] <- 0
              }
              dry <- rows.yr[which(sim1[rows.yr,j] < 0)] # update our dry days
            }
            
            n.now <- round(rnorm(1, mean(rainless, na.rm=T), sd(rainless, na.rm=T)), 0) 
            cutoff <- quantile(sim1[rows.yr, j], n.now/366, na.rm=T)
            
            # Figure out which days are currently below our cutoff and randomly distribute 
            # their precip to days that are not below the cutoff (this causes a more bi-modal 
            # distribution hwere dry days get drier), but other options ended up with either 
            # too few rainless days because of only slight redistribution (r+1) or buildup 
            # towards the end of the year (random day that hasn't happened)
            dry <- rows.yr[which(sim1[rows.yr,j] < cutoff)]
            
            # Figure out where to put the extra rain; allow replacement for good measure
            wet <- sample(rows.yr[!rows.yr %in% dry], length(dry), replace=T)
            
            # Go through and randomly redistribute the precipitation to days we're not designating as rainless
            # Note, if we don't loop through, we might lose some of our precip
            for(r in 1:length(dry)){
              sim1[wet[r],j] <- sim1[wet[r],j] + sim1[dry[r],j]
              sim1[dry[r],j] <- 0
            }
          }
        }
      }
      
      # Randomly pick one from this meta-ensemble to save
      # this *should* be propogating uncertainty because we have the ind effects in all of the models and we're randomly adding as we go
      sim.final[,ens] <- sim1[,sample(1:n.ens,1)]
      
      setTxtProgressBar(pb, pb.ind)
      pb.ind <- pb.ind+1
      
      rm(mod.bias, anom.train, anom.src, mod.anom, Xp, Xp.anom, sim1, sim1a, sim1b)
    }

    if(v == "precipitation_flux"){ 
      # sim1 <- sim1*sim1c
      met.src$X <- met.src$X*met.src$X.tot
      met.src$anom.raw <- met.src$anom.raw*met.src$X.tot
    }
    
    if(v %in% vars.transform){
      sim.final <- sim.final^2
      dat.clim[,c("X", "Y")] <- (dat.clim[,c("X", "Y")]^2)
      met.src$X <- (met.src$X)^2
      met.train$X <- (met.train$X)^2
      met.train$Y <- (met.train$Y)^2
    }
    
    # Store the output in our dat.out
    dat.out[[v]] <- sim.final
    # -------------
    
    # -------------
    # Save some diagnostic graphs if useful
    # -------------
    if(save.diagnostics==TRUE){
      dir.create(path.diagnostics, recursive=T, showWarnings=F)
      
      dat.pred <- source.data$time
      dat.pred$obs  <- apply(source.data[[v]], 1, mean, na.rm=T)
      dat.pred$mean <- apply(dat.out[[v]], 1, mean, na.rm=T)
      dat.pred$lwr  <- apply(dat.out[[v]], 1, quantile, 0.025, na.rm=T)
      dat.pred$upr  <- apply(dat.out[[v]], 1, quantile, 0.975, na.rm=T)
      
      # Plotting the observed and the bias-corrected 95% CI
      png(file.path(path.diagnostics, paste(ens.name, v, "day.png", sep="_")))
      print(
        ggplot(data=dat.pred[dat.pred$Year>=mean(dat.pred$Year)-1 & dat.pred$Year<=mean(dat.pred$Year)+1,]) +
          geom_ribbon(aes(x=Date, ymin=lwr, ymax=upr), fill="red", alpha=0.5) +
          geom_line(aes(x=Date, y=mean), color="red", size=0.5) +
          geom_line(aes(x=Date, y=obs), color='black', size=0.5) +
          ggtitle(paste0(v, " - ensemble mean & 95% CI (daily slice)")) +
          theme_bw()
      )
      dev.off()
      
      # Plotting a few random series to get an idea for what an individual pattern looks liek
      stack.sims <- stack(data.frame(dat.out[[v]][,sample(1:n.ens, min(3, n.ens))]))
      stack.sims[,c("Year", "DOY", "Date")] <- dat.pred[,c("Year", "DOY", "Date")]
      
      png(file.path(path.diagnostics, paste(ens.name, v, "day2.png", sep="_")))
      print(
        ggplot(data=stack.sims[stack.sims$Year>=mean(stack.sims$Year)-2 & stack.sims$Year<=mean(stack.sims$Year)+2,]) +
          geom_line(aes(x=Date, y=values, color=ind), size=0.2, alpha=0.8) +
          ggtitle(paste0(v, " - example ensemble members (daily slice)")) +
          theme_bw()
      )
      dev.off()
      
      # Looking tat the annual means over the whole time series to make sure we're getting decent interannual variability
      dat.yr <- aggregate(dat.pred[,c("obs", "mean", "lwr", "upr")],
                          by=list(dat.pred$Year),
                          FUN=mean)
      names(dat.yr)[1] <- "Year"
      
      png(file.path(path.diagnostics, paste(ens.name, v, "annual.png", sep="_")))
      print(
        ggplot(data=dat.yr[,]) +
          geom_ribbon(aes(x=Year, ymin=lwr, ymax=upr), fill="red", alpha=0.5) +
          geom_line(aes(x=Year, y=mean), color="red", size=0.5) +
          geom_line(aes(x=Year, y=obs), color='black', size=0.5) +
          ggtitle(paste0(v, " - annual mean time series")) +
          theme_bw()
      )
      dev.off()
      
    }
    # -------------
    
  } # End looping through variables
  # -------------------------------------------
  
  
  # Save the output
  nc.info <- data.frame(CF.name  = c("air_temperature", "air_temperature_minimum", "air_temperature_maximum", "precipitation_flux", 
                                     "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", 
                                     "air_pressure", "specific_humidity", "wind_speed"), 
                        longname = c("2 meter air temperature", "2 meter minimum air temperature", "2 meter maximum air temperature", 
                                    "cumulative precipitation (water equivalent)", "incident (downwelling) showtwave radiation", 
                                     "incident (downwelling) longwave radiation", "air pressure at the surface", 
                                     "Specific humidity measured at the lowest level of the atmosphere", 
                                     "wind_speed speed"), 
                        units    = c("K", "K", "K", "kg m-2 s-1", "W m-2", "W m-2", "Pa", "kg kg-1", "m s-1")
                        )
  
  # Define our lat/lon dims since those will be constant
  dim.lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
  dim.lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
  
  print("")
  print("Saving Ensemble")
  pb <- txtProgressBar(min=0, max=length(yrs.save)*n.ens, style=3)
  pb.ind=1
  for(yr in yrs.save){
    # Doing some row/time indexing
    rows.yr <- which(dat.out$time$Year==yr)
    nday <- ifelse(leap_year(yr), 366, 365)
    
    # Finish defining our time variables (same for all ensemble members)
    dim.time <- ncdf4::ncdim_def(name='time', units="sec", vals=seq(1*24*360, (nday+1-1/24)*24*360, length.out=length(rows.yr)), create_dimvar=TRUE, unlim=TRUE)
    nc.dim=list(dim.lat,dim.lon,dim.time)
    
    # Setting up variables and dimensions
    var.list = list()
    dat.list = list()
    
    for(j in 1:length(vars.debias)){
      var.list[[j]] = ncdf4::ncvar_def(name=vars.debias[j], 
                                       units=as.character(nc.info[nc.info$CF.name==vars.debias[j], "units"]), 
                                       longname=as.character(nc.info[nc.info$CF.name==vars.debias[j], "longname"]),
                                       dim=nc.dim, missval=-999, verbose=verbose)
    }
    names(var.list) <- vars.debias
    
    # Loop through & write each ensemble member
    for(i in 1:n.ens){
      # Setting up file structure
      ens.path <- file.path(outfolder, paste(ens.name, ens.mems[i], sep="_"))
      dir.create(ens.path, recursive=T, showWarnings=F)
      loc.file <- file.path(ens.path, paste(ens.name, ens.mems[i], str_pad(yr, width=4, side="left",  pad="0"), "nc", sep = "."))
      
      for(j in 1:length(vars.debias)){
        dat.list[[j]] = array(dat.out[[vars.debias[j]]][rows.yr,i], dim=c(length(lat.in), length(lon.in), length(rows.yr))) # Go ahead and make the arrays
      }
      names(dat.list) <- vars.debias
      
      ## put data in new file
      loc <- ncdf4::nc_create(filename=loc.file, vars=var.list, verbose=verbose)
      for(j in 1:length(vars.debias)){
        ncdf4::ncvar_put(nc=loc, varid=as.character(vars.debias[j]), vals=dat.list[[j]])
      }
      ncdf4::nc_close(loc)
      
      setTxtProgressBar(pb, pb.ind)
      pb.ind <- pb.ind+1
    } # End ensemble member loop
  } # End year loop
  print("")
  print("Debiasing Completed!")
} # END FUNCTION!
