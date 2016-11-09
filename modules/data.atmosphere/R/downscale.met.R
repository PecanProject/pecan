##' downscale.met takes model data and flux data from the same site and temporally 
##'    downscales the modeled data based on statistics from the flux data.
##' @name downscale.met
##' @title downscale.met
##' @export
##' @param outfolder
##' @param modelfile - the modeled dataset in NC format. Could be CMIP5 dataset (see download.GFDL or download.MACA) or other modeled dataset 
##' @param fluxfile - the observed dataset in NC format i.e. Flux Tower dataset (see download.Fluxnet2015 or download.Ameriflux) 
##' @param method - downscaling method. Default will be 'normal', which performs a random normal distribution on the 
##'                 spline interpolated points using the model value as the mean and the standard deviation of the upscaled 
##'                 flux data +/- window_days from the value 
##' @param site.id
##' @param ensemble_members - a numeric value that selects how many ensemble members you would like to run 
##' @param wd - window days, a numeric value setting the size of the window for standard deviation calculations
##' @param reso - resolution in HOURS that you would like the modeled data to be downscaled to, default option assumes 6 hourly, i.e. a vector is created 
##'                     with values every 6 hours. Half hour is .5
##' @param swdn_method - Downscaling downwelling shortwave flux in air, options listed below.  
##'                      "sine" -  fits a sine curve with an amplitude of the model daily maximum which is derived from the model 
##'                                daily average value. The length of the sine wave from 0 to pi is equal to the daylight length. 
##'                      "Waichler" - from Waichler and Wigtosa 2003, calculates this based on downscaled min/max temps and precipitation. 
##'                      "spline" - fits a spline interpolation between the modeled values. 
##'                      
##' @param utc_diff - difference from UTC time at lat/lon. For example, a flux tower in EST would be UTC-5, which makes utc_diff = -5
##'                   This is used when swdn_method = "sine" to ensure a proper diurnal pattern 
##' @author James Simkins
downscale.met <- function(outfolder, modelfile, fluxfile, site_id, method='normal',ensemble_members = 10, wd = 20,
                          reso = 6, swdn_method = "sine", utc_diff = -6, overwrite=FALSE, verbose=FALSE, ...){  

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  sub_str <- substrRight(modelfile, 7)
  year <- substr(sub_str,1, 4)
  year = as.numeric(year)
  eph_year <- year
  mod_name <- substr(modelfile,1,nchar(modelfile)-8)
  # Variable names 
  var <- data.frame(CF.name = c("air_temperature","air_temperature_max","air_temperature_min","surface_downwelling_longwave_flux_in_air",
                                "air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind",
                               "specific_humidity","precipitation_flux"),
                   units = c('Kelvin','Kelvin','Kelvin',"W/m2","Pascal","W/m2","m/s","m/s","g/g","kg/m2/s")
  )
  # Reading in the flux data
    flux <- list()
    tem <- ncdf4::nc_open(fluxfile)
    dim <- tem$dim
    for (j in seq_along(var$CF.name)){
      if (exists(as.character(var$CF.name[j]),tem$var) == FALSE){
        flux[[j]] = NA}
      else {
        flux[[j]] <- ncdf4::ncvar_get(tem,as.character(var$CF.name[j]))}
    }
    lat_flux <- as.numeric(ncdf4::ncvar_get(tem,"latitude"))
    lon_flux <- as.numeric(ncdf4::ncvar_get(tem,"longitude"))
    ncdf4::nc_close(tem)
    
    flux <- data.frame(flux)
    colnames(flux) <- var$CF.name
    flux$air_temperature_max <- flux$air_temperature
    flux$air_temperature_min <- flux$air_temperature
  
  # Reading in the model data
    model <- list()
    tow <- ncdf4::nc_open(modelfile)
    for (j in seq_along(var$CF.name)){
      if (exists(as.character(var$CF.name[j]),tow$var) == FALSE){
        model[[j]] = NA}
      else {
        model[[j]] <- ncdf4::ncvar_get(tow,as.character(var$CF.name[j]))}
      }
    ncdf4::nc_close(tow)
    
    model <- data.frame(model)
    colnames(model) <- var$CF.name
  
  #Going to upscale the observations to the desired downscale resolution so we can take the standard deviatiion of the residuals
    # If a model doesn't have leap days, we need to remove them to ensure equal lengths
    if (lubridate::leap_year(year) == TRUE){
      if (length(model$air_temperature)%%366 > 0){
        if (length(flux$air_temperature)%%366 == 0){
          flux = flux[1:365*(nrow(flux)/366)]}
          eph_year = 2006}} #chose an non-leap year to use for daylength calculations if we don't have the 
    if (lubridate::leap_year(eph_year) == TRUE){
      sp = 366}
    if (lubridate::leap_year(eph_year) == FALSE){
      sp = 365}
    reso_len <- sp*24/reso #nrow(downscaled.met)
    
    step <- length(flux$air_temperature)/reso_len
    upscale_flux <- data.frame()
    for (n in 1:length(var$CF.name)){
      for (x in 1:reso_len){
        upscale_flux[x,n] <- mean(flux[(x*step-step+1):(x*step),n])}
    }
    colnames(upscale_flux) <- var$CF.name
    for (x in 1:reso_len){
      upscale_flux$air_temperature_max[x] <- max(flux$air_temperature[(x*step-step+1):(x*step)])
      upscale_flux$air_temperature_min[x] <- min(flux$air_temperature[(x*step-step+1):(x*step)])}
    
    # Now we start a for loop for the ensemble members and begin downscaling. If method = "normal", a random normal distribution
    # is used to downscale as so; (mean = value of modeled data) (sd = +/- window_days of upscale_flux data at the same time intervals)
    # Future work is to implement more methods for downscaling
    for (e in seq_along(1:ensemble_members)){
      if (method == "normal"){
      div = nrow(upscale_flux)/nrow(model) #tells us how many values need to be generated (via downscaling) from each model value
      sd_step = nrow(upscale_flux)/sp #allows us to step through each window at specific times
      
      # Temperature 
      # For temperature, we create a random normal distribution with the mean of the model value and the standard deviation
      # of the upscaled flux value within the window, and step through that window sequentially so that we are only using
      # the standard devaition of values recorded at the same time as the value we are generating.
      randtemp <- vector()
      for (x in seq_along(model$air_temperature)){
        four <- vector()
        for (n in seq_along(1:div)){
          lowday <- x*div-wd*sd_step+n
          highday <- x*div+wd*sd_step+n
          if (lowday < 0){lowday = x+n
          highday = x+n+wd*sd_step+n}
          if (highday > reso_len){
            highday = reso_len
            lowday = x*div-wd*sd_step+n}
          four[n] <- rnorm(1, mean = model$air_temperature[x],
                            sd = sd(upscale_flux$air_temperature[seq(from=lowday,to=highday,by=sd_step)]))}
        randtemp <- append(randtemp,four)
      }
      
      # Air Temperature Max and Min
      temp_max <- vector()
      if (all(is.na(model$air_temperature_max)) == TRUE){
        temp_max <- rep(NA,reso_len)}
      if (all(is.na(model$air_temperature_max)) == FALSE){
        for (x in seq_along(model$air_temperature_max)){
          four <- vector()
          for (n in seq_along(1:div)){
            lowday <- x*div-wd*sd_step+n
            highday <- x*div+wd*sd_step+n
            if (lowday < 0){lowday = x+n
            highday = x+n+wd*sd_step+n}
            if (highday > reso_len){
              highday = reso_len
              lowday = x*div-wd*sd_step+n}
            four[n] <- rnorm(1, mean = model$air_temperature_max[x],
                             sd = sd(upscale_flux$air_temperature_max[seq(from=lowday,to=highday,by=sd_step)]))}
        temp_max <- append(temp_max,four)}
      }
      
      temp_min <- vector()
      if (all(is.na(model$air_temperature_min)) == TRUE){
        temp_min <- rep(NA,reso_len)}
      if (all(is.na(model$air_temperature_min)) == FALSE){
        for (x in seq_along(model$air_temperature_min)){
          four <- vector()
          for (n in seq_along(1:div)){
            lowday <- x*div-wd*sd_step+n
            highday <- x*div+wd*sd_step+n
            if (lowday < 0){lowday = x+n
            highday = x+n+wd*sd_step+n}
            if (highday > reso_len){
              highday = reso_len
              lowday = x*div-wd*sd_step+n}
            four[n] <- rnorm(1, mean = model$air_temperature_min[x],
                             sd = sd(upscale_flux$air_temperature_min[seq(from=lowday,to=highday,by=sd_step)]))}
          temp_min <- append(temp_min,four)}
      }
      
      # Precipitation_flux
      # this takes the daily total of precipitation and uses that as a total possible amount of precip. 
      # It randomly distributes the values of precipitation
      rand_vect_cont <- function(N, M, sd = 1) {
        vec <- rnorm(N, M/N, sd)
        vec / sum(vec) * M
      }
      precip <- vector()
      for (x in seq_along(model$precipitation_flux)){
        lowday <- (x-wd)*div
        highday <- (x+wd)*div
        if (lowday < 0){lowday = 0}
        if (highday > reso_len){highday = reso_len}
        four <- vector()
        four <- rand_vect_cont(div,model$precipitation_flux[x], 
                               sd = sd(upscale_flux$precipitation_flux[lowday:highday]))
        four[four<0] = 0
        precip <- append(precip,four)
      }
      
      # Specific Humidity
      spechum <- vector()
      for (x in seq_along(model$specific_humidity)){
        lowday <- (x-wd)*div
        highday <- (x+wd)*div
        if (lowday < 0){lowday = 0}
        if (highday > reso_len){highday = reso_len}
        four <- vector()
        for (n in seq_along(1:div)){
          four[n] <- rnorm(1, mean = model$specific_humidity[x], 
                          sd = sd(upscale_flux$specific_humidity[lowday:highday]))}
        spechum <- append(spechum,four)
      }
      spechum[spechum < 0] = 0
      
      # Winds
      east <- vector()
      for (x in seq_along(model$eastward_wind)){
        lowday <- (x-wd)*div
        highday <- (x+wd)*div
        if (lowday < 0){lowday = 0}
        if (highday > reso_len){highday = reso_len}
        four <- vector()
        for (n in seq_along(1:div)){
          four[n] <- rnorm(1, mean = model$eastward_wind[x], 
                          sd = sd(upscale_flux$eastward_wind[lowday:highday]))}
        east <- append(east,four)
      }
  
      north <- vector()
      for (x in seq_along(model$northward_wind)){
        lowday <- (x-wd)*div
        highday <- (x+wd)*div
        if (lowday < 0){lowday = 0}
        if (highday > reso_len){highday = reso_len}
        four <- vector()
        for (n in seq_along(1:div)){
          four[n] <- rnorm(1, mean = model$northward_wind[x],
                          sd = sd(upscale_flux$northward_wind[lowday:highday]))}
        north <- append(north,four)
      }
  
      # Downwelling shortwave radiation flux
      # Ephemeris is a function to calculate sunrise/sunset times and daylength for SW calculations in sine method
      ephemeris <- function(lat, lon, date, span=1, tz="UTC") {
        
        lon.lat <- matrix(c(lon, lat), nrow=1)
        
        # using noon gets us around daylight saving time issues
        day <- as.POSIXct(sprintf("%s 12:00:00", date), tz=tz)
        sequence <- seq(from=day, length.out=span , by="days")
        
        sunrise <- maptools::sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
        sunset <- maptools::sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)
        solar_noon <- maptools::solarnoon(lon.lat, sequence, POSIXct.out=TRUE)
        
        data.frame(date=as.Date(sunrise$time),
                   sunrise=as.numeric(format(sunrise$time, "%H%M")),
                   solarnoon=as.numeric(format(solar_noon$time, "%H%M")),
                   sunset=as.numeric(format(sunset$time, "%H%M")),
                   day_length=as.numeric(sunset$time-sunrise$time)) 
        
      }
      
      swmodel <- model$surface_downwelling_shortwave_flux_in_air
      swdn <- vector()
    
      # The sine method produces an hourly sine wave of 
      if (swdn_method == "sine"){
        
        eph <- ephemeris(lat_flux,lon_flux,date = paste0(eph_year,"-01-01 00:00:00"), 
                       span = sp, tz = "UTC")
        day_len <- eph$day_length
        
        # Need to have average daily values for this method, so this upscales the model data to daily resolution if needed
        daily_row = nrow(model)
        daily_step = daily_row/sp
        daily.swdn = vector()
          for (x in 1:sp){
            daily.swdn[x] <- mean(swmodel[(x*daily_step-daily_step+1):(x*daily_step)])}
        
        # creating the sine wave
        for (i in seq_along(daily.swdn)){
          t <- seq(from=pi/day_len[i],to=pi,by=pi/day_len[i])
          wav <- ((daily.swdn[i]*(24/day_len[i]))/0.637)*sin(t)
          
        # swdn = 0 without sunlight
        srs <- eph$sunrise
        hr <- substr(srs[i],1,2)
        hr <-as.numeric(hr)
        hr <- hr+utc_diff
        
        l <- vector()
        for (n in seq_along(1:hr)){
          l[n] = 0}
        for (n in seq_along(wav)){
          l[n+hr] = wav[n]}
        for(n in seq_along(1:(24-(length(wav)+hr)))){
          l[n+hr+length(wav)] = 0}
        
        swdn <- append(swdn,l)}
        
        swflux <- vector()
        sw_step <- length(swdn)/reso_len
        for (x in 1:reso_len){
          swflux[x] <- mean(swdn[(x*sw_step-sw_step+1):(x*sw_step)])}
        swflux[swflux < 0] = 0 
      }
        
      # The spline method uses spline interpolation to connect existing values and downscale
      if (swdn_method == "spline"){
        tem.met = vector()
          for (x in seq(from=0, to=nrow(upscale_flux), by=div)){
            tem.met[x] = swmodel[x/div]}
        
      swflux = vector()
      swflux = zoo::na.spline(tem.met)
      swflux[swflux<0] <- 0
      }
      
      # The Waichler method doesn't need averaged SW flux values, it models SW downwelling flux based on Tmax-Tmin and Precipitation
      # Reference is Waichler and Wigtosa 2003. Our no-precip coefficient is 2 instead of 1 (fits the data better)
      if (swdn_method == "Waichler"){
        inter <- paste0(reso," hour")
        days <- seq(as.POSIXct(paste0(eph_year,"-01-01 00:00:00", tz = "US/Central")), 
                    as.POSIXct(paste0(eph_year,"-12-31 18:00:00", tz = "US/Central")), by=inter)
        
        Z <- Ratmosphere::SZA(days,lat_flux, lon_flux)
        I <- 1000*cos_d(Z)
        m <- vector()
        for (i in seq_along(1:12)){
          m[i] <- Hmisc::monthDays(as.Date(paste0(year,'-',i,'-01')))}
        bmlist <- vector()
  
        Bm <- c(0.2089,0.2857, 0.2689, 0.2137, 0.1925, 0.2209, 0.2527, 0.2495,0.2232, 0.1728, 0.1424, 0.1422)
        for (x in seq_along(Bm)){
          mlen <- list()
          mlen <- rep(Bm[x],m[x]*24/reso)
          bmlist <- append(bmlist,mlen)}
        A <-.73
        C <- .7
        hdry <- vector()
        for (i in seq_along(precip)){
          if (precip[i] > 0){p=.65}
          if (precip[i] == 0){p=2}
          hdry[i] <- A*p*(1-exp(-1*bmlist[i]*((temp_max[i]-temp_min[i])^C)))
        }
        hdry[hdry<0]=0
        swflux <- hdry*I
        swflux[swflux<0]=0
      }
      
      # Longwave Downwelling in air flux
      if (all(is.na(model$surface_downwelling_longwave_flux_in_air)) == TRUE){
        lwflux<-vector()
        lwflux<- rep(NA,reso_len)}
      if (all(is.na(model$surface_downwelling_longwave_flux_in_air)) == FALSE){
      lwflux<- vector()
      for (x in seq_along(model$surface_downwelling_longwave_flux_in_air)){
        lowday <- (x-wd)*div
        highday <- (x+wd)*div
        if (lowday < 0){lowday = 0}
        if (highday > reso_len){highday = reso_len}
        four <- vector()
        for (n in seq_along(1:div)){
          four[n] <- rnorm(1, mean = model$surface_downwelling_longwave_flux_in_air[x], 
                          sd = sd(upscale_flux$surface_downwelling_longwave_flux_in_air[lowday:highday]))}
        lwflux <- append(lwflux,four)}
      }
      
      # Atmospheric Pressure
      if (all(is.na(model$air_pressure)) == TRUE){
        pres<-vector()
        pres<- rep(NA,reso_len)}
      if (all(is.na(model$air_pressure)) == FALSE){
        pres<- vector()
        for (x in seq_along(model$air_pressure)){
          lowday <- (x-wd)*div
          highday <- (x+wd)*div
          if (lowday < 0){lowday = 0}
          if (highday > reso_len){highday = reso_len}
          four <- vector()
          for (n in seq_along(1:div)){
            four[n] <- rnorm(1, mean = model$air_pressure[x], sd = sd(upscale_flux$air_pressure[lowday:highday]))}
          pres <- append(pres,four)}
        }
      
      } #this ends the method = 'normal' downscaling phase

    # Putting all the variables together in a data frame
    downscaled.met <- data.frame(randtemp,temp_max,temp_min,lwflux,pres,swflux,east,north,spechum,precip)
    colnames(downscaled.met) <- var$CF.name

    flux.list <- list()
    lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat_flux, create_dimvar=TRUE)
    lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon_flux, create_dimvar=TRUE)
    time <- ncdf4::ncdim_def(name='time', units="sec", vals=(1:reso_len)*reso*3600, create_dimvar=TRUE, unlim=TRUE)
    dim<-list(lat,lon,time)
    
    for (j in seq_along(var$CF.name)){
    flux.list[[j]] <- ncdf4::ncvar_def(name=as.character(var$CF.name[j]), 
                                units=as.character(var$units[j]), 
                                dim=dim, 
                                missval=-999, 
                                verbose=verbose)}
    
    rows <- 1
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = paste0(mod_name,".dwnsc.ens",e,".",year,sep="."),
                        stringsAsFactors = FALSE)
  

  loc.file <- file.path(outfolder, paste0(mod_name,".dwnsc.ens",e,".",year,".nc"))
  
  loc <- ncdf4::nc_create(filename=loc.file, vars=flux.list, verbose=verbose)
  for(j in seq_along(var$CF.name)){
    ncdf4::ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=downscaled.met[[j]])
  }
  ncdf4::nc_close(loc)
  
  results$file <- loc.file
  results$host <- PEcAn.utils::fqdn()
  results$startdate <- paste0(year,"-01-01 00:00:00", tz = "UTC")
  results$enddate <- paste0(year,"-12-31 23:59:59", tz = "UTC")
  results$mimetype <- 'application/x-netcdf'
  results$formatname <- 'CF Meteorology'
  
  invisible(results)
  
  }
}
