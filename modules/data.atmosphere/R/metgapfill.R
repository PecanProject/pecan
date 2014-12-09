##' Take an Ameriflux NetCDF file
##' Fill missing met values using MDS approach using MPI-BGC REddyProc library
##' Currently 
##' Future version: Choose which variables to gap fill
##' Future version will first downscale and fill with NARR, then REddyProc
##'
##' @export
##' @param in.path location on disk where inputs are stord
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @author Ankur Desai
##'
metgapfill <- function(in.path, in.prefix, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE){
  # get start/end year code works on whole years only
  start_year <- year(start_date)
  end_year <- year(end_date)

  require(REddyProc)  
  require(ncdf4)
##  require(udunits2)
##  require(PEcAn.utils)
  
  #REddyProc installed to ~/R/library by install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
  #dependency minpack.lm may not install automatically, so install it first

  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        stringsAsFactors = FALSE)
  
  for(year in start_year:end_year) {
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep="."))
    
    # check if input exists
    if (!file.exists(old.file)) {
      logger.sever("Missing input file for year", year, "in folder", in.path)
    }
        
    # create array with results
    row <- year - start_year + 1
    results$file[row] <- new.file
    results$host[row] <- fqdn()
    results$startdate[row] <- paste0(year,"-01-01 00:00:00")
    results$enddate[row] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[row] <- 'application/x-netcdf'
    results$formatname[row] <- 'CF (gapfilled)'
    
    if (file.exists(new.file) && !overwrite) {
      logger.debug("File '", new.file, "' already exists, skipping to next file.")
      next
    }
    
    ## copy old file to new directory
    file.copy(old.file, new.file, overwrite=TRUE)
    
    ## Let's start with reading a few variables
    nc <- nc_open(new.file,write=TRUE)

    ## Should probably check for variable names (need to install ncdf4-helpers package)
    Tair <- ncvar_get(nc=nc,varid='air_temperature')
    Rg <- ncvar_get(nc=nc,varid='surface_downwelling_shortwave_flux_in_air')
    rH <- ncvar_get(nc=nc,varid='relative_humidity')
    PAR <- ncvar_get(nc=nc,varid='surface_downwelling_photosynthetic_photon_flux_in_air')
    precip <- ncvar_get(nc=nc,varid='precipitation_flux')
    #Rn <- ncvar_get(nc=nc,varid='Rn')
    sHum <- ncvar_get(nc=nc,varid='specific_humidity')
    Lw <- ncvar_get(nc=nc,varid='surface_downwelling_longwave_flux_in_air')
    Ts1 <-ncvar_get(nc=nc,varid='soil_temperature')
    #Ts2 <-ncvar_get(nc=nc,varid='TS2')
    VPD <-ncvar_get(nc=nc,varid='water_vapor_saturation_deficit')
    ws <-ncvar_get(nc=nc,varid='wind_speed')
    co2 <- ncvar_get(nc=nc,varid='mole_fraction_of_carbon_dioxide_in_air')
    press <- ncvar_get(nc=nc,varid='air_pressure')
    east_wind <- ncvar_get(nc=nc,varid='eastward_wind')
    north_wind <- ncvar_get(nc=nc,varid='northward_wind')
    
    # check to see if we have Rg values
    if (length(which(is.na(Rg))) == length(Rg)) {
      if (length(which(is.na(PAR))) == length(PAR)) {
        logger.severe("Missing both PAR and Rg")
      }
      Rg <- PAR / 2.1
    }
    
    ## make a data frame, convert -9999 to NA, convert to degrees C
    EddyData.F <- data.frame(Tair,Rg,rH,PAR,precip,sHum,Lw,Ts1,VPD,ws,co2,press,east_wind,north_wind)
    EddyData.F['Tair'] <- EddyData.F['Tair'] - 273.15

    ## Optional need: Compute VPD
    ##    EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))   

    ## Estimate number of good values, don't gap fill if no gaps or all gaps
    n_Tair <- sum(is.na(EddyData.F['Tair']))
    n_Rg <- sum(is.na(EddyData.F['Rg']))
    n_rH <- sum(is.na(EddyData.F['rH']))
    n_PAR <- sum(is.na(EddyData.F['PAR']))
    n_precip <- sum(is.na(EddyData.F['precip']))
    #n_Rn <- sum(is.na(EddyData.F['Rn']))
    n_sHum <- sum(is.na(EddyData.F['sHum']))
    n_Lw <- sum(is.na(EddyData.F['Lw']))
    n_Ts1 <- sum(is.na(EddyData.F['Ts1']))
    #n_Ts2 <- sum(is.na(EddyData.F['Ts2']))
    n_VPD <- sum(is.na(EddyData.F['VPD']))
    n_ws <- sum(is.na(EddyData.F['ws']))
    n_co2 <- sum(is.na(EddyData.F['co2']))
    n_press <- sum(is.na(EddyData.F['press']))
    n_east_wind <- sum(is.na(EddyData.F['east_wind']))
    n_north_wind <- sum(is.na(EddyData.F['north_wind']))

    # figure out datetime of nc file and convert to POSIX
    time <- ncvar_get(nc=nc,varid='time')
    nelem = length(time)
    tunit <- ncatt_get(nc=nc, varid='time', attname='units', verbose=verbose)
    origin <- "1900-01-01 00:00:00"
    time <-round(as.POSIXlt(ud.convert(time, tunit$value, paste('seconds since', origin)), origin=origin, tz="GMT"), units="mins")
    if (length(time) > 10000) {
      DTS.n <- 48
      time <-  30*60 + time
    } else {
      time <-  60*60 + time
      DTS.n <- 24
    }
    EddyData.F <- cbind(EddyData.F, DateTime=time)
    
    ## Create EddyProc object 
    EddyProc.C <- sEddyProc$new('Site', EddyData.F, c('Tair','Rg','rH','PAR','precip','sHum','Lw','Ts1','VPD','ws','co2','press','east_wind','north_wind'), DTS.n=DTS.n)
    
    ## Gap fill with default (see below for examples of advanced options)
    ## Have to do Rg, Tair, VPD first
    if(n_Rg>0&&n_Rg<nelem) EddyProc.C$sMDSGapFill('Rg', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Tair>0&&n_Tair<nelem) EddyProc.C$sMDSGapFill('Tair', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_VPD>0&&n_VPD<nelem) EddyProc.C$sMDSGapFill('VPD', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_rH>0&&n_rH<nelem) EddyProc.C$sMDSGapFill('rH', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_PAR>0&&n_PAR<nelem) EddyProc.C$sMDSGapFill('PAR', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_precip>0&&n_precip<nelem) EddyProc.C$sMDSGapFill('precip', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_sHum>0&&n_sHum<nelem) EddyProc.C$sMDSGapFill('sHum', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Lw>0&&n_Lw<nelem) EddyProc.C$sMDSGapFill('Lw', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Ts1>0&&n_Ts1<nelem) EddyProc.C$sMDSGapFill('Ts1', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_ws>0&&n_ws<nelem) EddyProc.C$sMDSGapFill('ws', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_co2>0&&n_co2<nelem) EddyProc.C$sMDSGapFill('co2', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_press>0&&n_press<nelem) EddyProc.C$sMDSGapFill('press', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_east_wind>0&&n_east_wind<nelem) EddyProc.C$sMDSGapFill('east_wind', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_north_wind>0&&n_north_wind<nelem) EddyProc.C$sMDSGapFill('north_wind', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    
    ## Extract filled variables into data frame, replace any NA back to -9999
    ## print('Extracting dataframe elements and writing back to nc file')
    Extracted <- EddyProc.C$sExportResults()

    ##Write back to NC file, convert air T to Kelvin
    error <- c()
    if(n_Tair>0&&n_Tair<nelem) {
      Tair_f <- Extracted[,'Tair_f'] + 273.15
      if (length(which(is.na(Tair_f))) > 0) error <- c(error, "air_temperature")
      ncvar_put(nc,varid='air_temperature',vals=Tair_f)
    }
    if(n_Rg>0&&n_Rg<nelem) {
      Rg_f <- Extracted[,'Rg_f']
      if (length(which(is.na(Rg_f))) > 0) error <- c(error, "surface_downwelling_shortwave_flux_in_air")
      ncvar_put(nc,varid='surface_downwelling_shortwave_flux_in_air',vals=Rg_f)
    }
    if(n_rH>0&&n_rH<nelem) {
      rH_f <- Extracted[,'rH_f']
      if (length(which(is.na(rH_f))) > 0) error <- c(error, "relative_humidity")
      ncvar_put(nc,varid='relative_humidity',vals=rH_f)
    }
    if(n_PAR>0&&n_PAR<nelem) {
      PAR_f <- Extracted[,'PAR_f']
      if (length(which(is.na(PAR_f))) > 0) error <- c(error, "surface_downwelling_photosynthetic_photon_flux_in_air")
      ncvar_put(nc,varid='surface_downwelling_photosynthetic_photon_flux_in_air',vals=PAR_f)
    }
    if(n_precip>0&&n_precip<nelem) {
      precip_f <- Extracted[,'precip_f']
      if (length(which(is.na(precip_f))) > 0) error <- c(error, "precipitation_flux")
      ncvar_put(nc,varid='precipitation_flux',vals=precip_f)
    }
    if(n_sHum>0&&n_sHum<nelem) {
      sHum_f <- Extracted[,'sHum_f']
      if (length(which(is.na(sHum_f))) > 0) error <- c(error, "specific_humidity")
      ncvar_put(nc,varid='specific_humidity',vals=sHum_f)
    }
    if(n_Lw>0&&n_Lw<nelem) {
      Lw_f <- Extracted[,'Lw_f']
      if (length(which(is.na(Lw_f))) > 0) error <- c(error, "surface_downwelling_longwave_flux_in_air")
      ncvar_put(nc,varid='surface_downwelling_longwave_flux_in_air',vals=Lw_f)
    }
    if(n_Ts1>0&&n_Ts1<nelem) {
      Ts1_f <- Extracted[,'Ts1_f']
      if (length(which(is.na(Ts1_f))) > 0) error <- c(error, "soil_temperature")
      ncvar_put(nc,varid='soil_temperature',vals=Ts1_f)
    }
    if(n_VPD>0&&n_VPD<nelem) {
      VPD_f <- Extracted[,'VPD_f']
      if (length(which(is.na(VPD_f))) > 0) error <- c(error, "water_vapor_saturation_deficit")
      ncvar_put(nc,varid='water_vapor_saturation_deficit',vals=VPD_f)
    }
    if(n_ws>0&&n_ws<nelem) {
      ws_f <- Extracted[,'ws_f']
      if (length(which(is.na(ws_f))) > 0) error <- c(error, "wind_speed")
      ncvar_put(nc,varid='wind_speed',vals=ws_f)
    }
    if(n_co2>0&&n_ws<nelem) {
      co2_f <- Extracted[,'co2_f']
      if (length(which(is.na(co2_f))) > 0) error <- c(error, "mole_fraction_of_carbon_dioxide_in_air")
      ncvar_put(nc,varid='mole_fraction_of_carbon_dioxide_in_air',vals=co2_f)
    }
    if(n_press>0&&n_press<nelem) {
      press_f <- Extracted[,'press_f']
      if (length(which(is.na(press_f))) > 0) error <- c(error, "air_pressure")
      ncvar_put(nc,varid='air_pressure',vals=press_f)
    }
    if(n_east_wind>0&&n_east_wind<nelem) {
      east_wind_f <- Extracted[,'east_wind_f']
      if (length(which(is.na(east_wind_f))) > 0) error <- c(error, "eastward_wind")
      ncvar_put(nc,varid='eastward_wind',vals=east_wind_f)
    }
    if(n_north_wind>0&&n_north_wind<nelem) {
      north_wind_f <- Extracted[,'north_wind_f']
      if (length(which(is.na(north_wind_f))) > 0) error <- c(error, "northward_wind")
      ncvar_put(nc,varid='northward_wind',vals=north_wind_f)
    }

    nc_close(nc)
    
    if (length(error) > 0) {
      logger.severe("Could not do gapfill, results are in", new.file, ".",
                    "The following variables have NA's:", paste(error, sep=", "))
    }
    
  } #end loop
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Extra: Examples of extended usage for advanced users
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ Add some (non-sense) example vectors:
  #+++ Quality flag vector (e.g. from applying ustar filter)
#  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10))
  #+++ Step function vector to simulate e.g. high/low water table
#  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, Step=ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1))
  #+++ Initialize eddy processing class with more columns
#  EddyTest.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))
  #+++ Gap fill variable with (non-default) variables and limits including preselection with quality flag QF 
#  EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1)
  #+++ Use individual gap filling subroutines with different window sizes and up to five variables and limits
#  EddyTest.C$sFillInit('NEE') #Initalize 'NEE' as variable to fill
#  Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'rH',30, 'Tair',2.5, 'Tsoil',2, 'Step',0.5)
#  Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.5)
#  Result_Step3.F <- EddyTest.C$sFillMDC(3)
  
  ## NARR BASED downscaling - future funtcionality
## Step 2. Determine if gaps need filling - if not, skip to step 7
## Step 3. Read fill file (reanalysis - NARR or CFSR)
  ## Run Steps 4-6 for each variable of interest (Temperature, precipitation, wind speed, etc...)
  ## Step 4_simple. Debias with a simple normalize filter
  ## 4a. Average Ameriflux up to reanalysis
  ## 4b. Put gaps in reanalysis to mimic Ameriflux
  ## 4c. Estimate mean and standard deviation of reanalysis_gappy and Ameriflux
  ## 4d. Normalize reanalysis_nongappy time series by subtracting gappy mean and divide by stdev
  ## 4e. Debias by multiplying by Ameriflux mean and stdev
  ## 4f. This may not work so well for precipitation (non-Gaussian)
  ## Step 4. Debias with Copula approach (advanced - see papers by Kunstman et al)
  ## 4a. Average Ameriflux up to reanalysis
  ## 4b. Put gaps in reanalysis to mimic Ameriflux
  ## 4c. Estimate rank-sun MDF for each variable
  ## 4d. Fit Copula function to rank-rank correlation
  ## 4e. Construct function to debias reanalysis - apply to non-gappy reanalysis
  ## Step 5. Temporal downscale debiased time series if needed (CFSR is hourly, NARR is 3 hourly)
  ## 5a. Simpliest approach just use linear interpolation or spline interpolate with built-in R functions
  ## 5b. May need to have an edge case for precipitation
  ## Step 6.Replace gaps with debiased time series (perhaps store statistics of fit somewhere as a measure of uncertainty?)
  ## Step 7. Write to outfolder the new NetCDF file

  invisible(results)
} ## End function

