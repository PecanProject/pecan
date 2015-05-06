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
##' @param lst is timezone offset from UTC, if timezone is available in time:units atribute in file, it will use that, default is to assume UTC
##' @author Ankur Desai
##'
metgapfill <- function(in.path, in.prefix, outfolder, start_date, end_date, lst=0, overwrite=FALSE, verbose=FALSE){

  require(REddyProc)    
  #REddyProc installed to ~/R/library by install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
  #dependency minpack.lm may not install automatically, so install it first
  require(ncdf4)
  require(udunits2)
  require(PEcAn.utils)
  require(lubridate)
  
  # get start/end year code works on whole years only
  start_year <- year(start_date)
  end_year <- year(end_date)

  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  
  for(year in start_year:end_year) {
    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep="."))
    
    # check if input exists
    if (!file.exists(old.file)) {
      logger.severe("Missing input file for year", year, "in folder", in.path)
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
    
    #extract time, lat, lon, elevation, timezone for pressure and radiation calculations
    time <- ncvar_get(nc=nc,varid='time')
    lat <- ncvar_get(nc=nc,varid='latitude')
    lon <- ncvar_get(nc=nc,varid='longitude')
    elev <- ncatt_get(nc=nc,varid=0,'elevation')
    tzone <- ncatt_get(nc=nc,varid='time','units')
    ##Future: query elevation from site.id
    if (elev$hasatt) {
      elevation <- as.numeric((unlist(strsplit(elev$value," ")))[1])
    } else {
      elevation <- 0  #assume sea level by default
    }
    if (tzone$hasatt) { 
      tdimunit <- unlist(strsplit(tzone$value," "))
      tdimtz <- substr(tdimunit[length(tdimunit)],1,1)
      if ((tdimtz=="+")||(tdimtz=="-")) {
        lst <- as.numeric(tdimunit[length(tdimunit)]) #extract timezone from file      
      }
    }
    
    #default pressure (in Pascals) if no pressure observations are available (based on NOAA 1976 equation for pressure altitude for WMO international standard atmosphere)
    standard_pressure <- ((1 - ((3.28084*elevation)/145366.45))^(1/.190284))*101325.0
    if (length(which(is.na(press))) == length(press)) { press[is.na(press)] <- standard_pressure }
    
    # check to see if we have Rg values
    if (length(which(is.na(Rg))) == length(Rg)) {
      if (length(which(is.na(PAR))) == length(PAR)) {
        logger.severe("Missing both PAR and Rg")
      }
      Rg <- PAR*1e6 / 2.1
    }
    
    ## Use Rg and PAR to gap fill
    badPAR <- is.na(PAR)
    badRg <- is.na(Rg)    
    PAR[badPAR] = Rg[badPAR] * 2.1 / 1e6
    Rg[badRg] = 1e6 * PAR[badRg] / 2.1
    Rg[Rg<0] = 0.0
    PAR[PAR<0]= 0.0
    
    ## make night dark - based on met2model.ED2.R in models/ed/R
    ## First: calculate potential radiation
    sec   <- nc$dim$time$vals
    sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
    ifelse(leap_year(year)==TRUE,
           dt <- (366*24*60*60)/length(sec), #leap year
           dt <- (365*24*60*60)/length(sec)) #non-leap year
    ifelse(leap_year(year)==TRUE,
           doy <- rep(1:366,each=86400/dt),
           doy <- rep(1:365,each=86400/dt))
    ifelse(leap_year(year)==TRUE,
           hr <- rep(seq(0,length=86400/dt,by=dt/86400*24),366),
           hr <- rep(seq(0,length=86400/dt,by=dt/86400*24),365))
    f <- pi/180*(279.5+0.9856*doy)
    et <- (-104.7*sin(f)+596.2*sin(2*f)+4.3*sin(4*f)-429.3*cos(f)-2.0*cos(2*f)+19.3*cos(3*f))/3600  #equation of time -> eccentricity and obliquity
    merid <- floor(lon/15)*15
    merid[merid<0] <- merid[merid<0]+15
    lc <- (lon-merid)*-4/60  ## longitude correction
    tz <- merid/360*24 ## time zone
    midbin <- 0.5*dt/86400*24 ## shift calc to middle of bin
    t0 <- 12+lc-et-tz-midbin   ## solar time
    h <- pi/12*(hr-t0)  ## solar hour
    dec <- -23.45*pi/180*cos(2*pi*(doy+10)/365)  ## declination
    cosz <- sin(lat*pi/180)*sin(dec)+cos(lat*pi/180)*cos(dec)*cos(h)
    cosz[cosz<0] <- 0  
    rpot <- 1366*cosz  #in UTC
    toff <- lst*3600/dt  #timezone offset correction
    if (toff < 0) { 
      slen <- length(rpot)
      rpot <- c(rpot[(abs(toff)+1):slen],rpot[1:abs(toff)])
    }
    if (toff > 0) {
      slen <- length(rpot)
      rpot <- c(rpot[(slen-toff+1):slen],rpot[1:(slen-toff)])
    }
    ## Next: turn nighttime to 0
    Rg[rpot==0] <- 0
    PAR[rpot==0] <- 0    
    ## we could add Rg[Rg>rpot] <- rpot, but probably should bias correct first?
    
    ## make a data frame, convert -9999 to NA, convert to degrees C
    EddyData.F <- data.frame(Tair,Rg,rH,PAR,precip,sHum,Lw,Ts1,VPD,ws,co2,press,east_wind,north_wind)
    EddyData.F['Tair'] <- EddyData.F['Tair'] - 273.15
    EddyData.F['Ts1'] <- EddyData.F['Ts1'] - 273.15
    EddyData.F['VPD'] <- EddyData.F['VPD']/1000.0

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
    maxbad <- nelem/2
    
    ## Gap fill with default (see below for examples of advanced options)
    ## Have to do Rg, Tair, VPD first
    
    ## First, define filled variable and do some simple gap filling where possible
    Rg_f <- Rg
    Tair_f <- Tair
    VPD_f <- VPD
    rH_f <- rH
    PAR_f <- PAR
    precip_f <- precip
    sHum_f <- sHum
    Lw_f <- Lw
    Ts1_f <- Ts1
    ws_f <- ws
    ws_f[is.na(ws_f)] <- mean(ws,na.rm=TRUE)
    ws_f[is.na(ws_f)] <- 1.0
    co2_f <- co2
    press_f <- press
    press_f[is.na(press_f)] <- mean(press,na.rm=TRUE)
    east_wind_f <- east_wind
    east_wind_f[is.na(east_wind_f)] <- 0.0
    north_wind_f <- north_wind
    north_wind_f[is.na(north_wind_f)] <- ws_f[is.na(north_wind_f)]
    
    if(n_Rg>0&&n_Rg<maxbad) EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Tair>0&&n_Tair<maxbad) EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_VPD>0&&n_VPD<maxbad) EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_rH>0&&n_rH<maxbad) EddyProc.C$sMDSGapFill('rH', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_PAR>0&&n_PAR<maxbad) EddyProc.C$sMDSGapFill('PAR', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_precip>0&&n_precip<maxbad) EddyProc.C$sMDSGapFill('precip', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_sHum>0&&n_sHum<maxbad) EddyProc.C$sMDSGapFill('sHum', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Lw>0&&n_Lw<maxbad) EddyProc.C$sMDSGapFill('Lw', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_Ts1>0&&n_Ts1<maxbad) EddyProc.C$sMDSGapFill('Ts1', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_ws>0&&n_ws<maxbad) EddyProc.C$sMDSGapFill('ws', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose) 
    if(n_co2>0&&n_co2<maxbad) EddyProc.C$sMDSGapFill('co2', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose) 
    if(n_press>0&&n_press<maxbad) EddyProc.C$sMDSGapFill('press', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    if(n_east_wind>0&&n_east_wind<maxbad) EddyProc.C$sMDSGapFill('east_wind', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose) 
    if(n_north_wind>0&&n_north_wind<maxbad) EddyProc.C$sMDSGapFill('north_wind', FillAll.b=FALSE,V1.s='Rg',V2.s='VPD',V3.s='Tair',Verbose.b=verbose)
    
    ## Extract filled variables into data frame
    ## print('Extracting dataframe elements and writing back to nc file')
    Extracted <- EddyProc.C$sExportResults()

    ##Write back to NC file, convert air T to Kelvin
    error <- c()
    if(('Tair_f' %in% colnames(Extracted))) Tair_f <- Extracted[,'Tair_f'] + 273.15
    if (length(which(is.na(Tair_f))) > 0) error <- c(error, "air_temperature")
    ncvar_put(nc,varid='air_temperature',vals=Tair_f)

    if(('Rg_f' %in% colnames(Extracted))) Rg_f <- Extracted[,'Rg_f']
    if (length(which(is.na(Rg_f))) > 0) error <- c(error, "surface_downwelling_shortwave_flux_in_air")
    ncvar_put(nc,varid='surface_downwelling_shortwave_flux_in_air',vals=Rg_f)

    if(('rH_f' %in% colnames(Extracted))) rH_f <- Extracted[,'rH_f']
    if (length(which(is.na(rH_f))) > 0) error <- c(error, "relative_humidity")
    ncvar_put(nc,varid='relative_humidity',vals=rH_f)

    if(('PAR_f' %in% colnames(Extracted))) PAR_f <- Extracted[,'PAR_f']
    if (length(which(is.na(PAR_f))) > 0) error <- c(error, "surface_downwelling_photosynthetic_photon_flux_in_air")
    ncvar_put(nc,varid='surface_downwelling_photosynthetic_photon_flux_in_air',vals=PAR_f)
    
    if(('precip_f' %in% colnames(Extracted))) precip_f <- Extracted[,'precip_f']
    if (length(which(is.na(precip_f))) > 0) error <- c(error, "precipitation_flux")
    ncvar_put(nc,varid='precipitation_flux',vals=precip_f)
    
    if(('sHum_f' %in% colnames(Extracted))) sHum_f <- Extracted[,'sHum_f']
    sHum_f[is.na(sHum_f)] = 0.622*(rH_f[is.na(sHum_f)]/100.0)*(get.es(Tair_f[is.na(sHum_f)]-273.15) / 1e3)
    if (length(which(is.na(sHum_f))) > 0) error <- c(error, "specific_humidity")
    ncvar_put(nc,varid='specific_humidity',vals=sHum_f)
    
    if(('Lw_f' %in% colnames(Extracted))) Lw_f <- Extracted[,'Lw_f']
    Lw_f[is.na(Lw_f)] <- 0.83 * 5.67e-8 * Tair_f^4
    if (length(which(is.na(Lw_f))) > 0) error <- c(error, "surface_downwelling_longwave_flux_in_air")
    ncvar_put(nc,varid='surface_downwelling_longwave_flux_in_air',vals=Lw_f)

    if(('Ts1_f' %in% colnames(Extracted))) Ts1_f <- Extracted[,'Ts1_f'] + 273.15
    if(sum(is.na(Ts1_f)) > 0) {   
      Tair_ff <- Tair_f
      Tair_ff[is.na(Tair_ff)] <- mean(Tair_ff,na.rm=TRUE)
      tau = 15.0*DTS.n
      filt = exp(-(1:length(Tair_ff))/tau)
      filt = (filt/sum(filt))
      Ts_1ff = convolve(Tair_ff, filt) 
      Ts1_f[is.na(Ts1_f)] <- Ts_1ff[is.na(Ts1_f)] 
    }
    if (length(which(is.na(Ts1_f))) > 0) error <- c(error, "soil_temperature")
    ncvar_put(nc,varid='soil_temperature',vals=Ts1_f)

    if(('VPD_f' %in% colnames(Extracted))) VPD_f <- Extracted[,'VPD_f']*1000.0
    if (length(which(is.na(VPD_f))) > 0) error <- c(error, "water_vapor_saturation_deficit")
    ncvar_put(nc,varid='water_vapor_saturation_deficit',vals=VPD_f)
    
    if(('ws_f' %in% colnames(Extracted))) ws_f <- Extracted[,'ws_f']

    if(('co2_f' %in% colnames(Extracted))) co2_f <- Extracted[,'co2_f']
    co2_f[is.na(co2_f)] <- mean(co2,na.rm=TRUE)
    co2_f[is.na(co2_f)] <- 380.0/1e6
    if (length(which(is.na(co2_f))) > 0) error <- c(error, "mole_fraction_of_carbon_dioxide_in_air")
    ncvar_put(nc,varid='mole_fraction_of_carbon_dioxide_in_air',vals=co2_f)
    
    if(('press_f' %in% colnames(Extracted))) press_f <- Extracted[,'press_f']
    if(sum(is.na(press_f)) > 0) { press_f[is.na(press_f)] <- standard_pressure }
    if (length(which(is.na(press_f))) > 0) error <- c(error, "air_pressure")
    ncvar_put(nc,varid='air_pressure',vals=press_f)

    if(('east_wind_f' %in% colnames(Extracted))) east_wind_f <- Extracted[,'east_wind_f']
    if (length(which(is.na(east_wind_f))) > 0) error <- c(error, "eastward_wind")
    ncvar_put(nc,varid='eastward_wind',vals=east_wind_f)

    if(('north_wind_f' %in% colnames(Extracted))) north_wind_f <- Extracted[,'north_wind_f']
    north_wind_f[is.na(north_wind_f)] <- ws_f[is.na(north_wind_f)]
    north_wind_f[is.na(north_wind_f)] <- 1.0
    if (length(which(is.na(north_wind_f))) > 0) error <- c(error, "northward_wind")
    ncvar_put(nc,varid='northward_wind',vals=north_wind_f)
    
    ws_f[is.na(ws_f)] = sqrt(north_wind_f[is.na(ws_f)]^2 + east_wind_f[is.na(ws_f)]^2)
    if (length(which(is.na(ws_f))) > 0) error <- c(error, "wind_speed")
    ncvar_put(nc,varid='wind_speed',vals=ws_f)
    
    nc_close(nc)
    
    if (length(error) > 0) {
      fail.file <- file.path(outfolder, paste(in.prefix, year,"failure","nc", sep="."))
      file.rename(from = new.file, to = fail.file)
      logger.severe("Could not do gapfill, results are in", fail.file, ".",
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

