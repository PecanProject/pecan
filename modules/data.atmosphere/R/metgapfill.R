##' Take an Ameriflux NetCDF file
##' Fill missing met values using MDS approach using MPI-BGC REddyProc library
##' Currently 
##' Future version: Choose which variables to gap fill
##' Future version will first downscale and fill with NARR, then REddyProc
##
##' @param in.path
##' @param in.prefix
##' 
##' @param outfolder
##' @author Ankur Desai
##'

metgapfill <- function(in.path,in.prefix,outfolder){
  require(REddyProc)  
  require(ncdf4)
##  require(udunits2)
##  require(PEcAn.utils)
  
  #REddyProc installed to ~/R/library by install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")
  #dependency minpack.lm may not install automatically, so install it first

  ## Step 1. Read met variables from Ameriflux NetCDF CF in.file
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    ## send warning
    return(NULL)
  }  
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  for(i in 1:length(files)){
    
    new.file =file.path(outfolder,files[i])
    
    ## copy old file to new directory
    system2("cp",paste(file.path(in.path,files[i]),new.file))
    
    ## Let's start with reading a few variables
    nc <- nc_open(new.file,write=TRUE)

    ## Should probably check for variable names (need to install ncdf4-helpers package)
    Tair <- ncvar_get(nc=nc,varid='air_temperature')
    Rg <- ncvar_get(nc=nc,varid='surface_downwelling_shortwave_flux')
    rH <- ncvar_get(nc=nc,varid='relative_humidity')
    PAR <- ncvar_get(nc=nc,varid='PAR')
    precip <- ncvar_get(nc=nc,varid='precipitation_flux')
    Rn <- ncvar_get(nc=nc,varid='Rn')
    sHum <- ncvar_get(nc=nc,varid='specific_humidity')
    Lw <- ncvar_get(nc=nc,varid='surface_downwelling_longwave_flux')
    Ts1 <-ncvar_get(nc=nc,varid='TS1')
    Ts2 <-ncvar_get(nc=nc,varid='TS2')
    VPD <-ncvar_get(nc=nc,varid='VPD')
    ws <-ncvar_get(nc=nc,varid='wind_speed')
    co2 <- ncvar_get(nc=nc,varid='CO2')
    press <- ncvar_get(nc=nc,varid='air_pressure')
    east_wind <- ncvar_get(nc=nc,varid='eastward_wind')
    north_wind <- ncvar_get(nc=nc,varid='northward_wind')
    
    ## make a data frame, convert -9999 to NA, convert to degrees C
    EddyData.F <- data.frame(Tair,Rg,rH,PAR,precip,Rn,sHum,Lw,Ts1,Ts2,VPD,ws,co2,press,east_wind,north_wind)
    EddyData.F[EddyData.F <= -9999] = NA
    EddyData.F['Tair'] = EddyData.F['Tair']-273.15

    ## Optional need: Compute VPD
    ##    EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))   

    ## Estimate number of good values, don't gap fill if no gaps or all gaps
    n_Tair <- sum(is.na(EddyData.F['Tair']))
    n_Rg <- sum(is.na(EddyData.F['Rg']))
    n_rH <- sum(is.na(EddyData.F['rH']))
    n_PAR <- sum(is.na(EddyData.F['PAR']))
    n_precip <- sum(is.na(EddyData.F['precip']))
    n_Rn <- sum(is.na(EddyData.F['Rn']))
    n_sHum <- sum(is.na(EddyData.F['sHum']))
    n_Lw <- sum(is.na(EddyData.F['Lw']))
    n_Ts1 <- sum(is.na(EddyData.F['Ts1']))
    n_Ts2 <- sum(is.na(EddyData.F['Ts2']))
    n_VPD <- sum(is.na(EddyData.F['VPD']))
    n_ws <- sum(is.na(EddyData.F['ws']))
    n_co2 <- sum(is.na(EddyData.F['co2']))
    n_press <- sum(is.na(EddyData.F['press']))
    n_east_wind <- sum(is.na(EddyData.F['east_wind']))
    n_north_wind <- sum(is.na(EddyData.F['north_wind']))

    ## read time variables, add to data frame
    Year <- ncvar_get(nc=nc,varid='YEAR')
    DoY <- ncvar_get(nc=nc,varid='DOY')
    time <- ncvar_get(nc=nc,varid='time')
    Hour <- ((round((time-DoY)*48.0)/2.0)+0.5)
    
    nelem = length(Year)
    EddyData.F <- cbind(EddyData.F,Year=Year,DoY=DoY,Hour=Hour)
        
    ## convert time to Posix
    EddyData.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
    
    ## Create EddyProc object 
    EddyProc.C <- sEddyProc$new('Site', EddyData.F, c('Tair','Rg','rH','PAR','precip','Rn','sHum','Lw','Ts1','Ts2','VPD','ws','co2','press','east_wind','north_wind'))
                                
    ## Gap fill with default (see below for examples of advanced options)
    ## Have to do Rg, Tair, VPD first
    if(n_Rg>0&&n_Rg<nelem) EddyProc.C$sMDSGapFill('Rg', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_Tair>0&&n_Tair<nelem) EddyProc.C$sMDSGapFill('Tair', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_VPD>0&&n_VPD<nelem) EddyProc.C$sMDSGapFill('VPD', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_rH>0&&n_rH<nelem) EddyProc.C$sMDSGapFill('rH', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_PAR>0&&n_PAR<nelem) EddyProc.C$sMDSGapFill('PAR', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_precip>0&&n_precip<nelem) EddyProc.C$sMDSGapFill('precip', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_Rn>0&&n_Rn<nelem) EddyProc.C$sMDSGapFill('Rn', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_sHum>0&&n_sHum<nelem) EddyProc.C$sMDSGapFill('sHum', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_Lw>0&&n_Lw<nelem) EddyProc.C$sMDSGapFill('Lw', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_Ts1>0&&n_Ts2<nelem) EddyProc.C$sMDSGapFill('Ts1', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_Ts2>0&&n_Ts2<nelem) EddyProc.C$sMDSGapFill('Ts2', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_ws>0&&n_ws<nelem) EddyProc.C$sMDSGapFill('ws', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_co2>0&&n_co2<nelem) EddyProc.C$sMDSGapFill('co2', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_press>0&&n_press<nelem) EddyProc.C$sMDSGapFill('press', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_east_wind>0&&n_east_wind<nelem) EddyProc.C$sMDSGapFill('east_wind', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    if(n_north_wind>0&&n_north_wind<nelem) EddyProc.C$sMDSGapFill('north_wind', FillAll.b=TRUE,V1.s='Rg',V2.s='VPD',V3.s='Tair')
    
    ## Extract filled variables into data frame, replace any NA back to -9999
    ## print('Extracting dataframe elements and writing back to nc file')
    Extracted <- EddyProc.C$sExportResults()
    Extracted[is.na(Extracted)] = -9999.0

    ##Write back to NC file, convert air T to Kelvin
    if(n_Tair>0&&n_Tair<nelem) {
      Tair_f <- Extracted[,'Tair_f']
      Tair_f = Tair_f + 273.15
      ncvar_put(nc,varid='air_temperature',vals=Tair_f)
    }
    if(n_Rg>0&&n_Rg<nelem) {
      Rg_f <- Extracted[,'Rg_f']
      ncvar_put(nc,varid='surface_downwelling_shortwave_flux',vals=Rg_f)
    }
    if(n_rH>0&&n_rH<nelem) {
      rH_f <- Extracted[,'rH_f']
      ncvar_put(nc,varid='relative_humidity',vals=rH_f)
    }
    if(n_PAR>0&&n_PAR<nelem) {
      PAR_f <- Extracted[,'PAR_f']
      ncvar_put(nc,varid='PAR',vals=PAR_f)
    }
    if(n_precip>0&&n_precip<nelem) {
      precip_f <- Extracted[,'precip_f']
      ncvar_put(nc,varid='precipitation_flux',vals=precip_f)
    }
    if(n_Rn>0&&n_Rn<nelem) {
      Rn_f <- Extracted[,'Rn_f']
      ncvar_put(nc,varid='Rn',vals=Rn_f)
    }
    if(n_sHum>0&&n_sHum<nelem) {
      sHum_f <- Extracted[,'sHum_f']
      ncvar_put(nc,varid='specific_humidity',vals=sHum_f)
    }
    if(n_Lw>0&&n_Lw<nelem) {
      Lw_f <- Extracted[,'Lw_f']
      ncvar_put(nc,varid='surface_downwelling_longwave_flux',vals=Lw_f)
    }
    if(n_Ts1>0&&n_Ts1<nelem) {
      Ts1_f <- Extracted[,'Ts1_f']
      ncvar_put(nc,varid='TS1',vals=Ts1_f)
    }
    if(n_Ts2>0&&n_Ts2<nelem) {
      Ts2_f <- Extracted[,'Ts2_f']
      ncvar_put(nc,varid='TS2',vals=Ts2_f)
    }
    if(n_VPD>0&&n_VPD<nelem) {
      VPD_f <- Extracted[,'VPD_f']
      ncvar_put(nc,varid='VPD',vals=VPD_f)
    }
    if(n_ws>0&&n_ws<nelem) {
      ws_f <- Extracted[,'ws_f']
      ncvar_put(nc,varid='wind_speed',vals=ws_f)
    }
    if(n_co2>0&&n_co2<nelem) {
      co2_f <- Extracted[,'co2_f']
      ncvar_put(nc,varid='CO2',vals=co2_f)
    }
    if(n_press>0&&n_press<nelem) {
      press_f <- Extracted[,'press_f']
      ncvar_put(nc,varid='air_pressure',vals=press_f)
    }
    if(n_east_wind>0&&n_east_wind<nelem) {
      east_wind_f <- Extracted[,'east_wind_f']
      ncvar_put(nc,varid='eastward_wind',vals=east_wind_f)
    }
    if(n_north_wind>0&&n_north_wind<nelem) {
      north_wind_f <- Extracted[,'north_wind_f']
      ncvar_put(nc,varid='northward_wind',vals=north_wind_f)
    }

    nc_close(nc)
    
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

} ## End function

