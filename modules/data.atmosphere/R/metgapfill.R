##' Take an Ameriflux NetCDF file
##' Fill missing met values using MDS approach
##' and also NARR (in future version)
##
##' @param in.path
##' @param in.file
##' @param outfolder
##' @author Ankur Desai
##'

metgapfill <- function(in.path,in.file,outfolder){
  require(REddyProc)  
  require(ncdf4)
  require(udunits2)
  require(PEcAn.utils)
  
  #REddyProc installed to ~/R/library by install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")

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
    Tair <- ncvar_get(nc=nc,varid='air_temperature')
    Rg <- ncvar_get(nc=nc,varid='surface_downwelling_shortwave_flux_in_air')
    rH <- ncvar_get(nc=nc,varid='relative_humidity')
    
    ## May need to convert -9999 to NA here
    
    ## make a data frame
    EddyData.F <- data.frame(Tair,Rh,rH)
    EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))   

    ## have to deal with time
    ## time <- ncvar_get(nc=nc,varid='time') ## time is attr not var
    ## TODO Extract Year,Day,Hour
    ## HACK way here, assume file has 8760,8784,17520,or 17568 lines
    nelem = length(rH)
    if (nelem<=8784) nhrs <-24 else nhrs <- 48
    Year <- rep(2000,nelem)
    DoY <- floor(seq(1,nelem/nhrs,length=nelem))
    Hour <- rep(0:(nhrs-1),nelem/nhrs)
    EddyData.F <- cbind(EddyData.F,Year=Year,DoY=DoY,Hour=Hour)
    
    ## convert time to Posix
    EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
    
    ## Create EddyProc object
    EddyProc.C <- sEddyProc$new('Site', EddyDataWithPosix.F, c('Ta','Rg','rH','VPD'))
    
    ## Gap fill with default (see below for examples of advanced options)
    EddyProc.C$sMDSGapFill('Ta', FillAll.b=TRUE)
    EddyProc.C$sMDSGapFill('Rg', FillAll.b=TRUE)
    EddyProc.C$sMDSGapFill('rH', FillAll.b=TRUE)
    EddyProc.C$sMDSGapFill('VPD', FillAll.b=TRUE)
    
    ## Extract filled variables into data frame
    Extracted <- EddyProc.C$sExportResults()
    Tair_f <- Extracted['Tair_f']
    Rg_f <- Extracted['Rg_f']
    rH_f <- Extracted['rH_f']
    
    ## TODO: Put back in missing val? Plots? Convert VPD to RH?
    
    ## Debug: look at output
    
    ## Write to NC file
    ncvar_put(nc,varid='air_temperature',vals=Tair_f)
    ncvar_put(nc,varid='surface_downwelling_shortwave_flux_in_air',vals=Rg_f)
    ncvar_put(nc,varid='relative_humidity',vals=rH_f)
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

