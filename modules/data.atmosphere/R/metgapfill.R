##' Take an Ameriflux NetCDF file
##' Fill missing met values with corresponding
##' reanalysis
##
##' @param in.path
##' @param in.file
##' @param outfolder
##' @author Ankur Desai
##'

metgapfill.csv <- function(in.path,in.file,outfolder){

  library(REddyProc)
  #installed to ~/R/library by install.packages("REddyProc", repos="http://R-Forge.R-project.org", type="source")

  ## Step 1. Read met variables from Ameriflux NetCDF CF in.file

  ## Step 2. Determine if gaps need filling - if not, skip to step 7
  
  
  ## Step 3. USe REddyProc MPI library 
  
  #+++ Load data with one header and one unit row from (tab-delimited) text file
#  Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
#  EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
  #+++ If not provided, calculate VPD from Tair and rH
#  EddyData.F <- cbind(EddyData.F,VPD=fCalcVPDfromRHandTair(EddyData.F$rH, EddyData.F$Tair))
  
  #+++ Add time stamp in POSIX time format
#  EddyDataWithPosix.F <- fConvertTimeToPosix(EddyData.F, 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')
  
  #+++ Initalize R5 reference class sEddyProc for processing of eddy data
  #+++ with all variables needed for processing later
#  EddyProc.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, c('NEE','Rg', 'Tair', 'VPD'))
    
  #+++ Fill gaps in variables with MDS gap filling algorithm
#  EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE)
#  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE)
    
  #+++ Export gap filled data to standard data frame
#  FilledEddyData.F <- EddyProc.C$sExportResults()
  
  #+++ Save results into (tab-delimited) text file in directory \out
#  CombinedData.F <- cbind(EddyData.F, FilledEddyData.F)
#  fWriteDataframeToFile(CombinedData.F, 'DE-Tha-Results.txt', 'out')
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Extra: Examples of extended usage for advanced users
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++ Add some (non-sense) example vectors:
  #+++ Quality flag vector (e.g. from applying ustar filter)
#  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, QF=rep(c(1,0,1,0,1,0,0,0,0,0),nrow(EddyData.F)/10))
  #+++ Step function vector to simulate e.g. high/low water table
#  EddyDataWithPosix.F <- cbind(EddyDataWithPosix.F, Step=ifelse(EddyData.F$DoY < 200 | EddyData.F$DoY > 250, 0, 1))
  
  #+++ Initialize eddy processing class with more columns
#  EddyTest.C <- sEddyProc$new('DE-Tha', EddyDataWithPosix.F, 
                              c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD', 'QF', 'Step'))
  
  #+++ Gap fill variable with (non-default) variables and limits including preselection with quality flag QF 
#  EddyTest.C$sMDSGapFill('LE', QFVar.s='QF', QFValue.n=0, V1.s='Rg', T1.n=30, V2.s='Tsoil', T2.n=2, 'Step', 0.1)
  
  #+++ Use individual gap filling subroutines with different window sizes and up to five variables and limits
#  EddyTest.C$sFillInit('NEE') #Initalize 'NEE' as variable to fill
#  Result_Step1.F <- EddyTest.C$sFillLUT(3, 'Rg',50, 'rH',30, 'Tair',2.5, 'Tsoil',2, 'Step',0.5)
#  Result_Step2.F <- EddyTest.C$sFillLUT(6, 'Tair',2.5, 'VPD',3, 'Step',0.5)
#  Result_Step3.F <- EddyTest.C$sFillMDC(3)
}
  
  
  ## NARR BASED downscaling - future funtcion
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

  
}

