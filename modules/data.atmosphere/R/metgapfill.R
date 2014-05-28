##' Take an Ameriflux NetCDF file
##' Fill missing met values with corresponding
##' reanalysis
##
##' @param in.path
##' @param in.file
##' @param outfolder
##' @author Ankur Desai
##'

metgapfill.csv <- function(in.path,in.file,outfolder,lat=NULL,lon=NULL,fill.path=NULL,fill.file=NULL){

  ## Pseudo-code - first qualitiative description

  ## Step 1. Read met variables from Ameriflux NetCDF CF in.file

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

  
}

