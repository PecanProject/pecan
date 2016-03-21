##' @name load.data
##' @title load.data
##' @export
##' @param data.path
##' @param format
##' @param start_year
##' @param end_year
##' @param site
##' @author Betsy Cowdery

##' Generic function to convert input files containing observational data to 
##' a common PEcAn format. 
##' 
##' This should be the same as the read.output function:
##' This function uses MsTMIP variables except that units of (kg m-2 d-1)  
##' are converted to kg ha-1 y-1. 
##' 
##' Currently this function converts
##' 
##' Carbon fluxes: GPP, NPP, NEE, TotalResp, AutoResp, HeteroResp,
##' DOC_flux, Fire_flux, and Stem (Stem is specific to the BioCro model)
##' 
##' Water fluxes: Evaporation (Evap), Transpiration(TVeg),
##' surface runoff (Qs), subsurface runoff (Qsb), and rainfall (Rainf).  

load.data <- function(data.path, format, start_year = NA, end_year=NA, site=NA){
  
  require(PEcAn.benchmark)
  require(lubridate)
  
  fcn1 <- paste0("load.",format$file_name)
  fcn2 <- paste0("load.",format_table$mimetype)  	 +  fcn2 <- paste0("load.",format$mimetype)
  if(exists(fcn1)){
    fcn <- match.fun(fcn1)
  }else if(exists(fcn2)){
    fcn <- match.fun(fcn2)
  }else{
    logger.warn("no load data for current mimetype - converting using browndog")
    # Browndog
    # convert the observations to a mime pecan can use
    # ex: exel -> csv
  }
  
  result <- fcn(data.path, format, start_year, end_year, site)

  return(result) 
}

##' Future things to think about
##'   - error estimates
##'   - QAQC
##'   - STEPPS -> cov
##'   - MCMC samples
##'   - "data products" vs raw data
##'   - Is there a generic structure to ovbs?
##' 