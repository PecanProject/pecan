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

load.data <- function(data.path, format, start_year = NA, end_year=NA, site=NA, vars_used){
  
  require(PEcAn.benchmark)
  require(lubridate)
  require(udunits2)
  
  # Determine the function that should be used to load the data 
  fcn1 <- paste0("load.",format$file_name)
  fcn2 <- paste0("load.",format$mimetype)
  if(exists(fcn1)){
    fcn <- match.fun(fcn1)
  }else if(exists(fcn2)){
    fcn <- match.fun(fcn2)
  }else{
    logger.warn("no load data for current mimetype - converting using browndog")
  }
  
  loaded <- fcn(data.path, format, site, vars_used$orig_name)
  
  out <- loaded
  # Convert loaded data to the same standard varialbe names and units
  
  for(i in 1:nrow(vars_used)){
    col <- names(out)==vars_used$orig_name[i]
    if(vars_used$orig_units[i] == vars_used$pecan_units[i]){
      print("match")
      colnames(out)[col] <- vars_used$pecan_name[i]
    }else{
      print(paste("convert", vars_used$orig_name[i]))
      x <- as.matrix(out[col])
      u1 = vars_used$orig_units[i]
      u2 = vars_used$pecan_units[i]
      print(u1)
      print(u2)
      if(udunits2::ud.are.convertible(u1,u2)){
        out[col] <- udunits2::ud.convert(x,u1,u2)
        colnames(out)[col] <- vars_used$pecan_name[i]
      }else{logger.error("Units cannot be converted")} #This error should probably be thrown much earlier, like in query.format.vars - will move it eventually
    }
  }
  
  return(out) 
}

##' Future things to think about
##'   - error estimates
##'   - QAQC
##'   - STEPPS -> cov
##'   - MCMC samples
##'   - "data products" vs raw data
##'   - Is there a generic structure to ovbs?
##' 