##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html

##---------------------------------------------------------------------------------

##' Convert output for a single model run to NetCDF
##'
##' DEPRECATED this function will be removed in future versions, please update
##' your workflow.
##'
##' This function is a wrapper for model-specific conversion functions,
##' e.g. \code{model2netcdf.ED2}, \code{model2netcdf.BIOCRO}.
##' @title Convert model output to NetCDF 
##' @param runid 
##' @param outdir
##' @param model name of simulation model currently accepts ("ED2", "SIPNET", "BIOCRO")
##' @param lat Latitude of the site
##' @param lon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @return vector of filenames created, converts model output to netcdf as a side effect
##' @author Mike Dietze, David LeBauer
model2netcdfdep <- function(runid, outdir, model, lat, lon, start_date, end_date){
  ## load model-specific PEcAn module
  do.call(require, list(paste0("PEcAn.", model)))    


  model2nc <- paste("model2netcdf", model, sep=".")
  if(!exists(model2nc)){
    logger.warn("File conversion function model2netcdf does not exist for", model)
    return(NA)
  }
  
  do.call(model2nc, list(outdir, lat, lon, start_date, end_date))    
  
  print(paste("Output from run", runid, "has been converted to netCDF"))
  ncfiles <- list.files(path = outdir, pattern="\\.nc$", full.names=TRUE)
  if(length(ncfiles) == 0){
    logger.severe("Conversion of model files to netCDF unsuccessful")
  }
  return(ncfiles)
}

##' Convert output for a single model run to NetCDF
##'
##' DEPRECATED this function will be removed in future versions, please update
##' your workflow.
##'
##' This function is a wrapper for model-specific conversion functions,
##' e.g. \code{model2netcdf.ED2}, \code{model2netcdf.BIOCRO}.
##' @title Convert model output to NetCDF 
##' @param runid 
##' @param outdir
##' @param model name of simulation model currently accepts ("ED2", "SIPNET", "BIOCRO")
##' @param lat Latitude of the site
##' @param lon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @return vector of filenames created, converts model output to netcdf as a side effect
##' @author Mike Dietze, David LeBauer
model2netcdf <- function(runid, outdir, model, lat, lon, start_date, end_date){
  logger.severe("model2netcdf will be removed in future versions, plase update your worklow")
}


##' Reads the output of a single model run
##'
##' Generic function to convert model output from model-specific format to 
##' a common PEcAn format. This function uses MsTMIP variables except that units of
##'  (kg m-2 d-1)  are converted to kg ha-1 y-1. Currently this function converts
##' Carbon fluxes: GPP, NPP, NEE, TotalResp, AutoResp, HeteroResp,
##' DOC_flux, Fire_flux, and Stem (Stem is specific to the BioCro model)
##' and Water fluxes: Evaporation (Evap), Transpiration(TVeg),
##' surface runoff (Qs), subsurface runoff (Qsb), and rainfall (Rainf).
##' For more details, see the
##' \href{http://nacp.ornl.gov/MsTMIP_variables.shtml}{MsTMIP variables}
##' documentation 
##' @title Read model output
##' @name read.output
##' @param runid the id distiguishing the model run. 
##' @param outdir the directory that the model's output was sent to
##' @param start.year first year of output to read (should be greater than ) 
##' @param end.year last year of output to read
##' @param variables variables to be read from model output
##' @return vector of output variable
##' @export
##' @author Michael Dietze, David LeBauer
read.output <- function(runid, outdir, start.year=NA,
                        end.year=NA, variables = "GPP") {

  ## vars in units s-1 to be converted to y-1
  cflux = c("GPP", "NPP", "NEE", "TotalResp", "AutoResp", "HeteroResp",
    "DOC_flux", "Fire_flux") # kgC m-2 d-1
  wflux = c("Evap", "TVeg", "Qs", "Qsb", "Rainf") # kgH20 m-2 d-1
  
  # create list of *.nc years
  nc.years <- as.vector(unlist(strsplit(list.files(path = outdir, pattern="\\.nc$", full.names=FALSE),".nc")))
  # select only those *.nc years requested by user
  keep <- which(nc.years >= as.numeric(start.year) & nc.years <= as.numeric(end.year))
  ncfiles <- list.files(path = outdir, pattern="\\.nc$", full.names=TRUE)
  ncfiles <- ncfiles[keep]
  # throw error if no *.nc files selected/availible
  if(length(ncfiles) == 0) logger.error("no netCDF files of model output present")
  
  print(paste("Years: ",start.year," - ",end.year),sep="")
  result <- list()
  for(ncfile in ncfiles) {
    nc <- nc_open(ncfile)
    for(v in variables){
      if(v %in% c(names(nc$var),names(nc$dim))){
        newresult <- ncvar_get(nc, v)
        if(v %in% c(cflux, wflux)){
          newresult <- ud.convert(newresult, "kg m-2 s-1", "kg ha-1 yr-1")
        }
        result[[v]] <- abind(result[[v]], newresult)
      } else if (!(v %in% names(nc$var))){
        logger.warn(paste(v, "missing in", ncfile))
      }
    }
    nc_close(nc)
  }
  
  print(paste("----- Mean ", variables, " : ",
              lapply(result, mean, na.rm = TRUE)))
  print(paste("----- Median ", variables, ": ",
              lapply(result, median, na.rm = TRUE)))
  return(result)
}

##'--------------------------------------------------------------------------------------------------#
##' Converts the output of all model runs
##'
##' @title convert outputs from model specific code to 
##' @name convert.outputs
##' @param model name of simulation model currently accepts ("ED", "SIPNET", "BIOCRO")
##' @param settings settings loaded from pecan.xml
##' @param ... arguments passed to \code{\link{read.output}}, e.g. \code{variables}, \code{start.year}, \code{end.year}
##' @export
##' @author Rob Kooper
convert.outputs <- function(model, settings, ...) {
  logger.severe("This function is not longer used and will be removed in the future.")
}
####################################################################################################
