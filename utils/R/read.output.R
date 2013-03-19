#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Reads the output of a single model run
##'
##' @title Read output
##' @name read.output
##' @param run.id the id distiguishing the model run. 
##' @param outdir the directory that the model's output was sent to
##' @param model name of simulation model currently accepts ("ED", "SIPNET", "BIOCRO")
##' @param start.year first year of output to read (should be greater than ) 
##' @param end.year 
##' @param variables variables to be read from model output
##' @details Generic function to convert model output from model-specific format to 
##' a common PEcAn format. This function uses MsTMIP variables except that units of
##'  (kg m-2 s-1)  are converted to kg ha-1 y-1. Currently this function converts
##' Carbon fluxes: GPP, NPP, NEE, TotalResp, AutoResp, HeteroResp,
##' DOC_flux, Fire_flux, and Stem (Stem is specific to the BioCro model)
##' and Water fluxes: Evaporation (Evap), Transpiration(TVeg),
##' surface runoff (Qs), subsurface runoff (Qsb), and rainfall (Rainf).
##' For more details, see the
##' \href{http://nacp.ornl.gov/MsTMIP_variables.shtml}{MsTMIP variables}
##' documentation 
##' @return vector of output variable
##' @export
##' @author Michael Dietze
read.output <- function(run.id, outdir, model, start.year=NA,
                        end.year=NA, variables = "GPP") {
  do.call(require, list(paste0("PEcAn.", model)))
  model2nc <- paste("model2netcdf", model, sep=".")
  if(!exists(model2nc)){
    logger.warn("File conversion function model2netcdf does not exist for", model)
    return(NA)
  }
  
  cflux = c("GPP", "NPP", "NEE", "TotalResp", "AutoResp", "HeteroResp",
    "DOC_flux", "Fire_flux", "Stem") #kgC m-2 s-1
  wflux = c("Evap", "TVeg", "Qs", "Qsb", "Rainf") #kgH20 m-2 s-1
  
  ## Always call conversion to dangerous,
  ## should do check in conversion code since it knows what gets converted.
  if(tryl(do.call(model2nc, list(outdir)))){
    do.call(model2nc, list(outdir))    
  }
  print(paste("Output from run", run.id, "has been converted to netCDF"))
  ncfiles <- list.files(path=outdir, pattern="\\.nc$", full.names=TRUE)
  if(length(ncfiles) == 0){
    logger.stop("Conversion of model files to netCDF unsuccessful")
  }

  ## determine years to load
  nc.years = as.numeric(sub(paste(run.id,".",sep=""),"",
    sub(".nc","",basename(ncfiles), fixed = TRUE), fixed = TRUE))
  first <- max(1, which(nc.years == start.year), na.rm = TRUE)
  last <- min(length(nc.years),which(nc.years == end.year),na.rm=TRUE)

  if (model == "BIOCRO") {
    nc.years <- list(last)
    yrs <- last
  }
  
  ## load files
  yrs <- first:max(first,last)

  data <- list()
  
  for(i in 1:length(yrs)){
    print(paste("----- Processing year: ", nc.years[yrs[i]]))
    nc <- open.ncdf(ncfiles[yrs[i]], verbose=FALSE)
    for(j in 1:length(variables)){
      if(variables[j] %in% names(nc$var)){
        newdata <- get.var.ncdf(nc, varid=variables[j], verbose=FALSE)
        if(variables[j] %in% c(cflux, wflux)){
          ## Convert output to annual values.
          ## Multiply by seconds in a 365d year and convert per ha
          newdata <- newdata * 31536000 * 10000 # kgC or kgH2O / ha
        }
        if(i == 1) {
          data[[j]] <- newdata
        } else {
          data[[j]] <- c(data[[j]], newdata)
        }
      } else {
        warning(paste(variables[j], "missing in", ncfiles[yrs[i]]))
      }
    }
    close.ncdf(nc)
    showConnections(all = TRUE)
  }
  names(data) <- variables
  print(paste("----- Mean ", variables, " : ",
              sapply(data, median, na.rm = TRUE)))
  print(paste("----- Median ", variables, ": ",
              sapply(data, median, na.rm = TRUE)))
  return(data)   
}

#--------------------------------------------------------------------------------------------------#
##' Reads the output of all model runs
##'
##' @title Read outputs
##' @name read.outputs
##' @param model name of simulation model currently accepts ("ED", "SIPNET", "BIOCRO")
##' @param settings settings loaded from pecan.xml
##' @export
##' @author Rob Kooper
read.outputs <- function(model, settings) {
  for (runid in readLines(con=file.path(settings$rundir, "runs.txt"))) {
    read.output(runid, file.path(settings$run$host$outdir, runid), model)
  }
}
#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
