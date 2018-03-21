#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

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
##' @param end.year last year of output to read. Year is a four character string. (Ex: "2006"or "1875") 
##' @param variables variables to be read from model output
##' @param dataframe A boolean that will return output in a data.frame format with a posix column. Useful for align.data and plotting. 
##' @param pft.name character string, name of the pft to read pft-specific output. If NULL no pft-specific output will be read even the variable has pft as a dimension.
##' @return vector of output variable
##' @export
##' @author Michael Dietze, David LeBauer
read.output <- function(runid, outdir, start.year = NA, end.year = NA, variables = "GPP", dataframe= FALSE, pft.name = NULL) {
  
  ## vars in units s-1 to be converted to y-1 cflux = c('GPP', 'NPP', 'NEE',
  ## 'TotalResp', 'AutoResp', 'HeteroResp', 'DOC_flux', 'Fire_flux') # kgC m-2 s-1
  ## wflux = c('Evap', 'TVeg', 'Qs', 'Qsb', 'Rainf') # kgH20 m-2 d-1
  
  # create list of *.nc years
  nc.years <- as.vector(unlist(strsplit(list.files(path = outdir, pattern = "\\.nc$", 
                                                   full.names = FALSE), ".nc")))
  ncfiles <- list.files(path = outdir, pattern = "\\.nc$", full.names = TRUE)
  
  if(!is.na(start.year) && !is.na(end.year)){
    # select only those *.nc years requested by user
    keep <- which(as.numeric(nc.years) >= as.numeric(start.year) & as.numeric(nc.years) <= as.numeric(end.year))
    ncfiles <- ncfiles[keep]
  } else if(length(nc.years) != 0){
      PEcAn.logger::logger.info("No start or end year provided; reading output for all years")
      start.year <- min(nc.years)
      end.year <- max(nc.years)
  }
  
  # throw error if no *.nc files selected/availible
  nofiles <- FALSE
  if (length(ncfiles) == 0) {
    PEcAn.logger::logger.warn("read.output: no netCDF files of model output present for runid = ", 
                runid, " in ", outdir, " for years requested; will return NA")
    if (length(nc.years) > 0) {
      PEcAn.logger::logger.info("netCDF files for other years present", nc.years)
    }
    nofiles <- TRUE
  } else {
    PEcAn.logger::logger.info("Reading output for Years: ", start.year, " - ", end.year, 
                "in directory:", outdir,
                "including files", basename(ncfiles))
  }
  
  if(dataframe==TRUE){ #ensure that there is a time component when asking for a dataframe + posix code
    if(length(variables[variables=="time"])==0){
      variables<-c(variables, "time")
      PEcAn.logger::logger.info("No time variable requested, adding automatically")
    }
  }
  result <- list()

  if (!nofiles) {
    for (ncfile in ncfiles) {
      nc <- ncdf4::nc_open(ncfile)
      for (v in variables) {
        if (v %in% c(names(nc$var), names(nc$dim))) {
          newresult <- ncdf4::ncvar_get(nc, v)
          # begin per-pft read
          # check if the variable has 'pft' as a dimension
          if("pft" %in% sapply(nc$var[[v]]$dim, `[[`, "name")){
            # means there are PFT specific outputs we want
            # the variable *PFT* in standard netcdfs has *pft* dimension, 
            # numbers as values, and full pft names as an attribute
            # parse pft names and match the requested
            pft.string <- ncdf4::ncatt_get(nc, "PFT")
            pft.ind <- strsplit(pft.string$long_name, ",")[[1]] == pft.name
            # dimensions can differ from model to model or run to run
            # there might be other cases that are not covered here
            dim.check <- length(dim(newresult))
            if(any(pft.ind)){ # means pft.name passed, we want to read pft-specific outputs
              if(dim.check == 1){
                newresult <- newresult[pft.ind] 
              }else{
                newresult <- newresult[,pft.ind] 
              }
            }else{ 
              # means this variable is available as per-pft, so written as such to standard ncdf files
              # but we still want to read as total
              if(dim.check == 1){
                newresult <- sum(newresult)
              }else{
                newresult <- apply(newresult,1,sum)
              }
            }
          } # end of per-pft read
          
          # Dropping attempt to provide more sensible units because of graph unit errors,
          # issue #792 if(v %in% c(cflux, wflux)){ newresult <- udunits2::ud.convert(newresult, 'kg
          # m-2 s-1', 'kg ha-1 yr-1') }
          result[[v]] <- abind::abind(result[[v]], newresult)
        } else if (!(v %in% names(nc$var))) {
          PEcAn.logger::logger.warn(paste(v, "missing in", ncfile))
        }
      }
      ncdf4::nc_close(nc)
    }
  } else if (nofiles) {
    result <- lapply(variables, function(x) NA)
  }
  
  PEcAn.logger::logger.info(
    variables,
    "Mean:", lapply(result, function(x) signif(mean(x, na.rm = TRUE), 3)),
    "Median:", lapply(result, function(x) signif(stats::median(x, na.rm = TRUE), 3)))
  
  if(dataframe==FALSE){
  return(result)
  }else if (dataframe==TRUE){
    
    # Check if there are variables that have multiple dimensions 
    # for example soil moisture at multiple levels.
    # Currently we don't have a consensus how to convert these to dataframe format
    # so they should be omitted. 
    
    for(var in names(result)){
      c <- dim(result[[var]])[2]
      r <- dim(result[[var]])[1]
      if(!is.na(c) & r > 1){
        PEcAn.logger::logger.warn("Variable", var, "has", r, "dimensions,
      it cannot be loaded and will be omitted.")
        result[[var]] <- NULL 
      }
    }
    
    model <- as.data.frame(result) # put into a data.frame
    ########## add in posix column ###########
    
    if(assertthat::is.date(start.year)==TRUE){
      start_year<-start.year
      end_year<-end.year
      
    }else if(assertthat::is.date(start.year)==FALSE){
      origin<-paste0(as.character(start.year),"-01-01")
      start_year = start.year
      end_year = end.year 
    }else{
      PEcAn.logger::logger.error("Start and End year must be of type numeric, character or Date")
    }
    years <- start_year:end_year
    seconds <- udunits2::ud.convert(model$time,"years","seconds")
    Diff <- diff(model$time)
    time_breaks = which(Diff < 0)
    
    if(length(time_breaks) == 0 & length(years)>1){
      model$posix <- as.POSIXct(model$time*86400,origin= origin,tz="UTC")
      model$year <- lubridate::year(model$posix)
      return(model)
    } else {
      N <- c(0,time_breaks, length(model$time))
      n <- diff(N)
      y <- c()
      for (i in seq_along(n)) {
        y <- c(y, rep(years[i], n[i]))
      }
      
      model$year <- y
      makeDate <- function(x){as.POSIXct(model$time[x]*86400,origin=paste0(model$year[x],"-01-01"),tz="UTC")}
      model$posix <- makeDate(seq_along(model$time))
      return(model)
    }
  }else{
    PEcAn.logger::logger.error("Error in dataframe variable. Dataframe boolean must be set to TRUE or FALSE")
  }
  
} # read.output


##'--------------------------------------------------------------------------------------------------#
##' Converts the output of all model runs
##'
##' @title convert outputs from model specific code to 
##' @name convert.outputs
##' @param model name of simulation model currently accepts ('ED', 'SIPNET', 'BIOCRO')
##' @param settings settings loaded from pecan.xml
##' @param ... arguments passed to \code{\link{read.output}}, e.g. \code{variables}, \code{start.year}, \code{end.year}
##' @export
##' @author Rob Kooper
convert.outputs <- function(model, settings, ...) {
  PEcAn.logger::logger.severe("This function is not longer used and will be removed in the future.")
} # convert.outputs
