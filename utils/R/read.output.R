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
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @param start.year 
##' @param end.year
##' @param variables
##' @param model
##' @return vector of output variable
##' @export
##' @author Michael Dietze
read.output <- function(run.id, outdir, start.year=NA, end.year=NA, variables = "GPP") {
  ### Load requirements
  require(ncdf)
  
  model <- settings$model$name
  model2nc <- paste("model2netcdf", model, sep=".")
  if(!exists(model2nc)){
    log.warn("File conversion function model2netcdf does not exist for",model)
    return(NA)
  }
  
  cflux = c("GPP","NPP","NEE","TotalResp","AutoResp","HeteroResp","DOC_flux","Fire_flux","Stem") #kgC m-2 s-1
  wflux = c("Evap","TVeg","Qs","Qsb","Rainf") #kgH20 m-2 s-1
  
  ### ----- Get run info ----- 
  ## get list of files
  if (model=="SIPNET"){
     outfiles <- c(file.path(outdir, "sipnet.out"))
   } else if (model=="ED2"){
     outfiles <- list.files(path=outdir, pattern="analysis-T-.*\\.h5$", full.names=TRUE)
   } else if (model %in% c("BIOCRO", "C4PHOTO")) {
     outfiles <-  c(file.path(outdir, "result.Rdata"))
  } else {
    stop(paste("Don't know how to convert output for model", model))
  }
  print(outfiles)

  ## model-specific code to parse each file 
  if(length(outfiles) > 0) {
    ## Always call conversion to dangerous, should do check in conversion code since it knows what gets converted.
    do.call(model2nc, list(outdir))
    print(paste("Output from run", run.id, "has been converted to netCDF"))
    ncfiles <- list.files(path=outdir, pattern="\\.nc$", full.names=TRUE)
    if(length(ncfiles) == 0){
      log.error("Conversion of model files to netCDF unsuccessful")
      stop("Conversion of model files to netCDF unsuccessful")
    }

    ## determine years to load
    if(model != "C4PHOTO"){
      nc.years = as.numeric(sub(paste(run.id,".",sep=""),"",
        sub(".nc","",basename(ncfiles), fixed = TRUE), fixed = TRUE))
      first <- max(1, which(nc.years == start.year), na.rm = TRUE)
      last <- min(length(nc.years),which(nc.years == end.year),na.rm=TRUE)
      
      ## load files
      yrs <- first:max(first,last)
    } else if (model %in% c("BIOCRO", "C4PHOTO")) {
      nc.years <- list(1)
      yrs <- 1
    }

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
            newdata <- newdata*31536000*10000 # kgC/ha
          }
          if(i == 1) {
            data[[j]] <- newdata
          } else {
            data[[j]] <- c(data[[j]],newdata)
          }
        } else {
          warning(paste(variables[j], "missing in", ncfiles[yrs[i]]))
        }
      }
      close.ncdf(nc)
      showConnections(all = TRUE)
    }
    names(data) <- variables
    print(paste("----- Mean ",variables," : ",sapply(data,median,na.rm=TRUE)))
    print(paste("----- Median ",variables,": ",sapply(data,median,na.rm=TRUE)))
    return(data)   

  } else {
    log.error("No output files present for", model)
  }

  return(NA) 
}

#--------------------------------------------------------------------------------------------------#
##' Reads the output of all model runs
##'
##' @title Read outputs
##' @name read.outputs
##' @export
##' @author Rob Kooper
read.outputs <- function() {
  for (run in readLines(con=file.path(settings$rundir, "runs.txt"))) {
    read.output(run, file.path(settings$run$host$outdir, run))
  }
}
#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
