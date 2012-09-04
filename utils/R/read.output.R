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
read.output <- function(run.id, outdir, start.year=NA, end.year=NA,variables="GPP",model=model){
    
  ### Load requirements
  require(ncdf)
  
  model2nc = paste("model2netcdf",model,sep=".")
  if(!exists(model2nc)){
    warning(paste("File conversion function model2netcdf does not exist for",model))
  }
  
  cflux = c("GPP","NPP","NEE","TotalResp","AutoResp","HeteroResp","DOC_flux","Fire_flux") #kgC m-2 s-1
  wflux = c("Evap","TVeg","Qs","Qsb","Rainf") #kgH20 m-2 s-1
  
  ### ----- Get run info ----- 
  ## get list of files
  if (model=="SIPNET"){
     file.names <- dir(paste(outdir,run.id,sep="/"), pattern=run.id, full.names=TRUE)
     outfiles <- list.files(path=paste(outdir,run.id,sep="/"),pattern="\\.out$",full.names=TRUE)
   } else if (model=="ED2"){
                                        #file.names <- dir(outdir, pattern=paste(run.id,"\\.h5$",sep=""), full.names=TRUE)
     file.names <- dir(outdir, pattern=run.id, full.names=TRUE)
     file.names <- file.names[grep("\\.h5$",file.names)]
     outfiles <- list.files(path=outdir,pattern=run.id,full.names=TRUE)
     outfiles <- outfiles[grep("\\.h5$",outfiles)]
   } else if (model == "c4photo") {
     file.names <- dir(paste(outdir, run.id, sep = "/"),
                       pattern = paste(run.id, ".csv", sep = ""),
                       full.names = TRUE)
     outfiles <- list.files(path = outdir, pattern = run.id, full.names = TRUE)
     outfiles <- outfiles[grep("\\.csv", outfiles)]
  }
  ## model-specific code to parse each file 
  if(length(file.names) > 0) {

    ## subset output files
    if (model=="SIPNET"){
       ncfiles <- list.files(path=paste(outdir,run.id,sep="/"),pattern="\\.nc$",full.names=TRUE) ## previous was failing on filenames that have "nc" within them, for some reason? SPS    
     } else if (model=="ED2"){
       ## ncfiles <- list.files(path=outdir,pattern="\\.nc$",full.names=TRUE)
       ncfiles <- list.files(path=outdir,pattern=run.id,full.names=TRUE)
       ncfiles <- ncfiles[grep("\\.nc$",ncfiles)]
     } else if (model == "c4photo") {
       ncfiles <- list.files(path = outdir,
                             pattern = paste(run.id, "\\.nc", sep = "/"),
                             full.names = TRUE)
     }
    
    ## check that there are output files
    if(length(ncfiles) | length(outfiles)){

      ## if files have not been converted yet, convert to standard format
      if(length(ncfiles) == 0){
        do.call(model2nc,list(outdir,run.id))
        print(paste("Output from run", run.id, "has been converted to netCDF"))
        if (model %in% c("SIPNET", "c4photo")){
          ncfiles <- list.files(path=paste(outdir, run.id, sep="/"),
                                pattern="\\.nc$",full.names=TRUE)
        } else if (model %in% c("ED2", "c4photo")){
          ncfiles <- list.files(path=outdir, pattern=run.id, full.names=TRUE)
          ncfiles <- ncfiles[grep("\\.nc$",ncfiles)]
        }
        if(length(ncfiles) == 0){
          stop("Conversion of model files to netCDF unsuccessful")
        }
      }

      ## determine years to load
      if(model != "c4photo"){
        nc.years = as.numeric(sub(paste(run.id,".",sep=""),"",
          sub(".nc","",basename(ncfiles), fixed = TRUE), fixed = TRUE))
        first <- max(1, which(nc.years == start.year), na.rm = TRUE)
        last <- min(length(nc.years),which(nc.years == end.year),na.rm=TRUE)
        
        ## load files
        yrs <- first:max(first,last)
      } else if (model == "c4photo") {
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
      }
      names(data) <- variables
      print(paste("----- Median :",sapply(data,median,na.rm=TRUE)))
      return(data)   
    } else {
      stop("no output files present")
    }    
  }
  return(NA) 
}
#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
