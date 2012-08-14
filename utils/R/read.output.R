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
  }
 
#   print(file.names)  

  #print(file.names)
  #print(outfiles)
  
  ### model-specific code to parse each file 
  if(length(file.names) > 0) {

    #subset output files
    if (model=="SIPNET"){
       ncfiles <- list.files(path=paste(outdir,run.id,sep="/"),pattern="\\.nc$",full.names=TRUE) ## previous was failing on filenames that have "nc" within them, for some reason? SPS    
    } else if (model=="ED2"){
       #ncfiles <- list.files(path=outdir,pattern="\\.nc$",full.names=TRUE)
        ncfiles <- list.files(path=outdir,pattern=run.id,full.names=TRUE)
        ncfiles <- ncfiles[grep("\\.nc$",ncfiles)]
    }
    
    #check that there are output files
    if(length(ncfiles) | length(outfiles)){

      ## if files have not been converted yet, convert to standard format
      if(length(ncfiles) == 0){
        do.call(model2nc,list(outdir,run.id))
        print(list(outdir,run.id))
        #!!! Replaced by SPS
        #ncfiles <- dir(paste(outdir,run.id,sep="/"), pattern=run.id, full.names=TRUE)
        #ncfiles <- ncfiles[grep(".nc",ncfiles)]
        #!!!
        if (model=="SIPNET"){
            ncfiles <- list.files(path=paste(outdir,run.id,sep="/"),pattern="\\.nc$",full.names=TRUE)
        } else if (model=="ED2"){
            #ncfiles <- list.files(path=outdir,pattern="\\.nc$",full.names=TRUE)
            ncfiles <- list.files(path=outdir,pattern=run.id,full.names=TRUE)
            ncfiles <- ncfiles[grep("\\.nc$",ncfiles)]
        }
        if(length(ncfiles) == 0){
          stop("Conversion of model files to netCDF unsuccessful")
        }
      }

      ## determine years to load
      nc.years = as.numeric(sub(paste(run.id,".",sep=""),"",sub(".nc","",basename(ncfiles),fixed=TRUE),fixed=TRUE))
      first <- max(1,which(nc.years==start.year),na.rm=TRUE)
      last <- min(length(nc.years),which(nc.years == end.year),na.rm=TRUE)
      
      ## load files
      yrs <- first:max(first,last)
      data <- list()
      
      for(i in 1:length(yrs)){
        print(paste("----- Processing year: ",nc.years[yrs[i]]))
        nc <- open.ncdf(ncfiles[yrs[i]],verbose=FALSE)
        for(j in 1:length(variables)){
          if(variables[j] %in% names(nc$var)){
            newdata <- get.var.ncdf(nc,varid=variables[j],verbose=FALSE)
            if(variables[j] %in% c(cflux,wflux)){
              ## Convert output to annual values.  Mult by seconds in a 365d year and convert per ha
              newdata <- newdata*31536000*10000 # kgC/ha
            }
            if(i == 1){
              data[[j]] = newdata
            }else{
              data[[j]] = c(data[[j]],newdata)
            }
          } else {
            warning(paste(variables[j],"missing in",ncfiles[yrs[i]]))
          }
        }
        close.ncdf(nc)
      }
      names(data) <- variables
      #print(summary(data))
      print(paste("----- Median :",sapply(data,median,na.rm=TRUE)))
      #print(str(data))
      #print(data)
      return(data)
   
    } else {
      stop("no output files present")
    }
    
  }
  return(NA) 
}
#==================================================================================================#

#### BELOW IS DEPRECIATED
#--------------------------------------------------------------------------------------------------#
# Returns list of ensemble output
#
# @name read.ensemble.output
# @title Read Ensemble Output
# @return list of ensemble output 
# @export
#

### THIS FUNCTION IS DEFINED TWICE (also in utils.R), this is NOT the one that's called

#read.ensemble.output <- function(ensemble.size, host, outdir, pft.name='',
#                                 start.year=NA,end.year=NA,variables=NA,model=model){
#  my.read.output <- paste("read.output",model,sep=".")  
#  ensemble.output <- list()
#  rsync(paste(host$name, ':', host$outdir, 
#              '*', get.run.id('ENS', '', pft.name=pft.name), '*', sep=''),
#        outdir)
#  for(ensemble.id in seq(ensemble.size)) {
#    run.id <- get.run.id('ENS', left.pad.zeros(ensemble.id, 5), pft.name=pft.name)#log10(ensemble.size)+1))
#   # ensemble.output[[ensemble.id]] <- read.output(run.id, outdir)
#    ensemble.output[[ensemble.id]] <- do.call(my.read.output,args=list(run.id, outdir,start.year,end.year,variables))
#  }
#  return(ensemble.output)
#}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
# Read output from sensitivity runs
# 
# @name read.sa.output
# @title Read SA output
# @return dataframe with one col per quantile analysed and one row per trait,
#  each cell is a list of model output over time
# @export
#

### THIS FUNCTION IS DEFINED TWICE (also in utils.R), this is NOT the one that's called

#read.sa.output <- function(traits, quantiles, host, outdir, pft.name='', start.year=NA,end.year=NA,variables=NA,model=model){
#  my.read.output <- paste("read.output",model,sep=".")
#  sa.output <- data.frame()
#  rsync(paste(host$name, ':', host$outdir, 
#              '*', get.run.id('SA', '', pft.name=pft.name), '*', sep=''),
#        outdir)
#  for(trait in traits){
#    for(quantile in quantiles){
#      run.id <- get.run.id('SA', round(quantile,3), trait=trait, pft.name=pft.name)
#      #sa.output[as.character(round(quantile*100,3)), trait] <- read.output(run.id, outdir)
#      sa.output[as.character(round(quantile*100,3)), trait] <- do.call(my.read.output,args=list(run.id, outdir,start.year,end.year,variables))
#    }
#  }
#  #sa.output['50',] <- read.output(get.run.id('SA', 'median'), outdir)
#  sa.output['50',] <- do.call(my.read.output,args=list(get.run.id('SA', 'median'), outdir,start.year,end.year,variables))
#  return(sa.output)
#}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
