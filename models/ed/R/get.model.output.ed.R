#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Extract ED output for specific variables from an hdf5 file
##' @name read.output.file.ed
##' @title read output - ED
##' @param filename string, name of file with data
##' @param variables  variables to extract from file
##' @return single value of output variable from filename. In the case of AGB, it is summed across all plants
##' @export
##' @author David LeBauer, Carl Davidson
read.output.file.ed <- function(filename, variables = c("AGB_CO", "NPLANT")){
  if(filename %in% dir(pattern = 'h5')){
    Carbon2Yield = 20
    nc <- nc_open(filename)
    if(all(c("AGB_CO", "NPLANT") %in% variables)) {
      result <- (sum(ncvar_get(nc,'AGB_CO') * ncvar_get(nc,'NPLANT'), na.rm =TRUE) * Carbon2Yield)
    } else {
      result <- sum(sapply(variables, function(x){sum(ncvar_get(nc, x))}))
    }
    nc_close(nc)
    return(result)
  } else {
    return(NA)
  }
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##' Reads the output of a single model run
##'
##' This function applies \link{read.output.file.ed} to a list of files from a single run
##' @title Read ED output
##' @name read.output.ED2
##' @param run.id the id distiguishing the model run
##' @param outdir the directory that the model's output was sent to
##' @param start.year 
##' @param end.year
##' @param output.type type of output file to read, can be "-Y-" for annual output, "-M-" for monthly means, "-D-" for daily means, "-T-" for instantaneous fluxes. Output types are set in the ED2IN namelist as NL%I[DMYT]OUTPUT  
##' @return vector of output variable for all runs within ensemble
##' @export
##' @author David LeBauer, Carl Davidson
read.output.ED2 <- function(run.id, outdir, start.year=NA, end.year=NA, variables=c("AGB_CO", "NPLANT"), output.type = 'Y'){
  print(run.id)
  #if(any(grep(run.id, dir(outdir, pattern = 'finished')))){
  file.names <- dir(outdir, pattern=run.id, full.names=FALSE)
  file.names <- grep(paste('-', output.type, '-', sep = ''), file.names, value = TRUE)
  file.names <- grep('([0-9]{4}).*', file.names, value=TRUE)
  if(length(file.names) > 0) {
    years <- sub('((?!-Y-).)*-Y-([0-9]{4}).*', '\\2', file.names, perl=TRUE)
    if(!is.na(start.year) && nchar(start.year) ==  4){
      file.names <- file.names[years>=as.numeric(start.year)]
    }
    if(!is.na(end.year) && nchar(end.year) == 4){
      file.names <- file.names[years<=as.numeric(end.year)]
    }
    file.names <- file.names[!is.na(file.names)]
    print(file.names)
    
    result <- mean(sapply(file.names, read.output.file.ed,variables)) ## if any are NA, NA is returned

  } else {
    warning(cat(paste('no output files in', outdir, '\nfor', run.id, '\n')))
    result <- NA
  }
  #} else {
  #  warning(cat(paste(run.id, 'not finished \n')))
  #  result <- NA
  #}

  return(result)
}
#==================================================================================================#



#--------------------------------------------------------------------------------------------------#
##' Function to retrieve ED2 HDF model output from local or remote server
##'
##' @name get.model.output.ED2
##' @title Retrieve ED2 HDF model output from local or remote server
##' 
##' @import PEcAn.utils
##' @export
##'
##' @author Shawn Serbin
##' @author David LeBauer
get.model.output.ED2 <- function(settings){
  model <- settings$model$type
  
  ### Get ED2 model output on the localhost
  if(settings$run$host$name == 'localhost'){
    #setwd(settings$run$host$outdir)  # Host model output directory
    get.results(settings)
    ### Move required functions to host
    ## TODO: take out functions read.output.file.ed & read.output.ed from write.configs.ed &
    ## put into a new file specific for reading ED output
    
    ### Is the previous necessary for localhost?  These functions should be availible within R
    ### & should not need to be copied and run but could instead be called within the running R
    ### shell.  SPS
    
    #setwd(settings$outdir)
    #source('PEcAn.functions.R') # This won't work yet
    

  } else {
    ### Make a copy of the settings object for use on the remote sever
    save(settings,file=paste(settings$outdir,"settings.Rdata",sep=""))
    
    ### Make a copy of required functions and place in file PEcAn.functions.R
    dump(c("get.run.id", "left.pad.zeros", "read.ensemble.output",
           "read.sa.output", "read.output", "model2netcdf.ED2", "get.results"),
         file=paste(settings$outdir,"PEcAn.functions.R",sep=""))
    
    ### Add execution of get.results to the end of the PEcAn.functions.R file
    ### This will execute all the code needed to extract output on remote host
    ### --- added loading of pecan settings object
    cat('load("settings.Rdata")',file=paste(settings$outdir,"PEcAn.functions.R",sep=""),
        append=TRUE)
    cat("\n",file=paste(settings$outdir,"PEcAn.functions.R",sep=""),
        append=TRUE)
    cat("get.results(settings)",file=paste(settings$outdir,"PEcAn.functions.R",sep=""),
        append=TRUE)

    ### Copy required PEcAn.functions.R and settings object to remote host
    rsync('-outi',paste(settings$outdir,"settings.Rdata",sep=""),
          paste(settings$run$host$name, ':',settings$run$host$outdir, sep = '') )
    rsync('-outi',paste(settings$outdir,"PEcAn.functions.R",sep=""),
          paste(settings$run$host$name, ':',settings$run$host$outdir, sep = '') )

    ### Run script on remote host
    system(paste("ssh -T", settings$run$host$name, "'",
             "cd", settings$run$host$outdir, "; R --vanilla < PEcAn.functions.R'"))
    
    ### Get PEcAn output from remote host
    rsync('-outi', from = paste(settings$run$host$name, ':', settings$run$host$outdir, 'output.Rdata', sep=''),
      to = settings$outdir)

  } ### End of if/else
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
