#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' 
##' Start c4photo model runs on local or remote server
##' @title Start c4photo model runs
##' @name start.runs.c4photo
##' @export
##' @author Michael Dietze, David LeBauer, Shawn Serbin, Carl Davidson
start.run.c4photo <- function(run.id){
  print(paste("---- c4photo model run: ", run.id, sep=""))
  rundir <- paste(settings$outdir, run.id, sep = "")
  setwd(rundir)
  system(paste(settings$model$binary, run.id))
}
##' Function to start all runs of c4photo model in directory
##' @title Start run of c4photo model
##' @export
##' @return nothing, starts run as side effect
##' @author David LeBauer
start.runs.c4photo <- function(){
  host     <-  settings$run$host
  
  ## Run model from user Rscript 
  if(host$name == 'localhost') {
    ## find directories in rundir
    isrun <- file.info(dir(settings$outdir, full.names = TRUE))$isdir
    runs <- dir(settings$outdir)[isrun]
    ## run c4photo for each 
    for(run.id in runs){
      start.run.c4photo(run.id)
    }    
  }else{
    warning("Execution c4photo on Remote Server NOT YET IMPLEMENTED")
    stop() 
  }
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
