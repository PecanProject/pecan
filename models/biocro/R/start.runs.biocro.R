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
##' Start biocro model runs on local or remote server
##' @title Start biocro model runs
##' @name start.runs.biocro
##' @export
##' @author David LeBauer, Deepak Jaiswal
start.run.biocro <- function(run.id){
  print(paste("---- biocro model run: ", run.id, sep=""))
  rundir <- file.path(settings$outdir, run.id)
  setwd(rundir)
  config <- xmlToList(xmlParse(run.id))


################ Read  Year and Location Details and derive model input############
  lat <- as.numeric(settings$run$site$lat)
  lon <- as.numeric(settings$run$site$lon)
  dateofplanting <- ymd_hms(settings$run$start.date)
  dateofharvest <- ymd_hms(settings$run$end.date)

  
  weather <- InputForWeach(lat, lon, year(dateofplanting), year(dateofharvest))
  pp <- do.call(photoParms, list(unlist(config$parms)))
  result <- BioGro(weather05, photoControl = pp)
  save(result, file=paste(run.id,".Rdata",sep=""))
}



##' Function to start all runs of biocro model in directory
##' @title Start run of biocro model
##' @export
##' @return nothing, starts run as side effect
##' @author David LeBauer
start.runs.biocro <- function(){
  host     <-  settings$run$host
  
  ## Run model from user Rscript 
  if(host$name == 'localhost') {
    ## find directories in rundir
    isrun <- file.info(dir(settings$outdir, full.names = TRUE))$isdir
    runs <- dir(settings$outdir)[isrun]
    ## run biocro for each 
    for(run.id in runs){
      start.run.biocro(run.id)
    }    
  }else{
    warning("Execution biocro on Remote Server NOT YET IMPLEMENTED")
    stop() 
  }
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
