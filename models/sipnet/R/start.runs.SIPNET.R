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
##' Start SIPNET model runs on local or remote server
##' @title Start SIPNET model runs
##' @name start.runs.SIPNET
##' @export
##' @author Michael Dietze, David LeBauer, Shawn Serbin, Carl Davidson
start.runs.SIPNET <- function(){
  ### TODO: Old code, needs to be updated.  Could can make this a generalized script in
  ### utils package
  
  host     <-  settings$run$host
  
  #Run model from user made bash script 
  if(host$name == 'localhost') {

    ## generate list of runs
    runs <- dir(settings$outdir,full.names=TRUE)
    runs <- runs[file.info(runs)$isdir]
    
    ## run SIPNET for each 
    for(run in runs){
      print(run)
      if("sipnet.in" %in% dir(run)){  ## make sure directory is a SIPNET folder
        system(paste('(cd ', run, '; ',settings$model$binary," )", sep = ''))
      }
      
    }
    
  }else{
    warning("REMOTE RUNNING OF SIPNET NOT YET IMPLEMENTED")
    stop()
    
    ## Priority only needs to be set for very big jobs
    ## priority can be NULL in settings, if null,
    ## only batch.jobs.sh is required to exist
    if(is.null(settings$run$priority)){
      batch.jobs.script <- system.file("inst", "batch.jobs.sh", package="PEcAn.SIPNET")
      
    } else if (as.numeric(settings$run$priority) < 0) {
      batch.jobs.script <- system.file("inst", "batch.jobs.lowp.sh", package="PEcAn.SIPNET")
      
    } else if (as.numeric(settings$run$priority) > 0){
      stop("need admin rights to set higher priority")
    }
    
    ## write runscript that rsyncs at the end
    if(any(grep("SIPNET", settings$run$host$rundir))){
      write.run.SIPNET(settings)
    }
    
    system(paste("echo 'cd ", host$rundir, "' | ",
                 "cat - ", batch.jobs.script, " | ",
                 'ssh -T ', host$name, sep = ''))
  }
  
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
