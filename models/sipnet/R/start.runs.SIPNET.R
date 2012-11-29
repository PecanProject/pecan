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
##' Start SIPNET model runs on local server
##' @title Start SIPNET model runs
##' @name start.runs.SIPNET
##' @param runid the id of the run that needs to be executed
##' @export
##' @author Michael Dietze, David LeBauer, Shawn Serbin, Carl Davidson
start.runs.SIPNET <- function(runid){
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  cwd <- getwd()
  setwd(rundir)
  system2(settings$model$binary)
  file.rename(file.path(rundir, "sipnet.out"), file.path(outdir, "sipnet.out"))
  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
  setwd(cwd)

  if (FALSE) {
    #Run model from user made bash script 
    if(host$name == 'localhost') {

      ## generate list of runs
      runs <- dir(settings$outdir,full.names=TRUE)
      runs <- runs[file.info(runs)$isdir]
      
      ### Eliminate non-run dirs --------------------------------------------

  ## This one is removing all the SA runs (since the pft name is in the SA name)    
  #    pft.dir <- strsplit(settings$pfts$pft$outdir,"/")[[1]]
  #    ln <- length(pft.dir)
  #    pft.dir <- pft.dir[ln]
  #    runs <- runs[-grep(pft.dir,runs)]

  ## these two are removing all the run directories    
  #    run.dir <- strsplit(settings$run$host$rundir,"/")[[1]]
  #    ln <- length(run.dir)
  #    run.dir <- run.dir[ln]
  #    runs <- runs[-grep(run.dir,runs)]
      
  #    out.dir <- strsplit(settings$run$host$outdir,"/")[[1]]
  #    ln <- length(out.dir)
  #    out.dir <- out.dir[ln]
  #    runs <- runs[-grep(paste("/",out.dir,sep=""),runs)]
      ### Done -------------------------------------------------------------
      
      ## run SIPNET for each 
      for(run in runs){
        print(paste("---- SIPNET model run: ",run,sep=""))
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
  }
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
