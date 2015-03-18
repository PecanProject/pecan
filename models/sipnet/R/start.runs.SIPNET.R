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
##' @export start.runs.SIPNET
##' @author Michael Dietze, David LeBauer, Shawn Serbin, Carl Davidson
start.runs.SIPNET <- function(runid){
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  # run the model, sipnet needs to run in run folder
  cwd <- getwd()
  setwd(rundir)
  system2(settings$model$binary)
  setwd(cwd)

  # move output back to out folder
  file.rename(file.path(rundir, "sipnet.out"), file.path(outdir, "sipnet.out"))
  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
