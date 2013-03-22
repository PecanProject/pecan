#-------------------------------------------------------------------------------
##Copyright (c) 2012 University of Illinois, NCSA.
##All rights reserved. This program and the accompanying materials
##are made available under the terms of the 
##University of Illinois/NCSA Open Source License
##which accompanies this distribution, and is available at
##http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------#
##' 
##' Start ED2 model runs on local or remote server
##' @title Start ED2 model runs
##' @name start.runs.ED2
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author David LeBauer, Shawn Serbin, Carl Davidson
start.runs.ED2 <- function(runid) {
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  # TODO RK : this does not work with relative paths
  cwd <- getwd()
  setwd(rundir)
  system2(settings$model$binary, env=c("GFORTRAN_UNBUFFERED_PRECONNECTED=yes"))
  setwd(cwd)

  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))

} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
