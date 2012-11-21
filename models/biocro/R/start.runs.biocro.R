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
##' @author Michael Dietze, David LeBauer, Shawn Serbin, Carl Davidson
start.run.biocro <- function(run.id){
  print(paste("---- biocro model run: ", run.id, sep=""))
  rundir <- paste(settings$outdir, run.id, sep = "")
  setwd(rundir)
  library(EnCro)
  require(XML)
  config <- xmlToList(xmlParse(run.id))
# defaul data in EnCro  
  data(weather05)

#  result <- do.call(biocro, list(RH = 0.50, Tl = 25, Qp = 2000,
#                                unlist(config$parms)))
 
   pp<-do.call(photoParms,list(unlist(config$parms)))
   result<-BioGro(weather05,photoControl=pp)
   save(result,file=paste(run.id,".Rdata",sep=""))
   }


#--------------------------------------------------------------------------------------------------#
##' 
##' Start biocro model runs on local
##' @title Start run of biocro model
##' @param runid the id of the run (folder in runs) to execute
##' @export
##' @author David LeBauer
start.runs.BIOCRO <- function(runid) {
  if (settings$run$host$name != "localhost") {
    stop("Only local runs are executed here")
  }

  rundir <- file.path(settings$run$host$rundir, as.character(runid))
  outdir <- file.path(settings$run$host$outdir, as.character(runid))

  cwd <- getwd()
  setwd(rundir)

  # run model
  require(EnCro)
  require(XML)

  data(weather05)
  config <- xmlToList(xmlParse("data.xml"))
  pp<-do.call(photoParms,list(unlist(config$parms)))
  result<-BioGro(weather05, photoControl=pp)

  # save results
  save(result, file=file.path(outdir, "result.Rdata"))
  file.copy(file.path(rundir, "README.txt"), file.path(outdir, "README.txt"))
}

##' Function to start all runs of biocro model in directory
##' @title Start run of biocro model
##' @export
##' @return nothing, starts run as side effect
##' @author David LeBauer
start.runs.biocro <- function(runid){
  if (settings$run$host$name == "localhost") {
    print(paste("---- biocro model run: ", runid, sep=""))

    cwd = getwd()
    setwd(file.path(settings$run$host$rundir, as.character(runid)))

    # defaul data in EnCro  
    data(weather05)

    # run model
    require(EnCro)
    require(XML)
    config <- xmlToList(xmlParse("data.xml"))
    pp<-do.call(photoParms,list(unlist(config$parms)))
    result<-BioGro(weather05,photoControl=pp)

    # save results
    save(result, file.path(settings$run$host$outdir, runid, "result.Rdata"))
    file.copy(file.path(settings$run$host$rundir, runid, "README.txt"), file.path(settings$run$host$outdir, runid, "README.txt"))

    setwd(cwd)

  } else {  
    # ORIGINAL CODE
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
  }
} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################
