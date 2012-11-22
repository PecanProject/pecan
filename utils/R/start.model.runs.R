##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##'
##' Start selected ecosystem model runs within PEcAn workflow
##' 
##' @name start.model.runs
##' @title Start ecosystem model runs
##' @export 
##' @examples
##' \dontrun {
##' start.model.runs("ED2")
##' start.model.runs("SIPNET")
##' }
##' @author Shawn Serbin
##'
start.model.runs <- function(model=settings$model$name, write.to.db = TRUE){

  fcn.name <- paste("start.runs.", model, sep="")
  if(exists(fcn.name)){
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste(" Starting model runs", model))
    print("-------------------------------------------------------------------")
    print(" ")
    if(!write.to.db){
      warning("Run provenance not being logged by database")
      con <- NULL
    }
    if(write.to.db){
      ## write to the runs table
      con <- try(query.base.con(settings), silent=TRUE)
      if(is.character(con)) {
        con <- NULL
      }
    }
    
    ## TODO RK : create ssh connection to remote host and keep it open

    ## TODO RK : loop through runs in runs.txt and copy folder then execute
    for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
      ## write start time to database
      if (!is.null(con)) {
        query.base(paste("UPDATE runs SET started_at =  NOW() WHERE id = ", run), con)
      }

      ## start the actual model run
      do.call(fcn.name, args=list(run))

      ## write finished time to database
      ## TODO how do we deal with a qsub, do we know it is a qsub?
      if (!is.null(con)) {
        query.base(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", run), con)
      }
    }

    ## TODO RK: check to see if all runs are done

    ## TODO RK : close connection to remote site

    ## job is finished
    if (!is.null(con)) {
      query.close(con)
    }

  } else {
    warning(paste(fcn.name, "does not exist"))
    warning(paste("This function is required, please make sure the model module is loaded for",model))
    stop()
  }
} ### End of function
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
