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
##' @author Shawn Serbin, Rob Kooper, ...
##'
start.model.runs <- function(model, write = TRUE){

  load.modelpkg(model)
  fcn.name <- paste("start.runs.", model, sep="")
  if(exists(fcn.name)){
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste(" Starting model runs", model))
    print("-------------------------------------------------------------------")
    print(" ")

    # copy all run/out dirs to remote host
    if (!is.null(settings$run$host$qsub) || (settings$run$host$name != "localhost")) {
      if (settings$run$host$name == "localhost") {
        dir.create(settings$run$host$rundir, recursive = TRUE)
        dir.create(settings$run$host$outdir, recursive = TRUE)
      } else {
        system2("ssh", c(settings$run$host$name, "mkdir", "-p", settings$run$host$rundir), stdout=TRUE)
        system2("ssh", c(settings$run$host$name, "mkdir", "-p", settings$run$host$outdir), stdout=TRUE)
      }
      rsync("-a", settings$rundir, paste(settings$run$host$name, settings$run$host$rundir, sep=":"), pattern="/*")
      rsync("-a", settings$modeloutdir, paste(settings$run$host$name, settings$run$host$outdir, sep=":"), pattern="/*")
    }

    # loop through runs and either call start run, or launch job on remote machine
    jobids <- list()
    
    ## setup progressbar
    nruns <- length(readLines(con = file.path(settings$rundir, "runs.txt")))
    pb <- txtProgressBar(min = 0, max = nruns, style = 3)
    pbi <- 0

    if (write) {
      dbcon <- db.open(settings$database)
    } else {
      dbcon <- NULL
    }
    for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
      pbi <- pbi + 1
      setTxtProgressBar(pb, pbi)
      # write start time to database
      if (!is.null(dbcon)) {
        db.query(paste("UPDATE runs SET started_at =  NOW() WHERE id = ", run), con=dbcon)
      }

      # if qsub is requested on localhost
      if (!is.null(settings$run$host$qsub)){
        qsub <- gsub("@NAME@", paste("PEcAn-", run, sep=""), settings$run$host$qsub)
        qsub <- gsub("@STDOUT@", file.path(settings$run$host$outdir, run, "stdout.log"), qsub)
        qsub <- gsub("@STDERR@", file.path(settings$run$host$outdir, run, "stderr.log"), qsub)

        # start the actual model run
        if (settings$run$host$name == "localhost") {        
          out <- system2(qsub, c(file.path(settings$run$host$rundir, run, "job.sh")), stdout=TRUE)
        } else {
          out <- system2("ssh", c(settings$run$host$name, qsub, file.path(settings$run$host$rundir, run, "job.sh")), stdout=TRUE)
        }
        m <- regexec(settings$run$host$qsub.jobid, out) # needs updating.  currently not working properly
        jobids[run] <- regmatches(out, m)[[1]][2]
        
      # if qsub option is not invoked.  just start model runs in serial.
      } else {
        if (settings$run$host$name == "localhost") {        
          out <- system2(file.path(settings$run$host$rundir, run, "job.sh"), stdout=TRUE)
        } else {
          out <- system2("ssh", c(settings$run$host$name, file.path(settings$run$host$rundir, run, "job.sh")), stdout=TRUE)
        }

        # write finished time to database
        if (!is.null(dbcon)) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", run), con=dbcon)
        }
      }
    }
    close(pb)

    # check to see if all remote jobs are done
    if (settings$run$host$name != "localhost") {
      while(length(jobids) > 0) {
        Sys.sleep(10)
        for(run in names(jobids)) {
          check <- gsub("@JOBID@", jobids[run], settings$run$host$qstat)
          out <- system2("ssh", c(settings$run$host$name, check), stdout=TRUE)
          if ((length(out) > 0) && (out == "DONE")) {
            logger.debug("Job", jobids[run], "for run", run, "finished")
            jobids[run] <- NULL
            # write finished time to database 
            if (!is.null(dbcon)) {
              db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", run), con=dbcon)
            }
          }
        }
      }
    }

    # close database connection
    if (!is.null(dbcon)) {
      db.close(dbcon)
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
