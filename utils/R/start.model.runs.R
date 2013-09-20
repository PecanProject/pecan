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
  print(" ")
  print("-------------------------------------------------------------------")
  print(paste(" Starting model runs", model))
  print("-------------------------------------------------------------------")
  print(" ")

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

    # create folders and copy any data to remote host
    if (settings$run$host$name != "localhost") {
      system2("ssh", c(settings$run$host$name, "mkdir", "-p", file.path(settings$run$host$rundir, run)), stdout=TRUE)
      system2("ssh", c(settings$run$host$name, "mkdir", "-p", file.path(settings$run$host$outdir, run)), stdout=TRUE)
      rsync("-a --delete", file.path(settings$rundir, run), paste(settings$run$host$name, file.path(settings$run$host$rundir, run), sep=":"), pattern='/')
    }

    # if qsub is requested on localhost
    if (!is.null(settings$run$host$qsub)){
      qsub <- gsub("@NAME@", paste("PEcAn-", run, sep=""), settings$run$host$qsub)
      qsub <- gsub("@STDOUT@", file.path(settings$run$host$outdir, run, "stdout.log"), qsub)
      qsub <- gsub("@STDERR@", file.path(settings$run$host$outdir, run, "stderr.log"), qsub)
      qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl=TRUE)

      # start the actual model run
      if (settings$run$host$name == "localhost") {        
        cmd <- qsub[[1]]
        qsub <- qsub[-1]
        out <- system2(cmd, c(qsub, file.path(settings$rundir, run, "job.sh"), recursive=TRUE), stdout=TRUE)
      } else {
        out <- system2("ssh", c(settings$run$host$name, qsub, file.path(settings$run$host$rundir, run, "job.sh"), recursive=TRUE), stdout=TRUE)
      }
      #print(out) # <-- for debugging
      jobids[run] <- sub(settings$run$host$qsub.jobid, "\\1", out)

    # if qsub option is not invoked.  just start model runs in serial.
    } else {
      if (settings$run$host$name == "localhost") {        
        out <- system2(file.path(settings$rundir, run, "job.sh"), stdout=TRUE)
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

  if (length(jobids) > 0) {
    logger.debug("Waiting for the following jobs:", unlist(jobids, use.names=FALSE))
  }

  # check to see if all remote jobs are done
  while(length(jobids) > 0) {
    Sys.sleep(10)
    for(run in names(jobids)) {
      check <- gsub("@JOBID@", jobids[run], settings$run$host$qstat)
      args <- strsplit(check, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl=TRUE)
      if (settings$run$host$name == "localhost") {
        cmd <- args[[1]]
        args <- args[-1]
        out <- system2(cmd, args, stdout=TRUE)
      } else {
        out <- system2("ssh", c(settings$run$host$name, args, recursive=TRUE), stdout=TRUE)
      }
      if ((length(out) > 0) && (out == "DONE")) {
        logger.debug("Job", jobids[run], "for run", run, "finished")
        jobids[run] <- NULL
        # write finished time to database 
        if (!is.null(dbcon)) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", run), con=dbcon)
        } # end writing to database
      } # end job done if loop
    } # end for loop
  } # end while loop

  if (settings$run$host$name != 'localhost') {
    for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
      rsync("-a --delete", paste(settings$run$host$name, file.path(settings$run$host$outdir, run), sep=":"), file.path(settings$outdir, run), pattern="/")
    }
  }

  # close database connection
  if (!is.null(dbcon)) {
    db.close(dbcon)
  }
} ### End of function
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
