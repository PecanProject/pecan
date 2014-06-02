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
  print("-------------------------------------------------------------------")
  print(paste(" Starting model runs", model))
  print("-------------------------------------------------------------------")

  # loop through runs and either call start run, or launch job on remote machine
  jobids <- list()
  
  ## setup progressbar
  nruns <- length(readLines(con = file.path(settings$rundir, "runs.txt")))
  pb <- txtProgressBar(min = 0, max = nruns, style = 3)
  pbi <- 0

  # create database connection
  if (write) {
    dbcon <- db.open(settings$database)
  } else {
    dbcon <- NULL
  }

  # launcher folder
  launcherfile <- NULL
  jobfile <- NULL
  firstrun <- NULL

  # launch each of the jobs
  for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
    pbi <- pbi + 1
    setTxtProgressBar(pb, pbi)

    # write start time to database
    if (!is.null(dbcon)) {
      db.query(paste("UPDATE runs SET started_at =  NOW() WHERE id = ", format(run,scientific=FALSE)), con=dbcon)
    }

    # create folders and copy any data to remote host
    if (settings$run$host$name != "localhost") {
      system2("ssh", c(settings$run$host$name, "mkdir", "-p", file.path(settings$run$host$rundir, format(run,scientific=FALSE))), stdout=TRUE)
      system2("ssh", c(settings$run$host$name, "mkdir", "-p", file.path(settings$run$host$outdir, format(run,scientific=FALSE))), stdout=TRUE)
      rsync("-a --delete", file.path(settings$rundir, format(run,scientific=FALSE)), paste(settings$run$host$name, file.path(settings$run$host$rundir, format(run,scientific=FALSE)), sep=":"), pattern='/')
    }

    # check to see if we use the model launcer
    if (!is.null(settings$run$host$modellauncher)) {
      # set up launcher script if we use modellauncher 
      if (pbi == 1) {
        firstrun <- run
        launcherfile <- file.path(settings$rundir, format(run,scientific=FALSE), "launcher.sh")
        unlink(file.path(settings$rundir, format(run,scientific=FALSE), "joblist.txt"))
        jobfile <- file(file.path(settings$rundir, format(run,scientific=FALSE), "joblist.txt"), "w")

        writeLines(c("#!/bin/bash",
                     paste(settings$run$host$modellauncher$mpirun,
                           settings$run$host$modellauncher$binary,
                           file.path(settings$run$host$rundir, format(run,scientific=FALSE), "joblist.txt"))), con=launcherfile)
        writeLines(c("./job.sh"), con=jobfile)
      }
      writeLines(c(file.path(settings$run$host$rundir, format(run,scientific=FALSE))), con=jobfile)

    } else {
      # if qsub is requested
      if (!is.null(settings$run$host$qsub)){
        qsub <- gsub("@NAME@", paste("PEcAn-", format(run,scientific=FALSE), sep=""), settings$run$host$qsub)
        qsub <- gsub("@STDOUT@", file.path(settings$run$host$outdir, format(run,scientific=FALSE), "stdout.log"), qsub)
        qsub <- gsub("@STDERR@", file.path(settings$run$host$outdir, format(run,scientific=FALSE), "stderr.log"), qsub)
        qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl=TRUE)

        # start the actual model run
        if (settings$run$host$name == "localhost") {        
          cmd <- qsub[[1]]
          qsub <- qsub[-1]
          out <- system2(cmd, c(qsub, file.path(settings$rundir, format(run,scientific=FALSE), "job.sh"), recursive=TRUE), stdout=TRUE)
        } else {
          out <- system2("ssh", c(settings$run$host$name, qsub, file.path(settings$run$host$rundir, format(run,scientific=FALSE), "job.sh"), recursive=TRUE), stdout=TRUE)
        }
        #print(out) # <-- for debugging
        jobids[run] <- sub(settings$run$host$qsub.jobid, "\\1", out)

      # if qsub option is not invoked.  just start model runs in serial.
      } else {
        if (settings$run$host$name == "localhost") {        
          out <- system2(file.path(settings$rundir, format(run,scientific=FALSE), "job.sh"), stdout=TRUE)
        } else {
          out <- system2("ssh", c(settings$run$host$name, file.path(settings$run$host$rundir, format(run,scientific=FALSE), "job.sh")), stdout=TRUE)
        }

        # write finished time to database
        if (!is.null(dbcon)) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", format(run,scientific=FALSE)), con=dbcon)
        }

        # check output to see if an error occurred during the model run
        if ("ERROR IN MODEL RUN" %in% out) {
          logger.severe("Model run aborted, with error.\n", out)
        }
      }      
    }
  }
  close(pb)

  # check to see if we use the model launcer
  if (!is.null(settings$run$host$modellauncher)) {
    close(jobfile)

    # copy launcer and joblist
    if (settings$run$host$name != "localhost") {
      rsync("-a --delete", file.path(settings$rundir, format(firstrun,scientific=FALSE)), paste(settings$run$hmst$name, file.path(settings$run$host$rundir, format(firstrun,scientific=FALSE)), sep=":"), pattern='/')
    }

    # if qsub is requested
    if (!is.null(settings$run$host$qsub)) {
      qsub <- gsub("@NAME@", "PEcAn-all", settings$run$host$qsub)
      qsub <- gsub("@STDOUT@", file.path(settings$run$host$outdir, format(firstrun,scientific=FALSE), "launcher.out.log"), qsub)
      qsub <- gsub("@STDERR@", file.path(settings$run$host$outdir, format(firstrun,scientific=FALSE), "launcher.err.log"), qsub)
      qsub <- strsplit(paste(qsub, settings$run$host$modellauncher$qsub.extra), " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl=TRUE)

      # start the actual model run
      if (settings$run$host$name == "localhost") {        
        cmd <- qsub[[1]]
        qsub <- qsub[-1]
        out <- system2(cmd, c(qsub, file.path(settings$rundir, format(run,scientific=FALSE), "launcher.sh"), recursive=TRUE), stdout=TRUE)
      } else {
        out <- system2("ssh", c(settings$run$host$name, qsub, file.path(settings$run$host$rundir, format(firstrun,scientific=FALSE), "launcher.sh"), recursive=TRUE), stdout=TRUE)
      }
      #print(out) # <-- for debugging
      jobids[run] <- sub(settings$run$host$qsub.jobid, "\\1", out)
    } else {
      if (settings$run$host$name == "localhost") {        
        out <- system2(file.path(settings$rundir, format(firstrun,scientific=FALSE), "launcher.sh"), stdout=TRUE)
      } else {
        out <- system2("ssh", c(settings$run$host$name, file.path(settings$run$host$rundir, format(firstrun,scientific=FALSE), "launcher.sh")), stdout=TRUE)
      }
      # check output to see if an error occurred during the model run
      if ("ERROR IN MODEL RUN" %in% out) {
        logger.severe("Model run aborted, with error.\n", out)
      }
      # write finished time to database
      if (!is.null(dbcon)) {
        for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", format(run,scientific=FALSE)), con=dbcon)
        }
      }
    }
  }

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
        #cmd <- args[[1]]
        #args <- args[-1]
        #out <- system2(cmd, args, stdout=TRUE)
        out <- system(check, intern=TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait=TRUE) 
      } else {
        out <- system2("ssh", c(settings$run$host$name, args, recursive=TRUE), stdout=TRUE)
      }
      if ((nchar(out) > 0) && (substring(out, nchar(out)-3) == "DONE")) {
        logger.debug("Job", jobids[run], "for run", format(run,scientific=FALSE), "finished")
        jobids[run] <- NULL
        if (!is.null(dbcon)) {
          if (!is.null(settings$run$host$modellauncher)) {
            for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
              if (!is.null(dbcon)) {
                db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", format(run,scientific=FALSE)), con=dbcon)
              }
              outfile <- file.path(settings$run$host$outdir, format(run,scientific=FALSE), "stdout.log")
              out <- system2("ssh", c(settings$run$host$name, "cat", outfile), stdout=TRUE)
              # check output to see if an error occurred during the model run
              if ("ERROR IN MODEL RUN" %in% out) {
                logger.severe("Model run aborted, with error.\n", out)
              }
            }
          } else {
            # write finished time to database 
            if (!is.null(dbcon)) {
              db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", format(run,scientific=FALSE)), con=dbcon)
            }
            outfile <- file.path(settings$run$host$outdir, format(run,scientific=FALSE), "stdout.log")
            out <- system2("ssh", c(settings$run$host$name, "cat", outfile), stdout=TRUE)
            # check output to see if an error occurred during the model run
            if ("ERROR IN MODEL RUN" %in% out) {
              logger.severe("Model run aborted, with error.\n", out)
            }
          } # end modellauncher if
        } # end writing to database          
      } # end job done if loop
    } # end for loop
  } # end while loop

  if (settings$run$host$name != 'localhost') {
    for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
      rsync("-a --delete", paste(settings$run$host$name, file.path(settings$run$host$outdir, format(run,scientific=FALSE)), sep=":"), file.path(settings$modeloutdir, format(run,scientific=FALSE)), pattern="/")
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
