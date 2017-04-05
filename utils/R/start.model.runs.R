##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.  All rights reserved. This
## program and the accompanying materials are made available under the terms of
## the University of Illinois/NCSA Open Source License which accompanies this
## distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------


##'
##' Start selected ecosystem model runs within PEcAn workflow
##' 
##' @name start.model.runs
##' @title Start ecosystem model runs
##' @export start.model.runs
##' @examples
##' \dontrun{
##' start.model.runs(settings)
##' }
##' @author Shawn Serbin, Rob Kooper, David LeBauer
##'
start.model.runs <- function(settings, write = TRUE) {
  
  # check if runs need to be done
  if(!file.exists(file.path(settings$rundir, "runs.txt"))){
    logger.warn("runs.txt not found, assuming no runs need to be done")
    return()
  }
  
  
  model <- settings$model$type
  logger.info("-------------------------------------------------------------------")
  logger.info(paste(" Starting model runs", model))
  logger.info("-------------------------------------------------------------------")
  
  # loop through runs and either call start run, or launch job on remote machine
  jobids <- list()
  
  ## setup progressbar
  nruns <- length(readLines(con = file.path(settings$rundir, "runs.txt")))
  pb <- txtProgressBar(min = 0, max = nruns, style = 3)
  pbi <- 0
  
  # create database connection
  if (write) {
    dbcon <- db.open(settings$database$bety)
    on.exit(db.close(dbcon))
  } else {
    dbcon <- NULL
  }
  
  # launcher folder
  launcherfile <- NULL
  jobfile <- NULL
  firstrun <- NULL
  
  # launch each of the jobs
  for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
    # write start time to database
    if (!is.null(dbcon)) {
      db.query(paste("UPDATE runs SET started_at =  NOW() WHERE id = ", 
                     format(run, scientific = FALSE)), con = dbcon)
    }
    
    # if running on a remote cluster, create folders and copy any data to remote host
    if (!is.localhost(settings$host)) {
      remote.execute.cmd(settings$host, "mkdir", c("-p", file.path(settings$host$outdir, 
                                                                   format(run, scientific = FALSE))))
      remote.copy.to(settings$host, file.path(settings$rundir, format(run, 
                                                                      scientific = FALSE)), settings$host$rundir, delete = TRUE)
    }
    
    # check to see if we use the model launcer
    if (!is.null(settings$host$modellauncher)) {
      pbi <- pbi + 1
      
      # set up launcher script if we use modellauncher
      if (is.null(firstrun)) {
        firstrun <- run
        launcherfile <- file.path(settings$rundir, 
                                  format(run, scientific = FALSE), 
                                  "launcher.sh")
        unlink(file.path(settings$rundir, 
                         format(run, scientific = FALSE), 
                         "joblist.txt"))
        jobfile <- file(file.path(settings$rundir,
                                  format(run, scientific = FALSE), 
                                  "joblist.txt"), "w")
        
        writeLines(c("#!/bin/bash", paste(settings$host$modellauncher$mpirun, 
                                          settings$host$modellauncher$binary, 
                                          file.path(settings$host$rundir, format(run, scientific = FALSE),
                                                    "joblist.txt"))), con = launcherfile)
        writeLines(c("./job.sh"), con = jobfile)
      }
      writeLines(c(file.path(settings$host$rundir, format(run, scientific = FALSE))), 
                 con = jobfile)
      
    } else {
      # if qsub is requested
      if (!is.null(settings$host$qsub)) {
        qsub <- gsub("@NAME@", paste0("PEcAn-", format(run, scientific = FALSE)), settings$host$qsub)
        qsub <- gsub("@STDOUT@", file.path(settings$host$outdir,
                                           format(run, scientific = FALSE), "stdout.log"), qsub)
        qsub <- gsub("@STDERR@", file.path(settings$host$outdir, 
                                           format(run, scientific = FALSE), "stderr.log"), qsub)
        qsub <- strsplit(qsub, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
        
        # start the actual model run
        cmd <- qsub[[1]]
        args <- qsub[-1]
        if (is.localhost(settings$host)) {
          out <- system2(cmd, c(args, file.path(settings$rundir, format(run, 
                                                                        scientific = FALSE), "job.sh")), stdout = TRUE, stderr = TRUE)
        } else {
          out <- remote.execute.cmd(settings$host, cmd, c(args, file.path(settings$host$rundir, 
                                                                          format(run, scientific = FALSE), "job.sh")), stderr = TRUE)
        }
        print(out)  # <-- for debugging
        jobids[run] <- sub(settings$host$qsub.jobid, "\\1", out)
        
        # if qsub option is not invoked.  just start model runs in serial.
      } else {
        if (is.localhost(settings$host)) {
          out <- system2(file.path(settings$rundir, format(run, scientific = FALSE), 
                                   "job.sh"), stdout = TRUE, stderr = TRUE)
        } else {
          out <- remote.execute.cmd(settings$host, file.path(settings$host$rundir, 
                                                             format(run, scientific = FALSE), "job.sh"), stderr = TRUE)
        }
        
        # check output to see if an error occurred during the model run
        if ("ERROR IN MODEL RUN" %in% out) {
          logger.severe("Model run aborted, with error.\n", out)
        }
        
        # copy data back to local
        if (!is.localhost(settings$host)) {
          remote.copy.from(settings$host, file.path(settings$host$outdir, 
                                                    format(run, scientific = FALSE)), settings$modeloutdir)
        }
        
        # write finished time to database
        if (!is.null(dbcon)) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", 
                         format(run, scientific = FALSE)), con = dbcon)
        }
        
        # update progress bar
        pbi <- pbi + 1
        setTxtProgressBar(pb, pbi)
      }
    }
  }
  close(pb)
  
  # if using the model launcer
  if (!is.null(settings$host$modellauncher)) {
    close(jobfile)
    
    # copy launcer and joblist
    if (!is.localhost(settings$host)) {
      remote.copy.to(settings$host, file.path(settings$rundir, 
                                              format(firstrun, scientific = FALSE)), settings$host$rundir, delete = TRUE)
    }
    
    # if qsub is requested
    if (!is.null(settings$host$qsub)) {
      qsub <- gsub("@NAME@", "PEcAn-all", settings$host$qsub)
      qsub <- gsub("@STDOUT@", file.path(settings$host$outdir, 
                                         format(firstrun, scientific = FALSE), "launcher.out.log"), qsub)
      qsub <- gsub("@STDERR@", file.path(settings$host$outdir,
                                         format(firstrun, scientific = FALSE), "launcher.err.log"), qsub)
      qsub <- strsplit(paste(qsub, settings$host$modellauncher$qsub.extra), 
                       " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
      
      # start the actual model run
      if (is.localhost(settings$host)) {
        cmd <- qsub[[1]]
        qsub <- qsub[-1]
        out <- system2(cmd, c(qsub, file.path(settings$rundir,
                                              format(run, scientific = FALSE), "launcher.sh"), recursive = TRUE), stdout = TRUE)
      } else {
        out <- system2("ssh", c(settings$host$name, qsub,
                                file.path(settings$host$rundir, format(firstrun, scientific = FALSE), "launcher.sh"), recursive = TRUE), 
                       stdout = TRUE)
      }
      # print(out) # <-- for debugging
      
      # HACK: Code below gets 'run' from names(jobids) so need an entry for each run.
      # But when using modellauncher all runs have the same jobid
      for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
        jobids[run] <- sub(settings$host$qsub.jobid, "\\1", out)
      }
    } else {
      if (is.localhost(settings$host)) {
        out <- system2(file.path(settings$rundir, format(firstrun, scientific = FALSE), 
                                 "launcher.sh"), stdout = TRUE)
      } else {
        out <- system2("ssh", c(settings$host$name, file.path(settings$host$rundir, 
                                                              format(firstrun, scientific = FALSE), 
                                                              "launcher.sh")), stdout = TRUE)
      }
      
      # check output to see if an error occurred during the model run
      if ("ERROR IN MODEL RUN" %in% out) {
        logger.severe("Model run aborted, with error.\n", out)
      }
      
      # write finished time to database
      if (!is.null(dbcon)) {
        for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
          db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", 
                         format(run, scientific = FALSE)), con = dbcon)
        }
      }
      
      # update progress bar
      setTxtProgressBar(pb, pbi)
    }
  }
  
  # wait for all qsub jobs to finish
  if (length(jobids) > 0) {
    logger.debug("Waiting for the following jobs:", unlist(jobids, use.names = FALSE))
    while (length(jobids) > 0) {
      Sys.sleep(10)
      for (run in names(jobids)) {
        # check to see if job is done
        check <- gsub("@JOBID@", jobids[run], settings$host$qstat)
        args <- strsplit(check, " (?=([^\"']*\"[^\"']*\")*[^\"']*$)", perl = TRUE)
        if (is.localhost(settings$host)) {
          # cmd <- args[[1]] args <- args[-1] out <- system2(cmd, args, stdout=TRUE)
          out <- system(check, 
                        intern = TRUE, 
                        ignore.stdout = FALSE,
                        ignore.stderr = FALSE, 
                        wait = TRUE)
        } else {
          out <- remote.execute.cmd(settings$host, check, stderr = TRUE)
        }
        
        if ((length(out) > 0) && (substring(out, nchar(out) - 3) == "DONE")) 
        {
          logger.debug("Job", jobids[run], "for run", format(run, scientific = FALSE), 
                       "finished")
          jobids[run] <- NULL
          
          # copy data back to local
          if (!is.localhost(settings$host)) {
            remote.copy.from(settings$host, file.path(settings$host$outdir, 
                                                      format(run, scientific = FALSE)), settings$modeloutdir)
          }
          
          # TODO check output log
          
          # write finish time to database
          if (!is.null(dbcon)) 
          {
            if (!is.null(settings$host$modellauncher)) {
              for (run in readLines(con = file.path(settings$rundir, "runs.txt"))) {
                db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", 
                               format(run, scientific = FALSE)), con = dbcon)
              }
            } else {
              db.query(paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", 
                             format(run, scientific = FALSE)), con = dbcon)
            }
          }  # end writing to database
          
          # update progress bar
          if (is.null(settings$host$modellauncher)) {
            pbi <- pbi + 1
          }
          setTxtProgressBar(pb, pbi)
        }  # end job done if loop
      }  # end for loop
    }  # end while loop
  }
  
} # start.model.runs


##' @export
runModule.start.model.runs <- function(settings) {
  if (is.MultiSettings(settings) || is.Settings(settings)) {
    write <- settings$database$bety$write
    return(start.model.runs(settings, write))
  } else {
    stop("runModule.start.model.runs only works with Settings or MultiSettings")
  }
} # runModule.start.model.runs
