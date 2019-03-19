##-------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.  All rights reserved. This
## program and the accompanying materials are made available under the terms of
## the University of Illinois/NCSA Open Source License which accompanies this
## distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
##-------------------------------------------------------------------------------


##' Start selected ecosystem model runs within PEcAn workflow
##'
##' @param settings pecan settings object
##' @param write (logical) Whether or not to write to the database. Default TRUE.
##' @param stop.on.error Throw error if _any_ of the runs fails. Default TRUE.
##' @export start.model.runs
##' @examples
##' \dontrun{
##'   start.model.runs(settings)
##' }
##' @author Shawn Serbin, Rob Kooper, David LeBauer, Alexey Shiklomanov
##'
start.model.runs <- function(settings, write = TRUE, stop.on.error = TRUE) {

  run_file <- file.path(settings$rundir, "runs.txt")
  # check if runs need to be done
  if (!file.exists(file.path(settings$rundir, "runs.txt"))) {
    PEcAn.logger::logger.warn("runs.txt not found, assuming no runs need to be done")
    return()
  }
  run_list <- readLines(con = run_file)
  nruns <- length(run_list)
  if (nruns == 0) {
    PEcAn.logger::logger.warn("runs.txt found, but is empty. Assuming no runs need to be done")
    return()
  }

  model <- settings$model$type
  PEcAn.logger::logger.info("-------------------------------------------------------------------")
  PEcAn.logger::logger.info(paste(" Starting model runs", model))
  PEcAn.logger::logger.info("-------------------------------------------------------------------")

  is_local <- is.localhost(settings$host)
  is_qsub <- !is.null(settings$host$qsub)
  is_rabbitmq <- !is.null(settings$host$rabbitmq)
  is_modellauncher <- !is.null(settings$host$modellauncher)

  # loop through runs and either call start run, or launch job on remote machine
  jobids <- list()

  ## setup progressbar
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
  jobfile <- NULL
  firstrun <- NULL

  # launch each of the jobs
  for (run in run_list) {
    run_id_string <- format(run, scientific = FALSE)
    # write start time to database
    stamp_started(con = dbcon, run = run)

    # if running on a remote cluster, create folders and copy any data to remote host
    if (!is_local) {
      PEcAn.remote::remote.execute.cmd(settings$host, "mkdir", c("-p", file.path(settings$host$outdir, run_id_string)))
      PEcAn.remote::remote.copy.to(settings$host, file.path(settings$rundir, run_id_string), settings$host$rundir, delete = TRUE)
    }

    # check to see if we use the model launcer
    if (is_rabbitmq) {
      run_id_string <- format(run, scientific = FALSE)
      folder <- file.path(settings$rundir, run_id_string)
      out <- start_rabbitmq(folder, settings$host$rabbitmq$uri, settings$host$rabbitmq$queue)
      PEcAn.logger::logger.debug("JOB.SH submit status:", out)
      jobids[run] <- folder

    } else if (is_modellauncher) {
      # set up launcher script if we use modellauncher
      if (is.null(firstrun)) {
        firstrun <- run
        jobfile <- setup_modellauncher(run = run,
                                       rundir = settings$rundir,
                                       host_rundir = settings$host$rundir,
                                       mpirun = settings$host$modellauncher$mpirun,
                                       binary = settings$host$modellauncher$binary)
      }
      writeLines(c(file.path(settings$host$rundir, run_id_string)), con = jobfile)
      pbi <- pbi + 1

    } else if (is_qsub) {
      out <- start_qsub(run = run, qsub_string = settings$host$qsub, rundir = settings$rundir,
                        host = settings$host,  host_rundir = settings$host$rundir, host_outdir = settings$host$outdir,
                        stdout_log = "stdout.log", stderr_log = "stderr.log", job_script = "job.sh")
      PEcAn.logger::logger.debug("JOB.SH submit status:", out)
      jobids[run] <- qsub_get_jobid(out = out, qsub.jobid = settings$host$qsub.jobid, stop.on.error = stop.on.error)

    } else {
      # if qsub option is not invoked.  just start model runs in serial.
      out <- start_serial(run = run, host = settings$host, rundir = settings$rundir, host_rundir = settings$host$rundir, job_script = "job.sh")

      # check output to see if an error occurred during the model run
      check_model_run(out = out, stop.on.error = stop.on.error)

      if (!is_local) {
        # copy data back to local
        PEcAn.remote::remote.copy.from(settings$host, file.path(settings$host$outdir, run_id_string), settings$modeloutdir)
      }

      # write finished time to database
      stamp_finished(con = dbcon, run = run)

      pbi <- pbi + 1
      setTxtProgressBar(pb, pbi)
    }
  } # end loop over runs
  close(pb)

  # need to actually launch the model launcher
  if (is_modellauncher) {
    close(jobfile)

    if (!is_local) {
      # copy launcher and joblist
      PEcAn.remote::remote.copy.to(settings$host, file.path(settings$rundir,
                                              format(firstrun, scientific = FALSE)), settings$host$rundir, delete = TRUE)
    }

    if (is_qsub) {
      out <- start_qsub(run = firstrun, qsub_string = settings$host$qsub, rundir = settings$rundir,
                        host = settings$host, host_rundir = settings$host$rundir, host_outdir = settings$host$outdir,
                        stdout_log = "launcher.out.log", stderr_log = "launcher.err.log", job_script = "launcher.sh",
                        qsub_extra = settings$host$modellauncher$qsub)

      # HACK: Code below gets 'run' from names(jobids) so need an entry for each run.
      # But when using modellauncher all runs have the same jobid
      for (run in run_list) {
        jobids[run] <- sub(settings$host$qsub.jobid, "\\1", out)
      }
    } else {
      out <- start_serial(run = run, host = settings$host, rundir = settings$rundir,  host_rundir = settings$host$rundir,
                          job_script = "launcher.sh")

      # check output to see if an error occurred during the model run
      check_model_run(out = out, stop.on.error = TRUE)

      # write finished time to database
      for (run in run_list) {
        stamp_finished(con = dbcon, run = run)
      }

      setTxtProgressBar(pb, pbi)
    }
  }

  # wait for all jobs to finish
  if (length(jobids) > 0) {
    PEcAn.logger::logger.debug("Waiting for the following jobs:", unlist(jobids, use.names = FALSE))
  }

  while (length(jobids) > 0) {
    Sys.sleep(10)
    for (run in names(jobids)) {
      run_id_string <- format(run, scientific = FALSE)

      # check to see if job is done
      job_finished <- FALSE
      if (is_rabbitmq) {
        job_finished <- file.exists(file.path(jobids[run], "rabbitmq.out"))
      } else if (is_qsub) {
        job_finished <- qsub_run_finished(run = jobids[run], host = settings$host, qstat = settings$host$qstat)
      }

      if (job_finished) {

        # Copy data back to local
        if (!is_local) {
          PEcAn.remote::remote.copy.from(host = settings$host,
                                         src = file.path(settings$host$outdir, run_id_string),
                                         dst = settings$modeloutdir)
        }

        # TODO check output log
        if (is_rabbitmq) {
          data <- readLines(file.path(jobids[run], "rabbitmq.out"))
          if (data[-1] == "ERROR") {
            msg <- paste("Run", run, "has an ERROR executing")
            if (stop.on.error) {
              PEcAn.logger::logger.severe(msg)
            } else {
              PEcAn.logger::logger.error(msg)
            }
          }
        }

        # Write finish time to database
        if (is_modellauncher) {
          for (x in run_list) {
            stamp_finished(con = dbcon, run = x)
          }
        } else {
          stamp_finished(con = dbcon, run = run)
        }

        # move progress bar
        if (!is_modellauncher) {
          pbi <- pbi + 1
        }
        setTxtProgressBar(pb, pbi)

        # remove job
        if (is_modellauncher) {
          for (x in run_list) {
            jobids[x] <- NULL
          }          
        } else {
          jobids[run] <- NULL
        }
      } # End job finished
    }  # end loop over runs
  }  # end while loop checking runs

} # start.model.runs

##' @export
runModule.start.model.runs <- function(settings,stop.on.error=TRUE) {
  if (is.MultiSettings(settings) || is.Settings(settings)) {
    write <- settings$database$bety$write
    return(start.model.runs(settings, write,stop.on.error))
  } else {
    PEcAn.logger::logger.severe("runModule.start.model.runs only works with Settings or MultiSettings")
  }
} # runModule.start.model.runs
