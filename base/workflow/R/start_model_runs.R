#' Start selected ecosystem model runs within PEcAn workflow
#'
#' @param settings pecan settings object
#' @param write (logical) Whether or not to write to the database. Default TRUE.
#' @param stop.on.error Throw error if _any_ of the runs fails. Default TRUE.
#' @export
#' @examples
#' \dontrun{
#'   start_model_runs(settings)
#' }
#' @author Shawn Serbin, Rob Kooper, David LeBauer, Alexey Shiklomanov
#'
start_model_runs <- function(settings, write = TRUE, stop.on.error = TRUE) {
  
  run_file <- file.path(settings$rundir, "runs.txt")
  # check if runs need to be done
  if (!file.exists(run_file)) {
    PEcAn.logger::logger.warn(
      "runs.txt not found, assuming no runs need to be done")
    return()
  }
  run_list <- readLines(con = run_file)
  nruns <- length(run_list)
  if (nruns == 0) {
    PEcAn.logger::logger.warn(
      "runs.txt found, but is empty. Assuming no runs need to be done")
    return()
  }
  
  model <- settings$model$type
  PEcAn.logger::logger.info(
    "-------------------------------------------------------------------")
  PEcAn.logger::logger.info(paste(" Starting model runs", model))
  PEcAn.logger::logger.info(
    "-------------------------------------------------------------------")
  
  is_local <- PEcAn.remote::is.localhost(settings$host)
  is_qsub <- !is.null(settings$host$qsub)
  is_rabbitmq <- !is.null(settings$host$rabbitmq)
  is_modellauncher <- !is.null(settings$host$modellauncher)
  
  # Check if Njobmax tag exists in seetings
  if (is_modellauncher){
    if (!is.null(settings$host$modellauncher$Njobmax)){
      Njobmax <- settings$host$modellauncher$Njobmax
    } else {
      Njobmax <- nruns
    }
    compt_run <- 0
    compt_run_modellauncher <- 1
    job_modellauncher <- list()
  }
  
  # loop through runs and either call start run, or launch job on remote machine
  jobids <- list()
  
  ## setup progressbar
  pb <- utils::txtProgressBar(min = 0, max = nruns, style = 3)
  pbi <- 0
  
  # create database connection
  if (write) {
    dbcon <- PEcAn.DB::db.open(settings$database$bety)
    on.exit(PEcAn.DB::db.close(dbcon), add = TRUE)
  } else {
    dbcon <- NULL
  }
  
  # launcher folder
  jobfile <- NULL
  firstrun <- NULL
  
  #Copy all run directories over if not local
  if (!is_local) {
    # copy over run directories
    PEcAn.utils::retry.func(
      PEcAn.remote::remote.copy.to(
        host = settings$host,
        src = settings$rundir, 
        dst = dirname(settings$host$rundir), 
        delete = TRUE
      ), 
      sleep = 2
    )
    
    # copy over out directories
    PEcAn.utils::retry.func(
      PEcAn.remote::remote.copy.to(
        host = settings$host,
        src = settings$modeloutdir,
        dst = dirname(settings$host$outdir),
        #include all directories, exclude all files
        options = c("--include=*/", "--exclude=*"),
        delete = TRUE
      ),
      sleep = 2
    )
  }
  
  # launch each of the jobs
  for (run in run_list) {
    run_id_string <- format(run, scientific = FALSE)
    # write start time to database
    PEcAn.DB::stamp_started(con = dbcon, run = run)
    
    # check to see if we use the model launcher
    if (is_rabbitmq) {
      run_id_string <- format(run, scientific = FALSE)
      folder <- file.path(settings$rundir, run_id_string)
      out <- PEcAn.remote::start_rabbitmq(
        folder, settings$host$rabbitmq$uri, settings$host$rabbitmq$queue)
      PEcAn.logger::logger.debug("JOB.SH submit status:", out)
      jobids[run] <- folder
      
    } else if (is_modellauncher) {
      # set up launcher script if we use modellauncher
      if (is.null(firstrun)) {
        firstrun <- run
        jobfile <- PEcAn.remote::setup_modellauncher(
          run = run,
          rundir = settings$rundir,
          host_rundir = settings$host$rundir,
          mpirun = settings$host$modellauncher$mpirun,
          binary = settings$host$modellauncher$binary)
        job_modellauncher[compt_run_modellauncher] <- run
        compt_run_modellauncher <- compt_run_modellauncher+1
      }
      writeLines(
        c(file.path(settings$host$rundir, run_id_string)),
        con = jobfile)
      pbi <- pbi + 1
      
    } else if (is_qsub) {
      out <- PEcAn.remote::start_qsub(
        run = run,
        qsub_string = settings$host$qsub,
        rundir = settings$rundir,
        host = settings$host,
        host_rundir = settings$host$rundir,
        host_outdir = settings$host$outdir,
        stdout_log = "stdout.log",
        stderr_log = "stderr.log",
        job_script = "job.sh")
      PEcAn.logger::logger.debug("JOB.SH submit status:", out)
      jobids[run] <- PEcAn.remote::qsub_get_jobid(
        out = out[length(out)],
        qsub.jobid = settings$host$qsub.jobid,
        stop.on.error = stop.on.error)
      
    } else {
      # if qsub option is not invoked.  just start model runs in serial.
      out <- PEcAn.remote::start_serial(
        run = run,
        host = settings$host,
        rundir = settings$rundir,
        host_rundir = settings$host$rundir,
        job_script = "job.sh")
      
      # check output to see if an error occurred during the model run
      PEcAn.remote::check_model_run(out = out, stop.on.error = stop.on.error)
      
      if (!is_local) {
        # copy data back to local
        PEcAn.utils::retry.func(
          PEcAn.remote::remote.copy.from(
            host = settings$host,
            src = file.path(settings$host$outdir, run_id_string),
            dst = settings$modeloutdir),
          sleep = 2
        )
      }
      
      # write finished time to database
      PEcAn.DB::stamp_finished(con = dbcon, run = run)
      
      pbi <- pbi + 1
      utils::setTxtProgressBar(pb, pbi)
    }
    
    # Check if compt_run has reached Njobmax
    if (is_modellauncher){
      compt_run <- compt_run + 1
      if (compt_run == Njobmax){
        close(jobfile)
        firstrun <- NULL
        compt_run <- 0
        jobfile <- NULL
      }      
    }
    
  } # end loop over runs
  close(pb)
  
  # need to actually launch the model launcher
  if (is_modellauncher) {
    
    # Only close if not already closed
    if (compt_run != 0){
      close(jobfile)
    }
    
    if (!is_local) {
      for (run in run_list){ #only re-copy run dirs that have launcher and job list
        if (run %in% job_modellauncher) {
          # copy launcher and joblist
          PEcAn.utils::retry.func(
            PEcAn.remote::remote.copy.to(
              host = settings$host,
              src = file.path(settings$rundir, format(run, scientific = FALSE)),
              dst = settings$host$rundir,
              delete = TRUE),
            sleep = 2
          )
          
        }
      }
    }
    if (is_qsub) {
      for (run in run_list){
        if (run %in% job_modellauncher) {
          out <- PEcAn.remote::start_qsub(
            run = run,
            qsub_string = settings$host$qsub,
            rundir = settings$rundir,
            host = settings$host,
            host_rundir = settings$host$rundir,
            host_outdir = settings$host$outdir,
            stdout_log = "launcher.out.log",
            stderr_log = "launcher.err.log",
            job_script = "launcher.sh",
            qsub_extra = settings$host$modellauncher$qsub)
        }
        # HACK: Code below gets 'run' from names(jobids) so need an entry for
        # each run. But when using modellauncher all runs have the same jobid
        jobids[run] <- sub(settings$host$qsub.jobid, "\\1", out[length(out)])
      }
      
    } else {
      out <- PEcAn.remote::start_serial(
        run = run,
        host = settings$host,
        rundir = settings$rundir,
        host_rundir = settings$host$rundir,
        job_script = "launcher.sh")
      
      # check output to see if an error occurred during the model run
      PEcAn.remote::check_model_run(out = out, stop.on.error = TRUE)
      
      # write finished time to database
      for (run in run_list) {
        PEcAn.DB::stamp_finished(con = dbcon, run = run)
      }
      
      utils::setTxtProgressBar(pb, pbi)
    }
  }
  
  # wait for all jobs to finish
  if (length(jobids) > 0) {
    PEcAn.logger::logger.debug(
      "Waiting for the following jobs:",
      unlist(unique(jobids)))
  }
  
  #TODO figure out a way to do this while for unique(jobids) instead of jobids
  while (length(jobids) > 0) {
    Sys.sleep(10)
    
    if (!is_local) {
      #Copy over log files to check progress
      try(PEcAn.remote::remote.copy.from(
        host = settings$host,
        src = settings$host$outdir,
        dst = dirname(settings$modeloutdir),
        options = c('--exclude=*.h5')
      ))
    }
    
    for (run in names(jobids)) {
      run_id_string <- format(run, scientific = FALSE)
      
      # check to see if job is done
      job_finished <- FALSE
      if (is_rabbitmq) {
        job_finished <- 
          file.exists(file.path(jobids[run], "rabbitmq.out"))
      } else if (is_qsub) {
        job_finished <- PEcAn.remote::qsub_run_finished(
          run = jobids[run],
          host = settings$host,
          qstat = settings$host$qstat)
      }
      
      if (job_finished) {
        
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
        #TODO this repeats for every run in `jobids` writing every run's time stamp every time. This actually takes quite a long time with a lot of ensembles and should either 1) not be a for loop (no `for(x in run_list)`) or 2) if `is_modellauncher`, be done outside of the jobids for loop after all jobs are finished.
        if (is_modellauncher) {
          for (x in run_list) {
            PEcAn.DB::stamp_finished(con = dbcon, run = x)
          }
        } else {
          PEcAn.DB::stamp_finished(con = dbcon, run = run)
        }
        
        # move progress bar
        if (!is_modellauncher) {
          pbi <- pbi + 1
        }
        utils::setTxtProgressBar(pb, pbi)
        
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
  
  # Copy data back to local
  if (!is_local) {
    PEcAn.utils::retry.func(
      PEcAn.remote::remote.copy.from(
        host = settings$host,
        src = settings$host$outdir,
        dst = dirname(settings$modeloutdir)
      ),
      sleep = 2
    )
  }
} # start_model_runs


#' @describeIn start_model_runs
#'
#' A lightweight wrapper around `start_model_runs` that takes `write` from
#' `settings` instead of as a separate argument.
#'
#' @export
runModule_start_model_runs <- function(settings, stop.on.error=TRUE) {
  if (PEcAn.settings::is.MultiSettings(settings) ||
      PEcAn.settings::is.Settings(settings)) {
    write <- settings$database$bety$write
    return(start_model_runs(settings, write, stop.on.error))
  } else {
    PEcAn.logger::logger.severe(
      "runModule_start_model_runs only works with Settings or MultiSettings")
  }
} # runModule_start_model_runs