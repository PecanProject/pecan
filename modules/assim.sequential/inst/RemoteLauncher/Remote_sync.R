#----------------------------------------------------------------
# Copying over the launcher and sending the command
#---------------------------------------------------------------
suppressPackageStartupMessages({
  library(PEcAn.settings)
  library(PEcAn.remote)
  library(PEcAn.assim.sequential)
})
is.active <- FALSE
args <- commandArgs(trailingOnly = TRUE)
#----------------------------------------------------------------
# Copying over the luncher and sending the command
#---------------------------------------------------------------
#Settings
if (is.na(args[1])){
  PEcAn.logger::logger.severe("No path to active xml setting run is defined.")
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
  my_host <- list(name = settings$host$name, tunnel = settings$host$tunnel, user = settings$host$user)
}

#Settings
if (is.na(args[2])){
  PEcAn.logger::logger.severe("No path to active remote SDA run is defined.")
} else {
  remote.path = args[2]
  sda.path <- paste0("dir.exists(\'",remote.path,"/SDA\')")
}

#Settings
if (is.na(args[3])){
  PEcAn.logger::logger.severe("No PID to active remote SDA run is defined.")
} else {
  PID = args[3]
}
#----------------------------------------------------------------
# Copying over the luncher and sending the command
#---------------------------------------------------------------
# Let's see if the PID is still running

is.active <- qsub_run_finished(run = PID, host = my_host, qstat = settings$host$qstat)
is.active <- ifelse (nchar(is.active) > 1, TRUE, FALSE)

#----------------------------------------------------------------
# looping 
#---------------------------------------------------------------
while(is.active){
  is.active <- qsub_run_finished(run = PID, host = my_host, qstat = settings$host$qstat)
  is.active <- ifelse (nchar(is.active) > 1, TRUE, FALSE)
  
  remote.copy.from(my_host,
                   paste0(remote.path,"/SDA"),
                   paste0(settings$outdir)
  )
  PEcAn.logger::logger.info(paste0("SDA folder was synced at ------------------- ", Sys.time()))
  Sys.sleep(3000)
}
#----------------------------------------------------------------
# Final Copying 
#---------------------------------------------------------------
sda.dir.exists <- remote.execute.R(sda.path,
                 my_host,
                 user = my_host$user,
                 scratchdir = ".")

if (sda.dir.exists){
    remote.copy.from(my_host,
                     paste0(remote.path,"/SDA"),
                     paste0(settings$outdir)
    )
}
PEcAn.logger::logger.info("------------------- Finished Syncing -------------------")
