##-------------------------------------------------------------------------------------------------#
##' Start Benchmark Ensemble
##'  
##' @name start.bm.ensemble
##' @title Start benchmark ensemble
##' @param benchmark_reference_run.id
##' @param con database connection
##' @export start.bm.ensemble
##' 
##' @author Betsy Cowdery 
start.bm.ensemble <- function(BRR, con) {
  
  old.settings <- file.path(BRR$settings, "pecan.xml")
  new.settings <<- sprintf("%s/%s.pecan.xml", BRR$settings, basename(BRR$settings))
  clean.settings(old.settings, new.settings)
  
  commandArgs <- function(trailingOnly = TRUE) new.settings
  source("web/workflow.R")
  
  ensemble.id <- db.query(sprintf("SELECT id from ensembles as e join workflows as w on e.workflow_id = w.id where w.id = ", 
                                  settings$workflow$id))
  
  db.query(paste0("INSERT INTO benchmarks_ensembles (reference_run_id, ensemble_id, model_id, user_id,  citation_id) VALUES(", 
                  BRR$id, ",", ensemble.id, ",", BRR$model_id, ", ", user_id, ", 1000000001 ) RETURNING *;"), 
           con)
} # start.bm.ensemble


#- leverage workflow.R
#  - fill in new settings details
#  - start.runs
#  - update DB
#  - return a settings object (R-list) <- workflow.id, ensemble.id, benchmark_ensemble.id, etc.
#  - wait for runs to finish - status check function?

# https://github.com/PecanProject/pecan/issues/204
# 
# Internally it would query required info from the database, 
# generate the full settings from the reference run, 
# start the run (or runs for ensemble-based runs), 
# and insert the new run and benchmark records in the database
# 
# Output would be full setting object (R list) for the run, 
# which would internally include things like the the 
# workflow.id, ensemble.id, benchmark_ensemble.id, etc.
# 
# When generating the new settings details for the local run, 
# if any files are missing (available in the inputs table but not on the machine of interest) 
# list the missing inputs and available dbfiles records showing which 
# machines have that file and where it's located. 
# Also, if files are missing don't start runs and instead return 
# an error code that stops the rest of the benchmark workflow from running 
# (including returning the above list of inputs and dbfiles
# for reference, rather than only printing them to screen)
# # 