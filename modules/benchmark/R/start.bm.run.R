##-------------------------------------------------------------------------------------------------#
##' Start Benchmark Run
##'  
##' @name start.bm.run
##' @title Start benchmark run
##' @param benchmark_reference_run.id,
##' @param machine.id
##' @export 
##' 
##' @author Betsy Cowdery 

start.bm.run <- function(BRR.id, database, machine.id){ #- leverage workflow.R
#  - fill in new settings details
#  - start.runs
#  - update DB
#  - return a settings object (R-list) <- workflow.id, ensemble.id, benchmark_ensemble.id, etc.
#  - wait for runs to finish - status check function?
}

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
