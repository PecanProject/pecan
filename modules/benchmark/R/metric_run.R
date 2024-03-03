##' Model Run Check
##'
##' @param settings list
##' 
##' @author Betsy Cowdery
##' @export
metric_run <- function(settings){

  # The goal of this function is to determine if a model run has been successfully completed. 
  # There are three steps to determining success. 
  
  # Check the STATUS file to see if all the steps so far have completed
  # - If "ERROR"  ==> FAIL
  #   - Grab the error message and stage at which the workflow failed.
  # - If all "DONE": this means the run completed, but it could still have problems
  #   - Check to see if output exists 
  #     - If output exists, 
  #       - If there is no output or output is not the right length ==> FAIL
  #       - If output is the expected length ==> SUCCESS 
  # 
  # Open metric_run.Rdata 
  # Save in metric_run.Rdata:
  # - run information (run ID?, do we need to look that up in the database? If so, that should happen in calc_benchmark)
  # - SUCCESS/FAIL
  # - stage of failure
  # - error message
  # 
  # Calculate the benchmark metric_run score. 
  # What should this be?
  # 
  # Numeric?
  # 0 = SUCCESS
  # 1 = FAIL MODEL RUN, produces output
  # 2 = FAIL MODEL RUN, produces no output
  # 3 = FAIL CONFIG
  # 4 = FAIL META
  # 5 = FAIL TRAIT
  # 
  # Text?
  # SUCCESS/FAIL STAGE and error message?
  # 
  # return(score)
  
}



