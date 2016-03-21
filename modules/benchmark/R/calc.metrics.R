##' @name calc.metrics
##' @title calc.metrics
##' @export
##' @param data.path
##' @param format
##' @param model_run
##' @param metrics
##' @param start_year
##' @param end_year
##' 
##' @author Betsy Cowdery

calc.metrics <- function(data.path, format, model_run, metrics, start_year=NA, end_year=NA, site=NA){

  # Right now many argument are being read in as R list objects
  # This won't work remotely but at the moment I don't have a good solution
  
  # create results table
  results <- metrics
  results$score <- rep(NA, dim(metrics)[1])
  
  model <- read.output(model_run$id, model_run$outdir, start.year=NA, end.year=NA, var=format$bety_name) #which names????
  # model output list object with variable values (including time) variable name and units
  
  obvs <- load.data(data.path, format, start_year = NA, end_year=NA, site=NA)
  # observation list object with variable values (including time), variable name and units
  
  dat <- align.data(model, obvs)
  
  for(i in 1:length(metrics$name)){
    fcn <- paste("metric.",metrics$name[i])
    results$score[i] <- apply(fcn, dat)
  }
  
  return(results)
  
}
