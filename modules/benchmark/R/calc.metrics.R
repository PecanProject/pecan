calc.metrics <- function(input_path, format_table, vars_names_units, model_run, metrics, start_year=NA, end_year=NA, site=NA){

  # Right now parameters are being read in as R objects - this won't work remotely but 
  # I haven't decided exactly how to pass the parameters over
  
  # create results table
  results <- metrics
  results$score <- rep(NA, dim(metrics)[1])
  
  model <- read.output(model_run$id, model_run$outdir, start_year=NA, end_year=NA, var=vars_names_units$bety_names) #which names????
  # model output list object with values, variable name and units 
  
  obvs <- load.data(input_path, format_table, vars_names_units, start_year = NA, end_year=NA, site=NA)
  # observation list object with values, variable name and units 
  
  dat <- align.data(model, obvs)
  
  for(i in 1:length(metrics$name){
    fcn <- paste("metric.",metrics$name[i])
    results$score[i] <- apply(fcn, dat)
  }
  
  return(results)
  
}