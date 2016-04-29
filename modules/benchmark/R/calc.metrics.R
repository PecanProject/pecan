##' @name calc.metrics
##' @title calc.metrics
##' @export
##' @param data.path
##' @param format
##' @param vars_used
##' @param model_run
##' @param metrics
##' @param start_year
##' @param end_year
##' 
##' 
##' @author Betsy Cowdery

calc.metrics <- function(data.path, format, vars_used, model_run, metrics, start_year, end_year, site){
  
  # Right now many argument are being read in as R list objects
  # This won't work remotely but at the moment I don't have a good solution
  
  # create results table
  results <- as.data.frame(matrix(NA, nrow = length(vars_used$pecan_name), ncol = length(metrics$name)))
  rownames(results) <- vars_used$pecan_name
  colnames(results) <- metrics$name
  
  # Do I need these?
  # start.year <- lubridate::year(settings$run$start.date)
  # end.year <- lubridate::year(settings$run$end.date)
  
  model <- as.data.frame(read.output(runid=basename(model_run), outdir=model_run, start.year=start_year, end.year=end_year, format$vars$pecan_name))
  # returns model output list object with variable values (including time), variable name and units
  
  vars_used <- as.data.frame(matrix(NA, nrow = length(names(model)), ncol = ncol(format$vars)))
  colnames(vars_used) <- colnames(format$vars)
  for(i in  1:length(names(model))){
    vars_used[i,] <- format$vars[which(format$vars$pecan_name == names(model)[i]),]
  }
  
  obvs <- load.data(data.path, format, start_year, end_year, site=settings$run$site, vars_used)
  # observation list object with variable values (including time), variable name and units
  
  dat <- align.data(model, obvs, vars_used, start_year, end_year)
  
  for(v in 1:length(vars_used$pecan_name)){
    metric_dat <- dat[,c(paste(vars_used$pecan_name[v], c("model", "obvs"),sep = "_" ),"time")]
    colnames(metric_dat)<- c("model","obvs","time")
    #for(m in 1:length(metrics$name)){
      fcn <- paste0("metric.",metrics$name[m])
      results[vars_used$pecan_name[v],metrics$name[m]] <- do.call(fcn,list(metric_dat, vars_used$pecan_name[v]))
    #}
  }


return(results)

}
