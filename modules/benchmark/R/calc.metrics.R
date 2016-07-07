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
##' 
##' @author Betsy Cowdery

calc.metrics <- function(data.path, format, model_run, metrics, start_year, end_year, site, bm, ens){
  
  # Right now I'm making the inappropriate assumption that storage type will be 
  # empty unless it's a time variable. 
  # This is because I haven't come up for a good way to test that a character string is a date format
  st <- format$vars$storage_type
  
  time.row <- which(nchar(st)>1 & substr(st, 1,1) == "%")
  
  # ---- MODEL DATA ---- #
  
  model_vars <- format$vars$pecan_name[-time.row]
  model <- as.data.frame(read.output(runid=basename(model_run), outdir=model_run, 
                                     start.year=start_year, end.year=end_year, 
                                     c("time",model_vars)))
  vars.used.index <- which(format$vars$pecan_name %in% names(model)[!names(model)=="time"])
  
  # We know that the model output time is days since the beginning of the year.
  # Make a column of years to supplement the column of days of the year.
  years <- start_year:end_year
  Diff <- diff(model$time)
  n <- model$time[c(which(Diff < 0), length(model$time))]
  y <- c()
  for(i in 1:length(n)){
    y <- c(y, rep(years[i], n[i]))
  }
  model$year <- y
  model$posix <- strptime(paste(model$time, model$year), format = "%j %Y")
  model_full = model
  
  # ---- INPUT DATA ---- #
  
  obvs <- load.data(data.path, format, start_year, end_year, site, vars.used.index, time.row)
  dat_vars <- format$vars$pecan_name[vars.used.index]
  obvs_full = obvs
  
  #########################

  dat <- align.data(model_full, obvs_full, dat_vars, start_year, end_year)
  
  results <- as.data.frame(matrix(NA, nrow = length(metrics$name), ncol = length(dat_vars)+1))
  colnames(results) <- c("metric", dat_vars)
  rownames(results) <- metrics$name
  results$metric <- metrics$name
  
  file.sources = list.files("modules/benchmark/R/", pattern = "^metric.*", full.names = TRUE)
  for(i in 1:length(file.sources)){
    source(file.sources[i])
  }
  
  for(v in 1:length(dat_vars)){
    metric_dat <- dat[,c(paste(dat_vars[v], c("m", "o"),sep = "." ),"posix")]
    colnames(metric_dat)<- c("model","obvs","time")
    
    for(m in 1:length(metrics$name)){
      
      fcn <- paste0("metric.",metrics$name[m])
      score <- as.character(do.call(fcn, args <- list(metric_dat,dat_vars[v])))
      results[metrics$name[m],dat_vars[v]] <- score
      
      benchmarks_ensemble_id <- db.query(paste("SELECT id FROM benchmarks_ensembles where ensemble_id = ", ens$id),con )[[1]]
      
      db.query(sprintf("INSERT into benchmarks_ensembles_scores (score, benchmarks_ensemble_id, benchmark_id, metric_id, created_at, updated_at) VALUES ('%s', %.0f, %.0f, %.0f, NOW(), NOW())",
                       score, benchmarks_ensemble_id, bm$id, metrics$id[m]),con)
      
    } #end loop over metrics
  } #end loop over variables

return(results)
}
