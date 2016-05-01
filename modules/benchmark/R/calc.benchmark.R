##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name calc.benchmark 
##' @title Calculate benchmarking statistics
##' @param settings object from start.benchmark.runs (for now - we many only need a few variables)
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 

calc.benchmark <- function(settings, con){ #settings file is output from start.benchmark.runs
  
  require(RPostgreSQL)
  require(XML)
  require(data.table)
  
  
  # Query BRR for benchmark ids
  
  bm.id <- settings$benchmark$id 
  obvs.id <- settings$benchmark$input_id
  
  
  # For each bm.id - don't have this written as a loop yet. 
  #  Query database for: input path, variables, site, start/end dates, metrics, formats
  
  data.path <- query.file.path(obvs.id,settings$run$host$name,con)
  format <- query.format.vars(obvs.id,con)  
  
  site  <- query.site(settings$run$site$id, con)
  
  metrics <- db.query(paste("SELECT m.name from metrics as m JOIN benchmarks_metrics as b 
                              ON m.id = b.metric_id WHERE b.benchmark_id = ", bm.id),con)
  
  model_run <- dir(settings$run$host$outdir, full.names=TRUE)[1]
  #How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  # Local or remote
  # format is a list, metrics is a dataframe, both can be long. How to pass to calc.metrics remotely? 
  # Maybe as an Rdata file?
  
  start_year <- year(settings$run$start.date)
  end_year <- year(settings$run$end.date)
  
  results <- calc.metrics(data.path, format, vars_used, model_run, metrics, start_year, end_year, site)
  
  
  #  Update benchmark ensemble scores table
  

# Return results
return(results)

}
