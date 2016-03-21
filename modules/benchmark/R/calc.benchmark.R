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
  
  bm.id <- settings$benchmark$id #maybe something like that?
  
  # For each bm.id
  #  Query database for: input path, variables, site, start/end dates, metrics, formats
  
  data.path <- query.file.path(bm.id,settings$run$host$name,con)
  format <- query.format.vars(bm.id,con)  
  
  # Need to do something here to narrow down the variables to be used
  # rows <- which(vars_names_units$bety_name %in% calc.vars)
  # vars_names_units <- vars_names_units[rows,]
  
  site  <- query.site(settings$run$site$id, con)
  
  metrics <- db.query(paste("SELECT m.name from metrics as m JOIN benchmarks_metrics as b 
                            ON m.id = b.metric_id WHERE b.benchmark_id = ", bm.id),con)
  
  
  model_run <- dir(settings$run$host$outdir, full.names=TRUE)[1]
  #How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  # Local or remote
  # format is a list, metrics is a dataframe, both can be long. How to pass to calc.metrics remotely? 
  # Maybe as an Rdata file?
  results <- calc.metrics(data.path, format, model_run, metrics, 
                          start_year=NA, end_year=NA, site=NA)
  
  #  Update benchmark ensemble scores table
  
  metrics <- db.query(paste("SELECT * from inputs where id = ", bm.id),con)
  
  test <- db.query(paste("SELECT name from inputs where site_id = 1000000008"),con)
  
  typeof(test$name)

  
# Return results
return(results)

}
