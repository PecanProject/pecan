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
  
  bm.id <- settings.bm$run$id #maybe something like that?
  
  # For each bm.id
  #  Query database for: input path, variables, site, start/end dates, metrics, formats
  
  data.path <- query.file.path(input.id,host_name,con)
  format <- query.format.vars(input.id,con)  
  
  # Need to do something here to narrow down the variables to be used
  # rows <- which(vars_names_units$bety_name %in% calc.vars)
  # vars_names_units <- vars_names_units[rows,]
  
  site  <- query.site(input.id, con)
  
  metrics <- db.query(paste("SELECT m.id, m.name from metrics as m JOIN benchmarks_metrics as b 
                            ON m.id = b.metric_id WHERE b.benchmark_id = ", bm.id),con)
  
  # Local or remote
  results <- calc.metrics(data.path, format, model_run, metrics, 
                          start_year=NA, end_year=NA, site=NA)
  
  #  Update benchmark ensemble scores table
  

# Return results
return(results)

}
