##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name calc.benchmark 
##' @title Calculate benchmarking statistics
##' @param bm.ensemble object, either from create.BRR or start.bm.ensemle
##' @param con database connection
##' @export 
##' 
##' @author Betsy Cowdery 

calc.benchmark <- function(bm.ensemble, con){ 
  
  require(RPostgreSQL)
  require(XML)
  require(data.table)
  
  
  ens <- db.query(paste("SELECT * FROM ensembles where id = ",bm.ensemble$ensemble_id,";"), con) 
  wf <- db.query(paste("SELECT w.* FROM workflows as w join ensembles as e on w.id = e.workflow_id where e.id = ",bm.ensemble$ensemble_id,";"), con)
  site  <- query.site(wf$site_id, con)
  start_year <- year(wf$start_date)
  end_year <- year(wf$end_date)
  model_run <- dir(file.path(wf$folder,"out"), full.names=TRUE,  include.dirs = TRUE)[1]
  #How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  # All benchmarking records for the given benchmarking ensemble id
  bms <- db.query(paste("SELECT * FROM benchmarks as b join benchmarks_benchmarks_reference_runs as r on b.id=r.benchmark_id  join  benchmarks_ensembles as be on r.reference_run_id = be.reference_run_id where be.ensemble_id = ",bm.ensemble$ensemble_id,";"), con) 
  
  
  # Determine how many data sets inputs are associated with the benchmark id's
  inputs <- unique(bms$input_id)
  # bm.ids are split up in to groups according to their input data. 
  # So that the input data is only loaded once. 
  
  results <- list()
  
  for(obvs.id in inputs){
    
    bm.ids <- bms$id[which(bms$input_id == obvs.id)]
    data.path <- query.file.path(obvs.id,wf$hostname,con)
    format_full <- format <- query.format.vars(obvs.id,con) 
    # format is a list, metrics is a dataframe, both can be long. How to pass to calc.metrics remotely? 
    # Maybe as an Rdata file?
    
    results.list <- list()
    
    # Loop over benchmark ids
    for(i in 1:length(bm.ids)){
      bm <- db.query(paste("SELECT * from benchmarks where id =", bm.ids[i]), con)
      format$vars <- format_full$vars[nchar(format_full$vars$storage_type) > 0 | 
                                        format_full$vars$variable_id == bm$variable_id,]
      metrics <- db.query(paste("SELECT m.name, m.id from metrics as m JOIN benchmarks_metrics as b 
                              ON m.id = b.metric_id WHERE b.benchmark_id = ", bm.ids[i]),con)
      
      r <- calc.metrics(data.path, format, model_run, metrics, start_year, end_year, site,bm,ens)
      results.list <- append(results.list, list(r))
    } #end loop over benchmark ids
    
    results <- append(results, list(list(Reduce(function(...) merge(..., by="metric", all=T), 
                                                results.list), data.path, format_full)))
  }
  
  names(results) <- sprintf("input.%0.f",inputs)
  return(results)
  
}
