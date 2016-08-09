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
  
  library(RPostgreSQL)
  library(XML)
  library(data.table)
  
  
  ens <- db.query(paste("SELECT * FROM ensembles where id = ",bm.ensemble$ensemble_id,";"), con) 
  wf <- db.query(paste(
    "SELECT w.* FROM workflows as w join ensembles as e on w.id = e.workflow_id",
    "WHERE e.id = ",bm.ensemble$ensemble_id,";"), con)
  site  <- query.site(wf$site_id, con)
  start_year <- year(wf$start_date)
  end_year <- year(wf$end_date)
  model_run <- dir(file.path(wf$folder,"out"), full.names=TRUE,  include.dirs = TRUE)[1]
  #How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  # All benchmarking records for the given benchmarking ensemble id
  bms <- db.query(paste(
    "SELECT * FROM benchmarks as b",
    "JOIN benchmarks_benchmarks_reference_runs as r on b.id=r.benchmark_id",
    "JOIN benchmarks_ensembles as be on r.reference_run_id = be.reference_run_id",
    "WHERE be.ensemble_id = ",bm.ensemble$ensemble_id,";"), con) 
  
  
  # Determine how many data sets inputs are associated with the benchmark id's
  inputs <- unique(bms$input_id)
  # bm.ids are split up in to groups according to their input data. 
  # So that the input data is only loaded once. 
  
  results <- list()
  
  for(obvs.id in inputs){
    
    bm.ids <- bms$id[which(bms$input_id == obvs.id)]
    data.path <- query.file.path(obvs.id,wf$hostname,con)
    format_full <- format <- query.format.vars(obvs.id,con) 

    # ---- LOAD INPUT DATA ---- #
    # Right now I'm making the inappropriate assumption that storage type will be 
    # empty unless it's a time variable. 
    # This is because I haven't come up for a good way to test that a character string is a date format

    format$vars <- format_full$vars %>% filter(., variable_id %in% db.query(paste(
      "SELECT variable_id from benchmarks WHERE id IN (", paste(bm.ids, collapse = ","),");"),con)[[1]] | 
        nchar(storage_type) > 0)  
    st <- format$vars$storage_type
    time.row <- which(nchar(st)>1 & substr(st, 1,1) == "%")
    # vars.used.index is redundant and will be removed when I'm sure it won't break the PDA code. 
    vars.used.index <- which(nchar(st)==0) 
    
    obvs <- load.data(data.path, format, start_year, end_year, site, vars.used.index, time.row)
    dat_vars <- format$vars$pecan_name
    obvs_full = obvs
    
    # ---- LOAD MODEL DATA ---- #

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


    ####################################################
    results.list <- list()

    # Loop over benchmark ids
    for(i in 1:length(bm.ids)){
      bm <- db.query(paste("SELECT * from benchmarks where id =", bm.ids[i]), con)
      metrics <- db.query(paste(
        "SELECT m.name, m.id from metrics as m",
        "JOIN benchmarks_metrics as b ON m.id = b.metric_id",
        "WHERE b.benchmark_id = ", bm.ids[i]),con)
      var <- filter(format$vars, variable_id == bm$variable_id)[,"pecan_name"]
      
      obvs.bm <- obvs_full %>% select(., one_of(c("posix", var )))
      model.bm <- model_full %>% select(., one_of(c("posix", var )))

      r <- calc.metrics(model.bm, obvs.bm, var, metrics, start_year, end_year, bm, ens, model_run)
      
      benchmarks_ensemble_id <- db.query(paste("SELECT id FROM benchmarks_ensembles where ensemble_id = ",
                                               ens$id),con )[[1]]
      # for(j in 1:nrow(r)){
      # db.query(paste0(
      #   "INSERT INTO benchmarks_ensembles_scores",
      #   "(score, benchmarks_ensemble_id, benchmark_id, metric_id, created_at, updated_at) VALUES ",
      #   "('",score[j],"',",benchmarks_ensemble_id,", ",bm$id,",",metrics$id[j],", NOW(), NOW())"),con)
      # }
      
      results.list <- append(results.list, list(r))
      
    } #end loop over benchmark ids
    
    
    
    results <- append(results,
                      list(list( bench.results = Reduce(function(...) merge(..., by="metric", all=T), 
                                                results.list),
                                 data.path = data.path, 
                                 format = format_full)))
  }
  
  names(results) <- sprintf("input.%0.f",inputs)
  return(results)
  
}
