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

calc.benchmark <- function(bm.ensemble, con) {
  
  library(RPostgreSQL)
  library(data.table)
  
  ens <- db.query(paste("SELECT * FROM ensembles where id = ", bm.ensemble$ensemble_id, ";"), con)
  wf <- db.query(paste("SELECT w.* FROM workflows as w join ensembles as e on w.id = e.workflow_id", 
                       "WHERE e.id = ", bm.ensemble$ensemble_id, ";"), con)
  site <- query.site(wf$site_id, con)
  start_year <- lubridate::year(wf$start_date)
  end_year <- lubridate::year(wf$end_date)
  model_run <- dir(file.path(wf$folder, "out"), full.names = TRUE, include.dirs = TRUE)[1]
  # How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  # All benchmarking records for the given benchmarking ensemble id
  bms <- db.query(paste("SELECT * FROM benchmarks as b", "JOIN benchmarks_benchmarks_reference_runs as r on b.id=r.benchmark_id", 
                        "JOIN benchmarks_ensembles as be on r.reference_run_id = be.reference_run_id", "WHERE be.ensemble_id = ", 
                        bm.ensemble$ensemble_id, ";"), con)
  
  
  # Determine how many data sets inputs are associated with the benchmark id's
  inputs <- unique(bms$input_id)
  # bm.ids are split up in to groups according to their input data. 
  # So that the input data is only loaded once. 
  
  results <- list()
  
  for (obvs.id in inputs) {
    
    bm.ids <- bms$id[which(bms$input_id == obvs.id)]
    data.path <- query.file.path(obvs.id, wf$hostname, con)
    format_full <- format <- query.format.vars(obvs.id, con)
    
    # ---- LOAD INPUT DATA ---- #
    
    time.row <- format$time.row
    # vars.used.index is redundant and will be removed when I'm sure it won't break the PDA code.
    vars.used.index <- which(nchar(format$vars$storage_type) == 0)
    
    obvs <- load.data(data.path, format, start_year, end_year, site, vars.used.index, time.row)
    dat_vars <- format$vars$pecan_name  # IF : is this line redundant?
    obvs_full <- obvs
    
    # ---- LOAD MODEL DATA ---- #
    
    model_vars <- format$vars$pecan_name[-time.row]  # IF : what will happen when time.row is NULL? 
    # For example 'AmeriFlux.level2.h.nc' format (38) has time vars year-day-hour listed, 
    # but storage type column is empty and it should be because in load.netcdf we extract
    # the time from netcdf files using the time dimension we can remove time variables from
    # this format's related variables list or can hardcode 'time.row=NULL' in load.x_netcdf function
    model <- as.data.frame(read.output(runid = basename(model_run), 
                                       outdir = model_run, 
                                       start.year = start_year, 
                                       end.year = end_year,
                                       c("time", model_vars)))
    vars.used.index <- which(format$vars$pecan_name %in% names(model)[!names(model) == "time"])
    
    # We know that the model output time is days since the beginning of the year.
    # Make a column of years to supplement the column of days of the year.
    years <- start_year:end_year
    Diff <- diff(model$time)
    n <- model$time[c(which(Diff < 0), length(model$time))]
    y <- c()
    for (i in seq_along(n)) {
      y <- c(y, rep(years[i], n[i]))
    }
    model$year <- y
    model$posix <- strptime(paste(model$time, model$year), format = "%j %Y")
    model_full <- model
    
    #################################################### 
    results.list <- list()
    dat.list <- list()
    var.list <- c()
    
    # Loop over benchmark ids
    for (i in seq_along(bm.ids)) {
      bm <- db.query(paste("SELECT * from benchmarks where id =", bm.ids[i]), con)
      metrics <- db.query(paste("SELECT m.name, m.id from metrics as m", "JOIN benchmarks_metrics as b ON m.id = b.metric_id", 
                                "WHERE b.benchmark_id = ", bm.ids[i]), con)
      var <- filter(format$vars, variable_id == bm$variable_id)[, "pecan_name"]
      var.list <- c(var.list, var)
      
      obvs.bm <- obvs_full %>% select(., one_of(c("posix", var)))
      model.bm <- model_full %>% select(., one_of(c("posix", var)))
      
      out.calc.metrics <- calc.metrics(model.bm, 
                                       obvs.bm, 
                                       var, 
                                       metrics,
                                       start_year, end_year, 
                                       bm,
                                       ens,
                                       model_run)
      
      benchmarks_ensemble_id <- db.query(
        paste("SELECT id FROM benchmarks_ensembles where ensemble_id = ", ens$id),
        con)[[1]]
      # for(j in 1:out.calc.metrics[["r"]]){
      # db.query(paste0(
      #   "INSERT INTO benchmarks_ensembles_scores",
      #   "(score, benchmarks_ensemble_id, benchmark_id, metric_id, created_at, updated_at) VALUES ",
      #   "('",score[j],"',",benchmarks_ensemble_id,", ",bm$id,",",metrics$id[j],", NOW(), NOW())"),con)
      # }
      
      results.list <- append(results.list, list(out.calc.metrics[["r"]]))
      
      dat.list <- append(dat.list, list(out.calc.metrics[["dat"]]))
      
    }  #end loop over benchmark ids
    
    names(dat.list) <- var.list
    
    results <- append(results, 
                      list(list(bench.results = Reduce(function(...) merge(..., by = "metric", all = TRUE), results.list),
                                data.path = data.path, 
                                format = format_full$vars, 
                                model = model_full, 
                                obvs = obvs_full, 
                                aligned.dat = dat.list)))
  }
  
  names(results) <- sprintf("input.%0.f", inputs)
  return(results)
} # calc.benchmark
