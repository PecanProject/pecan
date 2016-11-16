##-------------------------------------------------------------------------------------------------#
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @name calc.benchmark 
##' @title Calculate benchmarking statistics
##' @param bm.ensemble object, either from create.BRR or start.bm.ensemle
##' @param bety database connection
##' @export 
##' 
##' @author Betsy Cowdery 

calc.benchmark <- function(settings, bety) {
  
  # dplyr functions
  tbl     <- dplyr::tbl
  filter  <- dplyr::filter
  rename  <- dplyr::rename
  collect <- dplyr::collect
  select  <- dplyr::select
  
  site <- query.site(settings$run$site$id, bety$con)
  start_year <- lubridate::year(settings$run$start.date)
  end_year <- lubridate::year(settings$run$end.date)
  model_run <- dir(settings$modeloutdir, full.names = TRUE, include.dirs = TRUE)[1]
  # How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
  
  var.ids <- as.numeric(unname(unlist(settings$benchmark$variables)))
  metric.ids <- as.numeric(unname(unlist(settings$benchmark$metrics)))
  
  # All benchmarking records for the given benchmarking ensemble id
  bms <- tbl(bety,'benchmarks') %>% rename(benchmark_id = id) %>%  
    left_join(.,tbl(bety, "benchmarks_benchmarks_reference_runs"), by="benchmark_id") %>% 
    filter(reference_run_id == settings$benchmark$reference_run_id) %>% 
    select(one_of("benchmark_id", "input_id", "site_id", "variable_id", "reference_run_id")) %>%
    collect() %>%
    filter(variable_id %in% var.ids)
  
  
  # Determine how many data sets inputs are associated with the benchmark id's
  # bm.ids are split up in to groups according to their input data. 
  # So that the input data is only loaded once. 
  
  results <- list()
  
  for (input.id in unique(bms$input_id)) {
    
    bm.ids <- bms$benchmark_id[which(bms$input_id == input.id)]
    data.path <- query.file.path(input.id, settings$host$name, bety$con)
    format_full <- format <- query.format.vars(input.id, bety, format.id = NA, var.ids=var.ids)
    
    # ---- LOAD INPUT DATA ---- #
    
    time.row <- format$time.row
    vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
    
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
      bm <- db.query(paste("SELECT * from benchmarks where id =", bm.ids[i]), bety$con)
      metrics <- db.query(paste("SELECT m.name, m.id from metrics as m", 
                                "JOIN benchmarks_metrics as b ON m.id = b.metric_id", 
                                "WHERE b.benchmark_id = ", bm.ids[i]), bety$con) %>%
        filter(id %in% metric.ids)
      var <- filter(format$vars, variable_id == bm$variable_id)[, "pecan_name"]
      var.list <- c(var.list, var)
      
      obvs.calc <- obvs_full %>% select(., one_of(c("posix", var)))
      model.calc <- model_full %>% select(., one_of(c("posix", var)))
      
      # TODO: If the scores have already been calculated, don't redo
      
      out.calc.metrics <- calc.metrics(model.calc, 
                                       obvs.calc, 
                                       var, 
                                       metrics,
                                       start_year, end_year, 
                                       bm,
                                       ensemble.id = settings$benchmark$ensemble_id,
                                       model_run)
      
      for(metric.id in metrics$id){
        metric <- filter(metrics,id == metric.id)[["name"]]
        score <- out.calc.metrics[["benchmarks"]] %>% filter(.,metric == metric) %>% select(score)
      #   score.entry <- tbl(bety, "benchmarks_ensembles_scores") %>% 
      #     filter(score == score) %>% 
      #     filter(bechmarks_ensemble_id == settings$benchmark$ensemble_id)
      #     
      # }else if(dim(bm)[1] >1){
      #   logger.error("Duplicate record entries in benchmarks")
      # }
        
        bm_ens_id <- tbl(bety, "benchmarks_ensembles") %>%
          filter(ensemble_id == settings$benchmark$ensemble_id) %>%
          filter(reference_run_id == settings$benchmark$reference_run_id) %>%
          filter(model_id == settings$model$id) %>%
          select(id) %>% collect %>% .[[1]]
        
      db.query(paste0(
        "INSERT INTO benchmarks_ensembles_scores",
        "(score, benchmarks_ensemble_id, benchmark_id, metric_id) VALUES ",
        "('",score,"',",bm_ens_id,", ",bm$id,",",metric.id,")"),bety$con)
      }
      
      results.list <- append(results.list, list(out.calc.metrics[["benchmarks"]]))
      dat.list <- append(dat.list, list(out.calc.metrics[["dat"]]))
      
    }  #end loop over benchmark ids
    
    names(dat.list) <- var.list
    results <- append(results, 
                      list(list(bench.results = do.call(rbind, results.list),
                                data.path = data.path, 
                                format = format_full$vars, 
                                model = model_full, 
                                obvs = obvs_full, 
                                aligned.dat = dat.list)))
  }
  
  names(results) <- sprintf("input.%0.f", unique(bms$input_id))
  save(results, file = file.path(settings$outdir,"benchmarking.output.Rdata"))
  
  return(invisible(results))
} # calc.benchmark
