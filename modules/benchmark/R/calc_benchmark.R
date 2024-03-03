##' Calculate benchmarking statistics
##'
##' For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
##'  
##' @param settings settings object describing the run to calculate
##' @param bety database connection
##' @param start_year,end_year time range to read. If NA, these are taken from `settings`
##' @export 
##' 
##' @author Betsy Cowdery 
##' @importFrom dplyr tbl filter rename collect select  
calc_benchmark <- function(settings, bety, start_year = NA, end_year = NA) {
  
  # run.score <- run.success.check(settings)
  
  if("benchmarking" %in% names(settings)){
    
    # If "run" is in the list of benchmarking metrics, add run.score record to the database
    # How are we dealing with ensemble runs? This is still an issue that has not been dealt with elsewhere in the code. 
    # For now this design only works with sigle run ensembles. 
    
    ##### This is where calc_benchmarks originally started 
    
    # Update benchmarks_ensembles and benchmarks_ensembles_scores tables
    
    ensemble <- tbl(bety,'ensembles') %>% filter(.data$workflow_id == settings$workflow$id) %>% collect()
    
    # Retrieve/create benchmark ensemble database record
    bm.ensemble <- tbl(bety,'benchmarks_ensembles') %>% 
      filter(.data$reference_run_id == settings$benchmarking$reference_run_id,
             .data$ensemble_id %in% ensemble$id,  # ensemble$id has more than one element
             .data$model_id == settings$model$id) %>%
      collect()
    
    if(dim(bm.ensemble)[1] == 0){
      bm.ensemble <- PEcAn.DB::db.query(paste0("INSERT INTO benchmarks_ensembles",
                                     "(reference_run_id, ensemble_id, model_id, ",
                                     "user_id, citation_id)",
                                     "VALUES(",settings$benchmarking$reference_run_id,
                                     ", ",ensemble$id,
                                     ", ",settings$model$id,", ",settings$info$userid,
                                     ", 1000000001 ) RETURNING *;"), bety)
    }else if(dim(bm.ensemble)[1] >1){
      PEcAn.logger::logger.error("Duplicate record entries in benchmarks_ensembles")
    }
    
    # --------------------------------------------------------------------------------------------- #
    # Setup
    
    site <- PEcAn.DB::query.site(settings$run$site$id, bety)
    model_run <- dir(settings$modeloutdir, full.names = TRUE, include.dirs = TRUE)[1]
    # How are we dealing with ensemble runs? Right now I've hardcoded to select the first run.
    
    # All benchmarking records for the given benchmarking ensemble id
    # The benchmark entries returned from this query will include all previous 
    # benchmarks that have ever been done for the ensemble id. 
    # For now all benchmarks will be (re)calculated.  
    # This is could become problematic if previous benchmarks were
    # calculated with multiple inputs, which would mean that all of that data 
    # would need to be loaded and aligned again. 
    
    bms <- tbl(bety,'benchmarks') %>% dplyr::rename(benchmark_id = "id") %>% 
      dplyr::left_join(tbl(bety, "benchmarks_benchmarks_reference_runs"), by="benchmark_id") %>% 
      dplyr::filter(.data$reference_run_id == settings$benchmarking$reference_run_id) %>% 
      dplyr::select(dplyr::one_of("benchmark_id", "input_id", "site_id", "variable_id", "reference_run_id")) %>%
      dplyr::collect() %>%
      dplyr::filter(.data$benchmark_id %in% unlist(settings$benchmarking[which(names(settings$benchmarking) == "benchmark_id")]))
    
    var.ids <- bms$variable_id
    
    # --------------------------------------------------------------------------------------------- #
    # Determine how many data sets inputs are associated with the benchmark id's
    # bm.ids are split up in to groups according to their input data. 
    # So that the input data is only loaded once. 
    
    results <- list()
    
    # input.id = unique(bms$input_id) # For testing
    for (input.id in unique(bms$input_id)) {
      
      # Create directory that will hold benchmarking results
      bm_dir <- file.path(dirname(dirname(model_run)), "benchmarking", input.id)
      dir.create(dirname(bm_dir))
      dir.create(bm_dir)
      
      bm.ids <- bms$benchmark_id[which(bms$input_id == input.id)]
      data.path <- PEcAn.DB::query.file.path(input.id, settings$host$name, bety)
      format_full <- format <- PEcAn.DB::query.format.vars(input.id = input.id, bety, format.id = NA, var.ids=var.ids)
      
      # ---- LOAD INPUT DATA ---- #
      
      time.row <- format$time.row
      vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
      
      if(is.na(start_year)) start_year <- lubridate::year(settings$run$start.date)
      if(is.na(end_year))   end_year <- lubridate::year(settings$run$end.date)
      
      obvs <- load_data(data.path, format, start_year = start_year, end_year = end_year, site, vars.used.index, time.row)
      dat_vars <- format$vars$pecan_name  # IF : is this line redundant?
      obvs_full <- obvs

      # ---- LOAD MODEL DATA ---- #
      
      #model_vars <- format$vars$pecan_name[-time.row]  # IF : what will happen when time.row is NULL? 
      model_vars <- format$vars$pecan_name # time.row is NULL
      # For example 'AmeriFlux.level2.h.nc' format (38) has time vars year-day-hour listed, 
      # but storage type column is empty and it should be because in load_netcdf we extract
      # the time from netcdf files using the time dimension we can remove time variables from
      # this format's related variables list or can hardcode 'time.row=NULL' in load_x_netcdf function
      read.model <- PEcAn.utils::read.output(runid = basename(model_run), 
                                outdir = model_run, 
                                start.year = start_year, 
                                end.year = end_year,
                                c("time", model_vars), dataframe = TRUE)
      
      model <- read.model
      vars.used.index <- which(format$vars$pecan_name %in% names(model)[!names(model) == "time"])
      model_full <- model
      
      # ---- CALCULATE BENCHMARK SCORES ---- #
      
      results.list <- list()
      dat.list <- list()
      var.list <- c()
      
      
      # Loop over benchmark ids
      # i = 1 # for testing
      for (i in seq_along(bm.ids)) {
        bm <- PEcAn.DB::db.query(paste("SELECT * from benchmarks where id =", bm.ids[i]), bety)
        metrics <- PEcAn.DB::db.query(paste("SELECT m.name, m.id from metrics as m", 
                                  "JOIN benchmarks_metrics as b ON m.id = b.metric_id", 
                                  "WHERE b.benchmark_id = ", bm.ids[i]), bety)
        #"run" metric needs to be removed from metrics so it isn't computed twice
        var <- dplyr::filter(format$vars, .data$variable_id == bm$variable_id)[, "pecan_name"]
        var.list <- c(var.list, var)
        
        obvs.calc <- obvs_full %>% dplyr::select(dplyr::one_of(c("posix", var)))
        obvs.calc[,var] <- as.numeric(obvs.calc[,var])
        model.calc <- model_full %>% dplyr::select(dplyr::one_of(c("posix", var)))
        
        # Check that the variables actually got loaded, otherwise don't send to calc_metrics
        
        if(!(var %in% names(obvs.calc))|!(var %in% names(model.calc))){
          PEcAn.logger::logger.warn(paste0("Load did not work for ",var,". No metrics will be calculated."))
          next
        }
        
        # TODO: If the scores have already been calculated, don't redo
        ensemble.id = bm.ensemble$ensemble_id # this is just to make debugging easier
        
        out.calc_metrics <- calc_metrics(model.calc, 
                                         obvs.calc, 
                                         var, 
                                         metrics,
                                         ensemble.id,
                                         bm_dir)
        
        for(metric.id in metrics$id){
          metric.name <- dplyr::filter(metrics,.data$id == metric.id)[["name"]]
          score <- out.calc_metrics[["benchmarks"]] %>% 
            dplyr::filter(.data$metric == metric.name) %>% 
            dplyr::select(score)
          
          # Update scores in the database
          
          score.entry <- tbl(bety, "benchmarks_ensembles_scores") %>%
            dplyr::filter(.data$benchmark_id == bm.ids[i]) %>%
            dplyr::filter(.data$benchmarks_ensemble_id == bm.ensemble$id) %>%
            dplyr::filter(.data$metric_id == metric.id) %>% 
            dplyr::collect()
          
          # If the score is already in the database, should check if it is the same as the calculated 
          # score. But this requires a well written regular expression since it can be matching text. 
          
          if(dim(score.entry)[1] == 0){
            PEcAn.DB::db.query(paste0(
              "INSERT INTO benchmarks_ensembles_scores",
              "(score, benchmarks_ensemble_id, benchmark_id, metric_id) VALUES ",
              "('",score,"',",bm.ensemble$id,", ",bm$id,",",metric.id,")"), bety)
          }else if(dim(score.entry)[1] >1){
            PEcAn.logger::logger.error("Duplicate record entries in scores")
          }
        }
        results.list <- append(results.list, list(out.calc_metrics[["benchmarks"]]))
        dat.list <- append(dat.list, list(out.calc_metrics[["dat"]]))
      }  #end loop over benchmark ids
      
      table.filename <- file.path(bm_dir, paste("benchmark.scores", var, bm.ensemble$ensemble_id, "pdf", sep = "."))
      grDevices::pdf(file = table.filename)
      gridExtra::grid.table(do.call(rbind, results.list))
      grDevices::dev.off()
      
      var.names <- c()
      for(k in seq_along(dat.list)){
        var.names <- c(var.names,unlist(strsplit(names(dat.list[[k]])[grep("[.]m", names(dat.list[[k]]))],"[.]"))[1]) # This is horrifying. Sorry future self. 
      }
      names(dat.list) <- var.names
      
      result.out <- list(bench.results = do.call(rbind, results.list),
                         data.path = data.path, 
                         format = format_full$vars, 
                         model = model_full, 
                         obvs = obvs_full, 
                         aligned.dat = dat.list)
      save(result.out, file = file.path(bm_dir,"benchmarking.output.Rdata"))
      
      results <- append(results, list(result.out)) # For testing
    } # end loop over input ids

    names(results) <- sprintf("input.%0.f", unique(bms$input_id)) # For testing
    return(invisible(results)) # For testing
  }
} # calc_benchmark
