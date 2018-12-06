##' Creates records for benchmarks, benchmarks_benchmarks_reference_runs, benchmarks_metrics
##'
##' @name define_benchmark
##' @title Benchmark Definition: Retrieve or Create Bety Benchmarking Records
##' @param bm.settings settings list
##' @return updated settings list
##' @author Betsy Cowdery
##' @export define_benchmark
##' @importFrom dplyr tbl filter rename collect select
define_benchmark <- function(settings, bety){
  
  if (is.MultiSettings(settings)) {
    return(papply(settings, function(x) define_benchmark(x, bety)))
  }
  bm.settings <- settings$benchmarking
  
  PEcAn.logger::logger.info(paste("Ensemble id:", bm.settings$ensemble_id))
  PEcAn.logger::logger.info(paste(!is.null(bm.settings$ensemble_id)))
  # Retrieve/create benchmark entries
  
  if(is.null(bm.settings$reference_run_id)){
    if(!is.null(bm.settings$ensemble_id)){
      
      # check if there is already a BRR for ensemble.id, otherwise make one
      bm_ens <- tbl(bety,"benchmarks_ensembles") %>% rename(bm_ensemble_id = id) %>% 
        filter(ensemble_id == bm.settings$ensemble_id) %>% collect()
      
      if(length(bm_ens) == 0){
        # Get workflow id from ensemble id
        ens_wf <- tbl(bety, 'ensembles') %>% filter(id == bm.settings$ensemble_id) %>% 
          rename(ensemble_id = id) %>% 
          left_join(.,tbl(bety, "workflows") %>% rename(workflow_id = id), by="workflow_id") %>% collect()
        BRR <- create_BRR(ens_wf, con = bety$con, user_id = settings$info$userid)
      }else if(dim(bm_ens)[1] == 1){
        BRR <- tbl(bety,"reference_runs") %>% filter(id == bm_ens$reference_run_id) %>% 
          rename(reference_run_id = id) %>% collect()
      }else if(dim(bm_ens)[1] > 1){ # There shouldn't be more than one reference run per run
        PEcAn.logger::logger.error("There is more than one reference run in the database for this ensemble id. Review for duplicates. ")
      }
      # add the ref_run id, remove the ensemble_id
      bm.settings$reference_run_id <- BRR$reference_run_id
      # bm.settings$ensemble_id <- NULL
      
    }else{logger.error("Cannot find or create benchmark reference run")}
  }else{logger.debug("Reference run already created")}
  
  
  # Retrieve/create benchmark entries
  which.bm <- which(names(bm.settings) == "benchmark")
  
  for(i in which.bm){
    benchmark <- bm.settings[[i]]
    
    # Create benchmark records using all metrics
    # Unless individual metrics are specified in settings
    # This can be expanded to "suits" of metrics 
    # (for example, all plots or all regression based metrics)
    if(!is.null(benchmark$metrics)){
      metric_ids <- as.numeric(unlist(benchmark$metrics))
    }else{
      metric_ids <- tbl(bety, 'metrics') %>% pull(id)
    }
    
    # If site is not specified in benchmark settings (this may be unnecessary)
    # Use ensemble site
    if(!is.null(benchmark$site_id)){
      site_id <- as.numeric(benchmark$site_id)
    }else{
      site_id <- as.numeric(strsplit(strsplit(BRR$settings, "<site>\n* *<id>")[[1]][2], "</id>")[[1]][1])
      # This doesn't seem like a good way to read the settings string, would love suggestions for something better
    }
    
    bm <- tbl(bety, 'benchmarks') %>% 
      filter(input_id == benchmark$input_id) %>%
      filter(variable_id == benchmark$variable_id) %>%
      filter(site_id == site_id) %>% collect()
    
    # Retrieve/create benchmark record
    if(dim(bm)[1] == 0){
      cmd <- sprintf(paste0("INSERT INTO benchmarks (input_id, variable_id, site_id, user_id)",
                            "VALUES ( %s, %s, %s, %s) RETURNING * ;"), 
                     benchmark$input_id, benchmark$variable_id,
                     site_id, settings$info$userid)
      bm <- db.query(cmd, bety$con)
      logger.debug(sprintf("Benchmark %.0f for input %.0f variable %.0f created", 
                           bm$id, bm$input_id, bm$variable_id))
    }else if(dim(bm)[1] >1){
      PEcAn.logger::logger.error(sprintf("DUPLICATE records exist for input %.0f variable %.0f", 
                                        as.numeric(benchmark$input_id), benchmark$variable_id))
    }else{
      logger.debug(sprintf("Benchmark %.0f for input %.0f variable %.0f exists", 
                           bm$id, bm$input_id, bm$variable_id))      
    }
    
    # Retrieve/create benchmarks_benchmarks_reference_runs record
    bmBRR <- tbl(bety, 'benchmarks_benchmarks_reference_runs') %>% 
      filter(benchmark_id == bm$id) %>%
      filter(reference_run_id == bm.settings$reference_run_id)  %>% collect()
    
    if(dim(bmBRR)[1] == 0){
      cmd <- sprintf(paste0("INSERT INTO benchmarks_benchmarks_reference_runs",
                            " (benchmark_id, reference_run_id) VALUES (%s, %s)"),
                     bm$id, bm.settings$reference_run_id)
      db.query(cmd, bety$con)
    }else if(dim(bmBRR)[1] > 1){
      PEcAn.logger::logger.error("Duplicate record entries in benchmarks_benchmarks_reference_runs")
    }
    
    # Retrieve/create benchmarks_metrics record
    for(k in seq_along(metric_ids)){
      bmmetric <- tbl(bety, 'benchmarks_metrics') %>% 
        filter(benchmark_id == bm$id) %>%
        filter(metric_id == metric_ids[[k]])  %>% collect()
      
      if(dim(bmmetric)[1] == 0){
        cmd <- sprintf(paste0("INSERT INTO benchmarks_metrics (benchmark_id, metric_id) VALUES (%s, %s)"),
                       bm$id, metric_ids[[k]])
        db.query(cmd, bety$con)
      }else if(dim(bmmetric)[1] > 1){
        PEcAn.logger::logger.error("Duplicate record entries in benchmarks_metrics")
      }
    } # end loop over metric ids
    
    benchmark$benchmark_id <- bm$id
    bm.settings[[i]] <- benchmark
    
  } # End loop over benchmarks in settings
  
  invisible(return(bm.settings))
} # create.benchmark
