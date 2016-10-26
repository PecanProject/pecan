##' Creates records for benchmarks_ensembles, benchmarks, benchmarks_benchmarks_reference_runs, benchmarks_metrics
##'
##' @title Create Bety Benchmarking Records
##' @param settings settings list
##' @return updated settings list
##' @author Betsy Cowdery
##' @export create.benchmark

create.benchmark <- function(settings, bety){
  
  # Get benchmark ensemble information
  
  if(as.logical(settings$benchmark$new_run)){
    # If new run, need a new emsemble id so that we can load output
    # For now, just choosing the first in case ensemble size > 1
    settings$benchmark$ensemble_id <- tbl(bety, 'ensembles') %>% filter(workflow_id == settings$workflow$id) %>% dplyr::select(id) %>% collect() %>% .[[1]] 
    bm.ensemble <- db.query(paste0("INSERT INTO benchmarks_ensembles",
                                   "(reference_run_id, ensemble_id, model_id, ",
                                   "user_id, created_at, updated_at, citation_id)",
                                   "VALUES(",settings$benchmark$reference_run_id,
                                   ", ",settings$benchmark$ensemble_id,
                                   ", ",settings$model$id,", ",settings$info$userid,
                                   ", NOW() , NOW(), 1000000001 ) RETURNING *;"), bety$con)
    bm.ensemble <- rename(bm.ensemble, bm_ensemble_id = id)
    
  }else{
    bm.ensemble <- tbl(bety,'ensembles') %>% filter(id == settings$benchmark$ensemble_id) %>% rename(bm_ensemble_id = id) %>% collect()
    wf <- tbl(bety, 'workflows') %>% filter(id == bm.ensemble$workflow_id) %>% collect()
  
    settings$rundir <- file.path(wf$folder, "run")
    settings$modeloutdir <- file.path(wf$folder, "out")
    settings$outdir <- wf$folder
    
  }
  
  # create benchmark entries
  bm.ids <- c()
  
  for(i in seq_along(settings$benchmark$variables)){
    
    bm <- tbl(bety, 'benchmarks') %>% 
      filter(input_id == settings$benchmark$input_id) %>%
      filter(variable_id == settings$benchmark$variables[[i]]) %>%
      filter(site_id == settings$run$site$id) %>% collect()
    
    
    # create benchmark record
    if(dim(bm)[1] == 0){
      cmd <- sprintf("INSERT INTO benchmarks (input_id, variable_id, site_id, user_id, created_at, updated_at) VALUES ( %s, %s, %s, %s, NOW(), NOW()) RETURNING * ", 
                     settings$benchmark$input_id, settings$benchmark$variables[[i]],
                     settings$run$site$id, settings$info$userid)
      bm <- db.query(cmd, bety$con)
    }else if(dim(bm)[1] >1){
      logger.error("Duplicate record entries in benchmarks")
    }
    bm.ids <- c(bm.ids, bm$id)
    
    # create benchmarks_benchmarks_reference_runs record
    bmBRR <- tbl(bety, 'benchmarks_benchmarks_reference_runs') %>% 
      filter(benchmark_id == bm$id) %>%
      filter(reference_run_id == settings$benchmark$reference_run_id)  %>% collect()
    
    
    if(dim(bmBRR)[1] == 0){
      cmd <- sprintf("INSERT INTO benchmarks_benchmarks_reference_runs (benchmark_id, reference_run_id) VALUES (%s, %s)",
                     bm$id, settings$benchmark$reference_run_id)
      db.query(cmd, bety$con)
    }else if(dim(bmBRR)[1] > 1){
      logger.error("Duplicate record entries in benchmarks_benchmarks_reference_runs")
    }
    
    # create benchmarks_metrics record
    for(j in seq_along(settings$benchmark$metrics)){
      bmmetric <- tbl(bety, 'benchmarks_metrics') %>% 
        filter(benchmark_id == bm$id) %>%
        filter(metric_id == settings$benchmark$metrics[[j]])  %>% collect()
      
      if(dim(bmmetric)[1] == 0){
        cmd <- sprintf("INSERT INTO benchmarks_metrics (benchmark_id, metric_id) VALUES (%s, %s)",
                       bm$id, settings$benchmark$metrics[[j]])
        db.query(cmd, bety$con)
      }else if(dim(bmmetric)[1] > 1){
        logger.error("Duplicate record entries in benchmarks_metrics")
      }
    }
  }
  invisible(return(settings))
} # create.benchmark
