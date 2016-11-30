##' Creates records for benchmarks, benchmarks_benchmarks_reference_runs, benchmarks_metrics
##'
##' @title Benchmark Definition: Retrieve or Create Bety Benchmarking Records
##' @param bm.settings settings list
##' @return updated settings list
##' @author Betsy Cowdery
##' @export 
##' @importFrom dplyr tbl
##' @importFrom dplyr filter
##' @importFrom dplyr rename
##' @importFrom dplyr collect
##' @importFrom dplyr select
define_benchmark <- function(bm.settings, bety){
  
  if (is.MultiSettings(bm.settings)) {
    return(papply(bm.settings, function(x) define_benchmark(x, bety)))
  }
  
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
        BRR <- create_BRR(ens_wf, con = bety$con, user_id = bm.settings$info$userid)
      }else if(dim(bm_ens)[1] == 1){
        BRR <- tbl(bety,"reference_runs") %>% filter(id == bm_ens$reference_run_id) %>% 
          rename(reference_run_id = id) %>% collect()
      }else if(dim(bm_ens)[1] > 1){ # There shouldn't be more than one reference run per run
        PEcAn.utils::logger.error("There is more than one reference run in the database for this ensemble id. Review for duplicates. ")}
      # add the ref_run id, remove the ensemble_id
      bm.settings$reference_run_id <- BRR$reference_run_id
      # bm.settings$ensemble_id <- NULL
      
    }else{logger.error("Cannot find or create_BRR")}
  } 
  
  
  # Retrieve/create benchmark entries
  
  for(i in which(names(bm.settings) == "benchmark")){
    
    benchmark <- bm.settings[[i]]
    
    bm <- tbl(bety, 'benchmarks') %>% 
      filter(input_id == benchmark$input_id) %>%
      filter(variable_id == benchmark$variable_id) %>%
      filter(site_id == benchmark$site_id) %>% collect()
    
    # Retrieve/create benchmark record
    if(dim(bm)[1] == 0){
      cmd <- sprintf(paste0("INSERT INTO benchmarks (input_id, variable_id, site_id, user_id)",
                            "VALUES ( %s, %s, %s, %s) RETURNING * ;"), 
                     benchmark$input_id, benchmark$variable_id,
                     benchmark$site_id, bm.settings$info$userid)
      bm <- db.query(cmd, bety$con)
    }else if(dim(bm)[1] >1){
      PEcAn.utils::logger.error("Duplicate record entries in benchmarks")
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
      PEcAn.utils::logger.error("Duplicate record entries in benchmarks_benchmarks_reference_runs")
    }
    
    # Retrieve/create benchmarks_metrics record
    for(j in seq_along(benchmark$metrics)){
      bmmetric <- tbl(bety, 'benchmarks_metrics') %>% 
        filter(benchmark_id == bm$id) %>%
        filter(metric_id == benchmark$metrics[[j]])  %>% collect()
      
      if(dim(bmmetric)[1] == 0){
        cmd <- sprintf(paste0("INSERT INTO benchmarks_metrics (benchmark_id, metric_id) VALUES (%s, %s)"),
                       bm$id, benchmark$metrics[[j]])
        db.query(cmd, bety$con)
      }else if(dim(bmmetric)[1] > 1){
        PEcAn.utils::logger.error("Duplicate record entries in benchmarks_metrics")
      }
    }
    
    benchmark$benchmark_id <- bm$id
    bm.settings[[i]] <- benchmark
    
  } # End loop over benchmark
  
  invisible(return(bm.settings))
} # create.benchmark
