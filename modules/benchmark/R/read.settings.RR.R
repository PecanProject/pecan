##-------------------------------------------------------------------------------------------------#
##' For each benchmark entry in a (multi)settings object, get run settings using reference run id 
##' and add to the settings object
##'  
##' @name read.settings.RR
##' @title Read settings from database using reference run id
##' @param settings settings or multisettings object
##' @export 
##' 
##' @author Betsy Cowdery 


read.settings.RR <- function(settings){
  
  # dplyr functions
  tbl     <- dplyr::tbl
  filter  <- dplyr::filter
  rename  <- dplyr::rename
  collect <- dplyr::collect
  select  <- dplyr::select
  
  # don't know how to check inputs
  if (is.null(settings$database$bety)) {
    logger.info("No databasse connection, can't get run information.")
    return (settings)
  }
  
  bety <- dplyr::src_postgres(dbname   = settings$database$bety$dbname,
                       host     = settings$database$bety$host,
                       user     = settings$database$bety$user,
                       password = settings$database$bety$password)
  
  if(!is.null(settings$benchmark$reference_run_id)){
    BRR <- tbl(bety,"reference_runs") %>% filter(id == settings$benchmark$reference_run_id)
  }else{
    if(!is.null(settings$benchmark$ensemble_id)){
      
      # check if there is already a BRR for ensemble.id, otherwise make one
      bm_ens <- tbl(bety,"benchmarks_ensembles") %>% rename(bm_ensemble_id = id) %>% 
        filter(ensemble_id == settings$benchmark$ensemble_id) %>% collect()
      
      if(length(bm_ens) == 0){
        # Get workflow id from ensemble id
        ens_wf <- tbl(bety, 'ensembles') %>% filter(id == settings$benchmark$ensemble_id) %>% 
          rename(ensemble_id = id) %>% 
          left_join(.,tbl(bety, "workflows") %>% rename(workflow_id = id), by="workflow_id") %>% collect()
        BRR <- create.BRR(ens_wf, con = bety$con, user_id = settings$info$userid)
      }else if(dim(bm_ens)[1] == 1){
        BRR <- tbl(bety,"reference_runs") %>% filter(id == bm_ens$reference_run_id) %>% 
          rename(reference_run_id = id) %>% collect()
      }else if(dim(bm_ens)[1] > 1){ # There shouldn't be more than one reference run per run
        logger.error("There is more than one reference run in the database for this ensemble id. Review for duplicates. ")}
      # add the ref_run id, remove the ensemble_id
      settings$benchmark$reference_run_id <- BRR$reference_run_id
      
      # won't need this ensemble_id anymore, because the new run will make a new ensemble_id
      if(as.logical(settings$benchmark$new_run)){settings$benchmark$ensemble_id <- NULL} 
      
    }else{logger.error("Cannot find or create BRR")}
  } 

  names(BRR$settings)
  
  BRR.settings <- BRR %>% select(settings) %>% collect() %>% unlist() %>%
    xmlToList(.,"pecan") 
  names(BRR.settings)
  
    
  settings <- BRR.settings %>% append(settings,.) %>% Settings()
  invisible(settings)
}

