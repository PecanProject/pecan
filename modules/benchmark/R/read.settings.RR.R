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
  library(PEcAn.DB)
  library(dplyr)
  
  # don't know how to check inputs
  if (is.null(settings$database$bety)) {
    logger.info("No databasse connection, can't get run information.")
    return (settings)
  }
  
  bety <- src_postgres(dbname   = settings$database$bety$dbname,
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
        BRR <- create.BRR(ens_wf, con = bety$con)
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

  settings <- BRR %>% dplyr::select(settings) %>% collect() %>% unlist() %>%
    xmlToList(.,"pecan") %>% append(settings,.) %>% Settings()
  invisible(settings)
}



# OLD Version where the function takes in a vector of id's

# read.settings.RR <- function(ids,bety){
#   settings.list <- list()
#   for(i in seq_along(ids)){
#     settings.list[[i]] <- tbl(bety,"reference_runs") %>% filter(id == ids[i]) %>% 
#       dplyr::select(settings) %>% collect() %>% unlist() %>%
#       xmlToList(.,"pecan") %>% Settings()
#     settings.list[[i]]$info <- list(reference_run_id = ids[i])
#   }
#   settings.multi <- MultiSettings(settings.list)
#   # This may not be the best way to add database information back into the xml, but it's a start
#   settings.multi$database[[bety$info$dbname]] <- bety$info[c("host","user","dbname")]
#   # Should I be getting the settings straight from config.php? That's where these settings are taken from anyway.
#   return(settings.multi)
# }
