##-------------------------------------------------------------------------------------------------#
##' For each benchmark entry in a (multi)settings object, get run settings using reference run id 
##' and add to the settings object
##'  
##' @name read_settings_BRR
##' @title Read settings from database using reference run id
##' @param settings settings or multisettings object
##' @export 
##' 
##' @author Betsy Cowdery 


read_settings_BRR <- function(settings){
  
  # dplyr functions
  tbl     <- dplyr::tbl
  filter  <- dplyr::filter
  rename  <- dplyr::rename
  collect <- dplyr::collect
  select  <- dplyr::select
  
  # Check database connection
  if (is.null(settings$database$bety)) {
    PEcAn.utils::logger.info("No databasse connection, can't get run information.")
    return (settings)
  }
  
  bety <- dplyr::src_postgres(dbname   = settings$database$bety$dbname,
                       host     = settings$database$bety$host,
                       user     = settings$database$bety$user,
                       password = settings$database$bety$password)
  
  BRR <- tbl(bety,"reference_runs") %>% filter(id == settings$benchmarking$reference_run_id)

  names(BRR$settings)
  
  BRR.settings <- BRR %>% select(settings) %>% collect() %>% unlist() %>%
    xmlToList(.,"pecan") 
  names(BRR.settings)
  
  settings <- BRR.settings %>% append(settings,.) %>% Settings()
  invisible(settings)
}

