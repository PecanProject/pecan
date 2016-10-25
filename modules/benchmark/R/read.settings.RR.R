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
  
  settings <- tbl(bety,"reference_runs") %>% 
    filter(id == settings$benchmark$reference_run_id) %>% 
      dplyr::select(settings) %>% collect() %>% unlist() %>%
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
