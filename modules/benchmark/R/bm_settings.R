## Functions for bulding benchmarking settings

##------------------------------------------------------------------------------------------------##
##' For each benchmark entry in a (multi)settings object, get run settings using reference run id 
##' and add to the settings object
##'  
##' @name read_settings_BRR
##' @title Read settings from database using reference run id
##' @param settings settings or multisettings object
##' @importFrom dplyr tbl filter rename collect select
##' @export 
##' @author Betsy Cowdery 

read_settings_BRR <- function(settings){
  
  # Check database connection
  if (is.null(settings$database$bety)) {
    PEcAn.logger::logger.info("No databasse connection, can't get run information.")
    return (settings)
  }
  
  bety <- dplyr::src_postgres(dbname   = settings$database$bety$dbname,
                              host     = settings$database$bety$host,
                              user     = settings$database$bety$user,
                              password = settings$database$bety$password)
  BRR <- tbl(bety,"reference_runs") %>% 
    filter(id == settings$benchmarking$reference_run_id) %>% 
    collect()
  
  BRR.settings <- BRR %>% pull(settings) %>% unlist() %>%
    xmlToList(.,"pecan") 
  
  logger.debug(names(BRR.settings))
  
  settings <- BRR.settings %>% append(settings,.) %>% PEcAn.settings::Settings()
  invisible(settings)
}

##------------------------------------------------------------------------------------------------##
##' @name clean_settings_BRR
##' @title Cleans PEcAn settings file and prepares the settings to be saved in a reference run record in BETY
##' @param inputfile the PEcAn settings file to be used.
##' @export
##' @author Betsy Cowdery

clean_settings_BRR <- function(inputfile){
  clean <- PEcAn.settings::clean.settings(inputfile,write=FALSE)
  if (is.MultiSettings(clean)) {
    logger.error("Cannot run clean settings for a mutlisettings object") # For now
  }
  
  # Remove database & host information
  clean$database <- NULL 
  clean$host <- NULL
  clean$info <- NULL
  clean$outdir <- NULL
  
  # Remove sections that should not be in benchmark refence runs
  clean$meta.analysis <- NULL
  clean$ensemble <- NULL
  clean$assim.batch <- NULL
  clean$state.data.assimilation <- NULL
  return(clean)
}


##------------------------------------------------------------------------------------------------##
##' @name add_workflow_info
##' @title Add workflow specific info to settings list for benchmarking
##' @param settings settings or multisettings object
##' @param bety connection to the database
##' @export 
##' @author Betsy Cowdery 

add_workflow_info <- function(settings, bety){
  if (is.MultiSettings(settings)) {
    return(papply(settings, add_workflow_id))
  }
  if(!as.logical(settings$benchmarking$new_run)){
    settings$workflow$id <- tbl(bety,"ensembles") %>% 
      filter(id == settings$benchmarking$ensemble_id) %>% 
      dplyr::select(workflow_id) %>% collect %>% .[[1]]
    wf <- tbl(bety, 'workflows') %>% filter(id == settings$workflow$id) %>% collect()
    settings$rundir <- file.path(wf$folder, "run")
    settings$modeloutdir <- file.path(wf$folder, "out")
    settings$outdir <- wf$folder
  }
  return(settings)
}

##------------------------------------------------------------------------------------------------##
##' @name bm_settings2pecan_settings
##' @title Move benchmarking settings back in to original pecan settings object
##' @param bm.settings settings or multisettings object
##' @export 
##' @author Betsy Cowdery 

bm_settings2pecan_settings <- function(bm.settings){
  if (is.MultiSettings(bm.settings)) {
    return(papply(bm.settings, bm_settings2pecan_settings))
  }
  out <- bm.settings["reference_run_id"]
  for(i in grep("benchmark", names(bm.settings))){
    print(bm.settings[i]$benchmark$benchmark_id)
    out <- append(out, list(benchmark_id = bm.settings[i]$benchmark$benchmark_id))
  }
  return(out)
} 