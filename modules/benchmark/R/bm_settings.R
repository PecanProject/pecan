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
    PEcAn.logger::logger.info("No database settings, can't get run information.")
    return (settings)
  }

  con <- PEcAn.DB::db.open(settings$database$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)

  BRR <- tbl(con,"reference_runs") %>%
    filter(.data$id == settings$benchmarking$reference_run_id) %>%
    collect()

  BRR.settings <- BRR %>% dplyr::pull(settings) %>% unlist() %>%
    XML::xmlToList("pecan")

  PEcAn.logger::logger.debug(names(BRR.settings))

  settings <- append(settings, BRR.settings) %>% PEcAn.settings::Settings()
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
  if (PEcAn.settings::is.MultiSettings(clean)) {
    PEcAn.logger::logger.error("Cannot run clean settings for a mutlisettings object") # For now
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

  # Remove machine specific information, leaving only database ids
  # This probably needs to be more generalized

  clean$model$binary <- NULL
  # Remove all file paths
  for(input in names(clean$run$inputs)){
    if("path" %in% names(clean$run$inputs[[input]])){
      clean$run$inputs[[input]][["path"]] <- NULL
    }
  }


  return(clean)
}


##------------------------------------------------------------------------------------------------##
##' @name add_workflow_info
##' @title Add workflow specific info to settings list for benchmarking
##' @param settings settings or multisettings object
##' @param bety connection to the database
##' @importFrom rlang .data
##' @export
##' @author Betsy Cowdery

add_workflow_info <- function(settings, bety){
  if (PEcAn.settings::is.MultiSettings(settings)) {
    return(PEcAn.settings::papply(settings, add_workflow_id))
  }
  if(!as.logical(settings$benchmarking$new_run)){
    settings$workflow$id <- tbl(bety,"ensembles") %>%
      dplyr::filter(.data$id == settings$benchmarking$ensemble_id) %>%
      dplyr::select("workflow_id") %>% dplyr::collect %>% .[[1]]
    wf <- tbl(bety, 'workflows') %>% dplyr::filter(.data$id == settings$workflow$id) %>% collect()
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
  if (PEcAn.settings::is.MultiSettings(bm.settings)) {
    return(PEcAn.settings::papply(bm.settings, bm_settings2pecan_settings))
  }
  out <- bm.settings["reference_run_id"]
  for(i in grep("benchmark", names(bm.settings))){
    print(bm.settings[i]$benchmark$benchmark_id)
    out <- append(out, list(benchmark_id = bm.settings[i]$benchmark$benchmark_id))
  }
  return(out)
}

##------------------------------------------------------------------------------------------------##
##' @name check_BRR
##' @title Check whether a run has been registered as a reference run in BETY
##' @param settings_xml cleaned settings to be compared with BRR in the database
##' @param con database connection
##' @importFrom dplyr tbl filter collect
##' @export
##' @author Betsy Cowdery

check_BRR <- function(settings_xml, con){
  # This is NOT a good way to find matching reference run records
  # Other options include comparing lists (slow)
  # more spohisticated PSQL queries
  # changing the settings field to jsonb
  ref_run <- tbl(con, "reference_runs") %>% filter(.data$settings == settings_xml) %>% collect
  return(ref_run)
}
