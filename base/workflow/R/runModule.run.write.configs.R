#' Generate model-specific run configuration files for one or more PEcAn runs
#'
#' @param settings a PEcAn Settings or MultiSettings object
#' @param overwrite logical: Replace config files if they already exist?
#' @return A modified settings object, invisibly
#' @export
runModule.run.write.configs <- function(settings, overwrite = TRUE) {

  if (PEcAn.settings::is.MultiSettings(settings)) {
    if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
      PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
      unlink(file.path(settings$rundir, "runs.txt"))
    }
    return(PEcAn.settings::papply(settings, runModule.run.write.configs, overwrite = FALSE))
  } else if (PEcAn.settings::is.Settings(settings)) {
    write <- settings$database$bety$write
    # double check making sure we have method for parameter sampling
    if (is.null(settings$ensemble$samplingspace$parameters$method)) settings$ensemble$samplingspace$parameters$method <- "uniform"
    ens.sample.method <-  settings$ensemble$samplingspace$parameters$method
    
   
    #check to see if there are posterior.files tags under pft
    posterior.files <-settings$pfts %>%
      purrr::map(purrr::possibly('posterior.files', NA_character_)) %>%
      purrr::modify_depth(1, function(x) {
        ifelse(is.null(x), NA_character_, x)
      }) %>%
      unlist()
    
    return(PEcAn.workflow::run.write.configs(settings, write, ens.sample.method, posterior.files = posterior.files, overwrite = overwrite))
  } else {
    stop("runModule.run.write.configs only works with Settings or MultiSettings")
  }
}
