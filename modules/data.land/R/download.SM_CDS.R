#' Download CDS soil moisture data for the SDA workflow.
#'
#' @param outfolder physical paths to where the unziped soil moisture files are downloaded.
#' @param time_points A vector contains each time point within the start and end date.
#' @param overwrite flag determine if we want to overwrite existing files when downloading.
#'
#' @return A vector containing file paths to the downloaded files.
#' @export
#' 
#' @examples
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
download.SM_CDS <- function(outfolder, time.points, overwrite = FALSE) {
  #load cdsapi from python environment.
  tryCatch({
    cdsapi <- reticulate::import("cdsapi")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `cdsapi` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user cdsapi`.",
      "The following error was thrown by `reticulate::import(\"cdsapi\")`: ",
      conditionMessage(e)
    )
  })
  #check if the token exists for the cdsapi.
  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")))
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key."
    )
  #grab the client object.
  tryCatch({
    cclient <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })
  #loop over each time point.
  file.names <- c()
  #setup progress bar.
  pb <- utils::txtProgressBar(min = 0, max = length(time.points), style = 3)
  for (i in seq_along(time.points)) {
    #name file.
    fname <- file.path(outfolder, paste('surface_soil_moisture', time.points[i], "nc", sep = "."))
    fname.zip <- gsub(".nc", ".zip", fname, fixed = T)
    #add new extracted file into vector.
    file.names <- c(file.names, fname)
    #if we have already downloaded this file.
    if (file.exists(fname) && !overwrite) {
      PEcAn.logger::logger.warn(glue::glue(
        "File `{fname}` already exists, and `overwrite` is FALSE. ",
        "Skipping to next variable."
      ))
      next
    }
    #prepare file through cds server.
    while ("try-error" %in% class(try(do_next <- cclient$retrieve(
      'satellite-soil-moisture',
      list(
        'variable'= 'surface_soil_moisture',
        'type_of_sensor'= 'active',
        'time_aggregation'= 'day_average',
        'year'= sprintf("%04d", lubridate::year(time.points[i])),
        'month'= sprintf("%02d", lubridate::month(time.points[i])),
        'day'= sprintf("%02d", lubridate::day(time.points[i])),
        'type_of_record'= 'cdr',
        'version'= 'v202212'
      ),
      'download.zip'
    )))) {
      Sys.sleep(10)
      PEcAn.logger::logger.info("Encounter error! Will try download in 10 seconds.")
    }
    #download file to local.
    utils::download.file(do_next$reply$location, destfile = fname.zip)
    #unzip file.
    unzipPath <- utils::unzip(zipfile = fname.zip, exdir = outfolder)
    #rename unziped file.
    base::file.rename(unzipPath, fname)
    #remove zip file.
    base::file.remove(fname.zip)
    #update progress bar
    pbi <- i
    utils::setTxtProgressBar(pb, pbi)
  }
  file.names
}