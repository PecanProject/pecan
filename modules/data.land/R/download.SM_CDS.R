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
#' @importFrom dplyr %>%
download.SM_CDS <- function(outfolder, time.points, overwrite = FALSE) {
  ###################################Introduction on how to play with the CDS python API##########################################
  #to correctly build the python environment with the cdsapi installed, you need to follow those steps.
  #1. Install miniconda.
  # create a directory to install minicaonda `mkdir -p ~/miniconda3`
  #2. Download latest miniconda version.
  # `wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda3/miniconda.sh`
  #3. run the install script.
  # `bash ~/miniconda3/miniconda.sh -b -u -p ~/miniconda3`
  #4. delete the intall script.
  # `rm -rf ~/miniconda3/miniconda.sh`
  #5. add a conda initialize to your bash
  # `~/miniconda3/bin/conda init bash`
  #6. Verify the installaton, you need to restart your session first.
  # `conda list`
  #7. Create Python environment.
  # `conda update conda`
  # `conda create -n myenv python=3.9 --yes`
  #8. Activate your python env.
  # `conda activate myenv`
  #9. Install the cdsapi package.
  # `pip install cdsapi`
  # in the meantime, you might encounter several issues saying XXXX dependency is not available.
  # to solve this issue, you just need to install those dependencies before hand.
  #10. Create CDS account.
  # go to `https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key` website.
  # create an account.
  #11. Create CDS personel token.
  # go to `https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key` website.
  # create local file using `touch $HOME/.cdsapirc`
  #12 open token file and add your token to it.
  # `vim $HOME/.cdsapirc`, copy and paste text to the target file, type `:wq`.
  ################################################################################################################################
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