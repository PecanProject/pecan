#' @description
#' This function helps to download the yearly ERA5 data based on the prescribed features using the CDS API.
#' @title ERA5_cds_annual_download
#' 
#' @param outfolder Character: physical path where the ERA5 data are stored.
#' @param start_date character: the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param end_date character: the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
#' @param extent numeric: a vector of numbers contains the bounding box (formatted as xmin, xmax, ymin, ymax) to be downloaded.
#' @param variables character: a vector contains variables to be downloaded (e.g., c("2m_temperature","surface_pressure")).
#' @param auto.create.key Boolean: decide if we want to generate the CDS RC file if it doesn't exist, the default is TRUE.
#' @param timeout numeric: the maximum time (in seconds) allowed to download the data. The default is 36000 seconds.
#'
#' @return A vector containing file paths to the downloaded files.
#' @export
#' 
#' @importFrom purrr %>%
#' @author Dongchen Zhang
download.ERA5_cds <- function(outfolder, start_date, end_date, extent, variables, auto.create.key = T, timeout = 36000) {
  # check shell environments.
  if ("try-error" %in% class(try(system("grib_to_netcdf"), silent = T))) {
    PEcAn.logger::logger.info("The grib_to_netcdf function is not detected in shell command.")
    return(NA)
  }
  # setup timeout for download.
  options(timeout=timeout)
  # convert arguments to CDS API specific arguments.
  years <- sort(unique(lubridate::year(seq(lubridate::date(start_date), lubridate::date(end_date), "1 year"))))
  months <- sort(unique(lubridate::month(seq(lubridate::date(start_date), lubridate::date(end_date), "1 month")))) %>% 
    purrr::map(function(d)sprintf("%02d", d))
  days <- sort(unique(lubridate::day(seq(lubridate::date(start_date), lubridate::date(end_date), "1 day")))) %>% 
    purrr::map(function(d)sprintf("%02d", d))
  times  <- list('00:00','03:00','06:00',
                 '09:00','12:00','15:00',
                 '18:00','21:00')
  area <- paste(c(extent[4], extent[1], extent[3], extent[2]), collapse = "/")
  variables <- as.list(variables)
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
  #define function for building credential file.
  #maybe as a helper function.
  getnetrc <- function (dl_dir) {
    netrc <- file.path(dl_dir, ".cdsapirc")
    if (file.exists(netrc) == FALSE ||
        any(grepl("https://cds.climate.copernicus.eu/api/v2",
                  readLines(netrc))) == FALSE) {
      netrc_conn <- file(netrc)
      writeLines(c(
        sprintf(
          "url: %s",
          getPass::getPass(msg = "Enter URL from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        ),
        sprintf(
          "key: %s",
          getPass::getPass(msg = "Enter KEY from the following link \n (https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key):")
        )
      ),
      netrc_conn)
      close(netrc_conn)
      message(
        "A netrc file with your CDS Login credentials was stored in the output directory "
      )
    }
    return(netrc)
  }
  #check if the token exists for the cdsapi.
  if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & auto.create.key) {
    if ("try-error" %in% class(try(find.package("getPass")))) {
      PEcAn.logger::logger.info("The getPass pacakge is not installed for creating the API key.")
      return(NA)
    } else {
      getnetrc(Sys.getenv("HOME"))
    }
  } else if (!file.exists(file.path(Sys.getenv("HOME"), ".cdsapirc")) & !auto.create.key) {
    PEcAn.logger::logger.severe(
      "Please create a `${HOME}/.cdsapirc` file as described here:",
      "https://cds.climate.copernicus.eu/api-how-to#install-the-cds-api-key ."
    )
  }
  #grab the client object.
  tryCatch({
    c <- cdsapi$Client()
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "The following error was thrown by `cdsapi$Client()`: ",
      conditionMessage(e)
    )
  })
  # loop over years.
  nc.paths <- c()
  for (y in years) {
    fname <- file.path(outfolder, paste0("ERA5_", y, ".grib"))
    # start retrieving data.
    # you need to have an account for downloaing the files
    # Read the documantion for how to setup your account and settings before trying this
    # https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#HowtodownloadERA5-3-DownloadERA5datathroughtheCDSAPI
    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = 'ensemble_members',
        'data_format' = 'grib',
        "download_format" = "unarchived",
        'day' = days,
        'time' = times,
        'month' = months,
        'year' = as.character(y),
        "area" = area,
        'variable' = variables
      ),
      fname
    )
    # convert grib to nc file.
    nc.path <- gsub(".grib", ".nc", fname, fixed = T)
    cmd <- paste("grib_to_netcdf", fname, "-o", nc.path)
    out <- system(cmd, intern = F, ignore.stdout = T, ignore.stderr = T)
    # store the path.
    nc.paths <- c(nc.paths, nc.path)
    # remove previous grib file.
    unlink(fname)
  }
  # construct results to meet the requirements of pecan.met workflow.
  results <- vector("list", length = length(years))
  for (i in seq_along(results)) {
    results[[i]] <- list(file = nc.paths[i],
                         host = PEcAn.remote::fqdn(),
                         startdate = paste0(paste(years[i], months[1], days[1], sep = "-"), " ", times[1], ":00"),
                         enddate = paste0(paste(years[i], months[length(months)], days[length(days)], sep = "-"), " ", times[length(times)], ":00"),
                         mimetype = "application/x-netcdf",
                         formatname = "ERA5_year.nc")
  }
  return(results)
}