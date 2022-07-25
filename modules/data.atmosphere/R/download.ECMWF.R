#' Downloads ECMWF Open Data 15 day forecast data products 
#' https://confluence.ecmwf.int/display/DAC/ECMWF+open+data%3A+real-time+forecasts
#' 
#' https://github.com/ecmwf/ecmwf-opendata
#' Under the hood, this function uses the Python `ecmwf-opendata` module,
#' which can be installed via `pip` (`pip install --user ecmwf-opendata`). The
#' module is accessed via the `reticulate` package.
#'
#' @param outfolder location on disk where outputs will be stored
#' @param lat.in,lon.in site coordinates, decimal degrees (numeric)
#' @param fchour reference time of the forecasts. Values are 00, 12 for 15 day forecast.
#' @param stream  forecasting system that produces the data. Current value is `"enfo"`
#' @param type the type of forecast data. Current values: `cf` (controlled forecast), `pf` (perturbed forecast)
#' @param parameters character vector of product types, or `"all"`.
#' @param reticulate_python Path to Python binary for `reticulate`
#'   (passed to [reticulate::use_python()]). If `NULL` (default), use
#'   the system default.
#' @param overwrite Logical. If `FALSE` (default), skip any files with
#'   the same target name (i.e. same variable) that already exist in
#'   `outfolder`. If `TRUE`, silently overwrite existing files.
#' @return information about the output file
#' @export
#' @examples
#' 
#' \dontrun{
#' files <- download.ECMWF(
#'   "ECMWF_output",
#'   lat.in = 0,
#'   lon.in = 180,
#'   fchour = 00,
#'   stream = "enfo"
#'   type = c("cf", "pf"), 
#'   parameters = "all", 
#' )
#' }
#' @author Swarnalee Mazumder
#'

download.ECMWF <- function(outfolder, 
                           lat.in,
                           lon.in,
                           fchour, 
                           stream = "enfo", 
                           type = c("cf", "pf"), 
                           parameters = "all", 
                           overwrite = FALSE, 
                           reticulate_python = NULL,...)
  
{
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  
  tryCatch({
    ecmwfod <- reticulate::import("ecmwf.opendata")
  }, error = function(e) {
    PEcAn.logger::logger.severe(
      "Failed to load `ecmwfod` Python library. ",
      "Please make sure it is installed to a location accessible to `reticulate`.",
      "You should be able to install it with the following command: ",
      "`pip install --user ecmwf-opendata`.",
      "The following error was thrown by `reticulate::import(\"ecmwf.opendata\")`: ",
      conditionMessage(e)
    )
  })
  
  hour <- fchour
  
  all_hours <- c(00, 12)
  
  if (any(!hour %in% all_hours)) {
    bad_hours <- setdiff(hour, all_hours)
    PEcAn.logger::logger.severe(sprintf(
      "Invalid forecast hours  %s. 15 day forecast hours must be one of the following: %s",
      paste0("`", bad_hours, "`", collapse = ", "),
      paste0("`", all_hours, "`", collapse = ", ")
    ))
  }
  
  if (fchour == 00){
    time <- 0
  } else if (fchour == 12){
    time <- 12
  }
  
  all_parameters <- c("2t", "tp", "10u", "10v", "q", "r", "sp")
  
  if (tolower(parameters) == "all") {
    parameter_types <- all_parameters
  }
  
  if (any(!parameter_types %in% all_parameters)) {
    bad_parameters <- setdiff(parameter_types, all_parameters)
    PEcAn.logger::logger.severe(sprintf(
      "Invalid parameter types %s. Products must be one of the following: %s",
      paste0("`", bad_parameters, "`", collapse = ", "),
      paste0("`", all_parameters, "`", collapse = ", ")
    ))
  }
  
  if (stream == "enfo"){
    time <- time
    step <- 360
    type <- type
  }  else{ 
    PEcAn.logger::logger.severe(sprintf(
      "Invalid data stream. Currently, data stream `enfo` is supported."
    ))
  }
  
  # Python script "download_ecmwf.py" to get latest forecast date and download forecast datasets
  script.path = file.path(system.file("download_ecmwf_opendata.py", package = "PEcAn.data.atmosphere"))
  reticulate::source_python(script.path)
  
  date_latestdata <- ecmwflatest(time, step, stream, type, all_parameters)
  latest_filedate <- strtoi(gsub("-", "", substr(as.character.Date(date_latestdata), 1, 10)))
  
  current_date <- strtoi(gsub("-", "", Sys.Date()))
  
  date <- if (latest_filedate == current_date){
    # today
    0
  } else if ((latest_filedate - current_date) == -1){
    # yesterday
    -1
  } else if ((latest_filedate - current_date) == -2){
    # the day before yesterday
    -2
  } else { PEcAn.logger::logger.severe(sprintf(
    "Invalid file download date."
  ))
  }
  
  # Removes any already existing files with base name same as `latest_filedate``
  if ((length(list.files(path = outfolder, pattern = as.character(latest_filedate)) ) > 0) == TRUE){
    file.remove(list.files(outfolder, pattern = as.character(latest_filedate)))
  }
  
  fname <- paste(latest_filedate, time, step, stream, type, sep = "_")
  
  in_filename <- paste0(fname, ".grib2")
  
  data_download <- ecmwfdownload(date, time, step, stream, type, all_parameters, in_filename)
  
  # Python script "ecmwf_grib2nc.py" to convert grib2 to netCDF
  # Converting Grib2 file to ensemble wise NetCDF file
  script.path = file.path(system.file("ecmwf_grib2nc.py", package = "PEcAn.data.atmosphere"))
  reticulate::source_python(script.path)
  
  nc_ecmwf <- grib2nc_ecmwf(in_filename, outfolder, out_filename)
  
  
  rows    <- 1
  results <- data.frame(
    file = character(rows),
    host = character(rows),
    mimetype = character(rows),
    formatname = character(rows),
    startdate = character(rows),
    enddate = character(rows),
    dbfile.name = "ECMWF",
    stringsAsFactors = FALSE
  )
  
  firstdate_st <- latest_filedate
  lastdate_st <- ymd(firstdate_st) + days(15)
  
  results$file[rows]       <-
    file.path(outfolder, out_file_names)
  results$host[rows]       <- PEcAn.remote::fqdn()
  results$startdate[rows]  <- firstdate_st
  results$enddate[rows]    <- lastdate_st
  results$mimetype[rows]   <- "application/x-netcd"
  results$formatname[rows] <- "CF Meteorology"
  
  return(results)
  
}


######### code to reproduce 
# library(reticulate)
# date <- -1
# time <- 0
# step <- 360
# stream <- "enfo"
# type <- c("cf","pf")
# 
# all_parameters <-  c("2t", "tp", "10u", "10v", "q", "r", "sp")
# in_filename <- "trial_ecmwfod.grib2"
# 
# script.path = file.path(system.file("download_ecmwf_opendata.py", package = "PEcAn.data.atmosphere"))
# reticulate::source_python(script.path)
# data_download <- ecmwfdownload(date, time, step, stream, type, all_parameters, in_filename)
# 
# out_filename <- "trial_ecmwfod"
#
# script.path = file.path(system.file("ecmwf_grib2nc.py", package = "PEcAn.data.atmosphere"))
# reticulate::source_python(script.path)
# nc_ecmwf <- grib2nc_ecmwf(in_filename, outfolder, out_filename, lat_in, lon_in)