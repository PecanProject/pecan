#' Downloads ECMWF Open Data 15 day forecast data products 
#' https://confluence.ecmwf.int/display/DAC/ECMWF+open+data%3A+real-time+forecasts
#' 
#' https://github.com/ecmwf/ecmwf-opendata
#' Under the hood, this function uses the Python `ecmwf-opendata` module.
#' The module and dependencies can be accessed via the `reticulate` package.
#' 
#' `reticulate::conda_install(c("scipy", "xarray", "eccodes", "cfgrib", "ecmwf-opendata", "dask", "netCDF4"), envname = "r-reticulate", pip = TRUE)`
#' `reticulate::use_condaenv("r-reticulate")`
#'  
#' 
#' @param outfolder location on disk where outputs will be stored
#' 
#' @param lat.in,lon.in site coordinates, decimal degrees (numeric)
#' 
#' @param fchour reference time of the forecasts. Values are 00, 12 for 15 day forecast.
#' `fchour` is not passed via upstream functions and we are using 00 by default when the function is called from within the pecan workflow.
#' 
#' @param type the type of forecast data. Current values: `cf` (controlled forecast), `pf` (perturbed forecast)
#' 
#' @param parameters character vector of product types, or `"all"`.
#' 
#' @param reticulate_python Path to Python binary for `reticulate`
#'   (passed to [reticulate::use_python()]). If `NULL` (default), use
#'   the system default.
#' 
#' @param overwrite Logical. If `FALSE` (default), skip any files with
#'   the same target name (i.e. same variable) that already exist in
#'   `outfolder`. If `TRUE`, silently overwrite existing files.
#' 
#' Forecast hour is reference time of the forecasts. Values are 00, 12 for 15 day forecast. 
# `fchour` is not passed via upstream functions currently as we are using 00 by default (time <- 0) for function calls from within the pecan workflow
#' 
#' @return information about the output file
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' files <- download.ECMWF(
#'   "ECMWF_output",
#'   lat.in = 0,
#'   lon.in = 180,
#'   fchour = 00,
#'   product = "NULL,
#'   type = c("cf", "pf"), 
#'   parameters = "all", 
#' )
#' }
#' @author Swarnalee Mazumder
#'

download.ECMWF <- function(outfolder, 
                           lat.in,
                           lon.in,
                           fchour = 00,
                           product = NULL, 
                           type = c("cf", "pf"), 
                           parameters = "all", 
                           overwrite = FALSE, 
                           reticulate_python = NULL,...)
  
{
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  ############## Forecast Hour ################
  
  # 15 day forecast hours can be 00h or 12h. Here forecast hour is assigned to 00
  # hence the time variable required by the download function becomes 0
  
  # all_hours <- c(00, 12)
  # if (any(!hour %in% all_hours)) {
  #   bad_hours <- setdiff(hour, all_hours)
  #   PEcAn.logger::logger.severe(sprintf(
  #     "Invalid forecast hours  %s. 15 day forecast hours must be one of the following: %s",
  #     paste0("`", bad_hours, "`", collapse = ", "),
  #     paste0("`", all_hours, "`", collapse = ", ")
  #   ))
  # }
  
  # for forecast hour = 00 
  if (fchour == 00){
    time <- 0
  } else {
    PEcAn.logger::logger.severe(sprintf(
      "Invalid forecast hour. Currently, forecast hour 00 is the default."
    ))
  }
  
  ############## Product (Forecasting system) ################
  
  # For product = NULL,the stream is set to "enfo"
  # stream is forecasting system that produces the data. Current value is `"enfo"`
  
  if(is.null(product)){
    stream <- "enfo"
  } else{
    PEcAn.logger::logger.severe(sprintf(
      "Invalid data stream. Currently, data stream `enfo` is supported."
    ))
  }
  
  if (stream == "enfo"){
    time <- time
    step_15day <- 360
    type <- type # type of forecast here c("cf", "pf")
  } else{
    PEcAn.logger::logger.severe(sprintf(
      "Invalid data stream. Currently, data stream `enfo` is supported."
    ))
  }
  
  
  ############## Product Types ################
  
  # Currently supported parameters are
  # "2t" - 2 metre temperature 
  # "tp" - total precipitation
  # "10u" - 10 metre U wind component
  # "10v" - 10 metre V wind component
  # "sp" - Surface Pressure
  
  
  all_parameters <- c("2t", "tp", "10u", "10v", "sp")
  
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
  
  
  # Python script "download_ecmwf.py" to get latest forecast date and download forecast datasets
  script.path = file.path(system.file("ECMWF/download_ecmwf.py", package = "PEcAn.data.atmosphere"))
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
  
  # Skip download if there is any already existing file with base name same as `latest_filedate``
  if ((length(list.files(path = outfolder, pattern = as.character(latest_filedate)) ) > 0) == TRUE){
    PEcAn.logger::logger.severe(sprintf(
      "Files already exist for %s",
      latest_filedate
    ))
  } else {
    
    in_fname <- paste(latest_filedate, time, stream, sep = "_")
    
    data_download <- ecmwfdownload(date, time, stream, type, all_parameters, in_fname)
    
    rows    <- 85 # 48 files at 3h timestep and 36 files at 6h timestep
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
    
    out_3h <- list()
    for (h3 in seq(0, 141, 3)){
      out_3h <- append(out_3h, paste0(in_fname,"_",h3,".grib2"))
    }
    
    out_6h <- list()
    for (h6 in seq(144, 360, 6)){
      out_6h <- append(out_6h, paste0(in_fname,"_",h6,".grib2"))
    }
    
    for (row in 1:48){
      results$file[row]       <-
        file.path(outfolder, out_3h[[row]])
      results$host[row]       <- PEcAn.remote::fqdn()
      results$startdate[row]  <- "start_date"
      results$enddate[row]    <- "end_date"
      results$mimetype[row]   <- "application/x-netcd"
      results$formatname[row] <- "CF Meteorology"
    }
    
    for (row in 1:37){
      results$file[row+48]       <-
        file.path(outfolder, out_6h[[row]])
      results$host[row+48]       <- PEcAn.remote::fqdn()
      results$startdate[row+48]  <- "start_date"
      results$enddate[row+48]    <- "end_date"
      results$mimetype[row+48]   <- "application/x-netcd"
      results$formatname[row+48] <- "CF Meteorology"
    }
    return(results)
  }
  
}

######### code to reproduce 
# library(reticulate)
# reticulate::conda_install(c("scipy", "xarray", "eccodes", "cfgrib", "ecmwf-opendata", "dask", "netCDF4"), envname = "r-reticulate", pip = TRUE)
# reticulate::use_condaenv("r-reticulate")
# 
# date <- -1
# time <- 0
# stream <- "enfo"
# type <- c("cf", "pf")
# step_15day <- 360
# 
# all_parameters <-  c("10u", "10v", "2t", "sp", "tp")
# fname <- "15day_ecmwfxxx"
# 
# reticulate::source_python("downloadecmwf.py")
# 
# latest <- ecmwflatest(time, step_15day, stream, type, all_parameters)
# 
# data_download <- ecmwfdownload(date= date, time= time, stream= stream, type= type, params= all_parameters, filename= fname)