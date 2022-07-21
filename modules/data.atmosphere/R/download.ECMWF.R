#' Download ECMWF Open Data 15 day forecast data products
#'
#' @author Swarnalee Mazumder
#'

library(reticulate)
# py_install("ecmwf-opendata",pip = TRUE)

download.ECMWF <- function(outfolder, 
                           fchour, 
                           stream, 
                           type = "pf", 
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
      "Invalid data stream."
    ))
  }
  
  # Python script "download_ecmwf.py" to get latest forecast date and download forecast datasets
  source_python("download_ecmwf.py")
  
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
  
  fname <- paste(latest_filedate, time, step, stream, type, sep = "_")
  
  grib_fname <- paste0(fname, ".grib2")
  data_download <- ecmwfdownload(date, time, step, stream, type, all_parameters, grib_fname)
  
  # Python script "ecmwf_grib2nc.py" to convert grib2 to netCDF
  # Converting Grib2 file to ensemble wise NetCDF file
  source_python("ecmwf_grib2nc.py")
  nc_ecmwf <- grib2nc_ecmwf(grib_fname)
  
}

### code to reproduce
# date <- -1
# time <- 0
# step <- 360
# stream <- "enfo"
# type <- "pf"
# 
# all_parameters <-  c("tp", "10u", "10v", "q", "r", "sp")
# grib_fname <- "trial_ecmwfod.grib2"
# 
# source_python("download_ecmwf.py")
# data_download <- ecmwfdownload(date, time, step, stream, type, all_parameters, grib_fname)
# 
# source_python("ecmwf_grib2nc.py")
# nc_ecmwf <- grib2nc_ecmwf(grib_fname)
