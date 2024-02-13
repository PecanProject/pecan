##' Convert NARR files to CF files
##'
##' @param in.path directory in which to find NARR files
##' @param in.prefix pattern to match to find NARR files inside `in.path`
##' @param outfolder directory name to write CF outputs
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should ouput of function be extra verbose
##' @param ... other arguments, currently ignored
##'
##' @author Elizabeth Cowdery, Rob Kooper
##' @export
met2CF.NARR <- function(in.path, in.prefix, outfolder, start_date, end_date, 
                        overwrite = FALSE, verbose = FALSE, ...) {
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  vars <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate", "uwnd.10m", "vwnd.10m")
  svars <- c("pres", "dswrf", "dlwrf", "air", "shum", "prate", "uwnd", "vwnd")
  cfvars <- c("air_pressure", "surface_downwelling_shortwave_flux_in_air",
              "surface_downwelling_longwave_flux_in_air", 
              "air_temperature", "specific_humidity", "precipitation_flux",
              "eastward_wind", "northward_wind")
  
  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  years      <- start_year:end_year
  
  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows), 
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = in.prefix, 
                        stringsAsFactors = FALSE)
  
  for (y in years) {
    newfile <- file.path(outfolder, paste0("NARR.", y, ".nc"))
    
    # create array with results
    row <- y - start_year + 1
    results$file[row]       <- newfile
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(y, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(y, "-12-31 23:59:59")
    results$mimetype[row]   <- "application/x-netcdf"
    results$formatname[row] <- "CF (regional)"
    
    if (file.exists(newfile) && !overwrite) {
      PEcAn.logger::logger.debug("File '", newfile, "' already exists, skipping to next file.")
      next
    } else {
      PEcAn.logger::logger.info("Preparing file '", newfile, "'. ")
    }
    
    # use tempfile
    tmpfile <- file.path(outfolder, paste0("NARR.", y, ".tmp"))
    unlink(tmpfile)
    
    # keep track of variables to rename
    renamevars <- list("-v", "lat,latitude", "-v", "lon,longitude")
    for (i in seq_along(vars)) {
      file <- file.path(in.path, paste0(vars[i], ".", y, ".nc"))
      if (verbose) {
        print(paste(c("ncpdq", list("-A", "-U", "-4", "--no_tmp_fl", file, tmpfile)), collapse = " "))
      } 
      system2("ncpdq", list("-A", "-U", "-4", "--no_tmp_fl", file, tmpfile))
      renamevars <- c(renamevars, c("-v", paste0(svars[i], ",", cfvars[i])))
    }
    
    # rename all variables
    if (verbose) {
      print(paste(c("ncrename", c(renamevars, tmpfile)), collapse = " "))
    }
    system2("ncrename", c(renamevars, tmpfile))
    
    # finally rename file
    file.rename(tmpfile, newfile)
  }
  
  return(invisible(results))
} # met2CF.NARR
