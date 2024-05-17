##' Download NARR files
##'
##' @param outfolder location where output is stored
##' @param overwrite Overwrite existing files?  Default=FALSE
##' @param verbose Turn on verbose output? Default=FALSE
##' @param method Method of file retrieval. Can set this using the options(download.ftp.method=[method]) in your Rprofile.
##' @param start_date desired start date YYYY-MM-DD
##' @param end_date desired end date YYYY-MM-DD
##' @param ... other inputs
##' example options(download.ftp.method="ncftpget")
##' @importFrom dplyr %>%
##' 
##' @examples
##' \dontrun{
##' download.NARR("~/",'2000/01/01','2000/01/02', overwrite = TRUE, verbose = TRUE)
##' }
##' 
##' @export
##'
##' @author Betsy Cowdery, Shawn Serbin
download.NARR <- function(outfolder, start_date, end_date, overwrite = FALSE, verbose = FALSE, method, ...) {
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  NARR_start <- 1979
  if (start_year < NARR_start) {
    PEcAn.logger::logger.severe(sprintf('Input year range (%d:%d) exceeds the NARR range (%d:present)',
                                       start_year, end_year,
                                       NARR_start))
  }
  
  # Download Raw NARR from the internet
  
  vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate", "vwnd.10m", "uwnd.10m")
  ylist <- seq(end_year, start_year, by = -1)
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  rows <- length(vlist) * length(ylist)
  results <- data.frame(file = character(rows),
                        host = character(rows),
                        mimetype = character(rows), 
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows),
                        dbfile.name = "NARR", 
                        stringsAsFactors = FALSE)
  
  for (v in vlist) {
    for (year in ylist) {
      new.file <- file.path(outfolder, paste(v, year, "nc", sep = "."))
      
      # create array with results
      row <- which(vlist == v) * which(ylist == year)
      results$file[row]       <- new.file
      results$host[row]       <- PEcAn.remote::fqdn()
      results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
      results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
      results$mimetype[row]   <- "application/x-netcdf"
      results$formatname[row] <- "NARR"
      
      if (file.exists(new.file) && !overwrite) {
        PEcAn.logger::logger.debug("File '", new.file, "' already exists, skipping to next file.")
        next
      }
      
      url <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/", v, ".", year, ".nc")
      
      PEcAn.logger::logger.debug(paste0("Downloading from:\n", url, "\nto:\n", new.file))
      PEcAn.utils::download_file(url, new.file, method)
    }
  }
  
  return(invisible(results))
} # download.NARR
