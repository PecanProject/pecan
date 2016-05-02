##' Download NARR files
##'
##' @name download.NARR
##' @title download.NARR
##' @export
##' @param outfolder
##' @param start_year
##' @param end_year
##'
##' @author Betsy Cowdery
download.NARR <- function(outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE, ...){

  require(PEcAn.utils)
  require(lubridate)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)

  # Download Raw NARR from the internet

  vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate","vwnd.10m","uwnd.10m")
  ylist <- seq(end_year,start_year,by=-1)

  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)

  rows <- length(vlist)*length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "NARR",
                        stringsAsFactors = FALSE)

  for (v in vlist) {
    for (year in ylist) {
      new.file <- file.path(outfolder, paste(v, year, "nc", sep="."))

      # create array with results
      row <- which(vlist==v)*which(ylist==year)
      results$file[row] <- new.file
      results$host[row] <- fqdn()
      results$startdate[row] <- paste0(year,"-01-01 00:00:00")
      results$enddate[row] <- paste0(year,"-12-31 23:59:59")
      results$mimetype[row] <- 'application/x-netcdf'
      results$formatname[row] <- 'NARR'

      if (file.exists(new.file) && !overwrite) {
        logger.debug("File '", new.file, "' already exists, skipping to next file.")
        next
      }

      url <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/", v, ".", year, ".nc")
      download.file(url, new.file)
    }
  }

  invisible(results)
}
