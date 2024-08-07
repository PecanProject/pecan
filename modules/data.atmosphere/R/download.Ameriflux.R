# lookup the site based on the site_id
download.Ameriflux.site <- function(site_id) {
  sites <- utils::read.csv(system.file("data/FLUXNET.sitemap.csv", package = "PEcAn.data.atmosphere"), 
                    stringsAsFactors = FALSE)
  sites$FLUX.id[which(sites$site.id == site_id)]
} # download.Ameriflux.site


##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param sitename the FLUXNET ID of the site to be downloaded, used as file name prefix. 
##' The 'SITE_ID' field in \href{http://ameriflux.lbl.gov/sites/site-list-and-pages/}{list of Ameriflux sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param ... further arguments, currently ignored
##' 
##' @author Josh Mantooth, Rob Kooper, Ankur Desai
download.Ameriflux <- function(sitename, outfolder, start_date, end_date,
                               overwrite = FALSE, verbose = FALSE, ...) {
  # get start/end year code works on whole years only
  
  site <- sub(".* \\((.*)\\)", "\\1", sitename)
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  # make sure output folder exists
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # url where Ameriflux data is stored
  baseurl <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/", site, "/with_gaps/")
  # Hack needed for US-UMB which has hourly and half-hourly folders separate. This version sticks
  # with hourly which has longer site record
  if (site == "US-UMB") {
    baseurl <- paste0(baseurl, "/hourly/")
  }
  
  # fetch all links
  links <- tryCatch({
    XML::xpathSApply(XML::htmlParse(baseurl), "//a/@href")
  }, error = function(e) {
    PEcAn.logger::logger.severe("Could not get information about", site, ".", "Is this an Ameriflux site?")
  })
  
  # find all links we need based on the years and download them
  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows),
                        startdate = character(rows),
                        enddate = character(rows), 
                        dbfile.name = site, 
                        stringsAsFactors = FALSE)
  for (year in start_year:end_year) {
    outputfile <- file.path(outfolder, paste(site, year, "nc", sep = "."))
    
    # create array with results
    row                     <- year - start_year + 1
    results$file[row]       <- outputfile
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[row]   <- "application/x-netcdf"
    results$formatname[row] <- "AmeriFlux.level2.h.nc"
    
    # see if file exists
    if (file.exists(outputfile) && !overwrite) {
      PEcAn.logger::logger.debug("File '", outputfile, "' already exists, skipping to next file.")
      next
    }
    
    file <- utils::tail(as.character(links[grep(paste0("_", year, "_.*.nc"), links)]), n = 1)
    if (length(file) == 0) {
      PEcAn.logger::logger.severe("Could not download data for", site, "for the year", year)
    }
    PEcAn.utils::download_file(paste0(baseurl, file), outputfile)
  }
  
  # return list of files downloaded
  return(invisible(results))
} # download.Ameriflux

#site <- download.Ameriflux.site(622)
#print(download.Ameriflux(2001, 2005, site, "/tmp/met/ameriflux"))
