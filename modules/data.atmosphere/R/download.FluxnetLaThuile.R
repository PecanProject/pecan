# lookup the site based on the site_id
download.FluxnetLaThuile.site <- function(site_id) {
  sites <- utils::read.csv(system.file("data/FLUXNET.sitemap.csv", package = "PEcAn.data.atmosphere"), 
                    stringsAsFactors = FALSE)
  sites$FLUX.id[which(sites$site.id == site_id)]
} # download.FluxnetLaThuile.site

##' Download Flxunet LaThuile CSV files
##'
##' @name download.FluxnetLaThuile
##' @title download.FluxnetLaThuile
##' @export
##' @param sitename the FLUXNET ID of the site to be downloaded, used as file name prefix.
##'   The 'SITE_ID' field in \href{http://www.fluxdata.org/DataInfo/Dataset\%20Doc\%20Lib/SynthDataSummary.aspx}{list of Fluxnet LaThuile sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param username should be the registered Fluxnet username, else defaults to pecan
##' @param ... further arguments, currently ignored
##' 
##' @author Ankur Desai
download.FluxnetLaThuile <- function(sitename, outfolder, start_date, end_date, 
                                     overwrite = FALSE, verbose = FALSE, username = "pecan", ...) {
  # get start/end year code works on whole years only

  
  site <- sub(".* \\((.*)\\)", "\\1", sitename)
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  # make sure output folder exists
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # url where Ameriflux data is stored
  # ftp://ftp:ankurdesai@ftp.fluxdata.org/.fluxnet_30284/.coreplusquality_4036/US-WCr.2004.synth.hourly.coreplusquality.csv
  # change from here
  baseurl <- paste0("ftp://ftp:", 
                    username, 
                    "@ftp.fluxdata.org/.fluxnet_30284/.coreplusquality_4036/", 
                    site)
  
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
    outputfile <- file.path(outfolder, 
                            paste(site, "FluxnetLaThuile.hourly.coreplusquality", year, "csv", sep = "."))
    
    # create array with results
    row                     <- year - start_year + 1
    results$file[row]       <- outputfile
    results$host[row]       <- PEcAn.remote::fqdn()
    results$startdate[row]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[row]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[row]   <- "text/csv"
    results$formatname[row] <- "FluxnetLaThuile.hourly.coreplusquality"
    
    # see if file exists
    if (file.exists(outputfile) && !overwrite) {
      PEcAn.logger::logger.debug("File '", outputfile, "' already exists, skipping to next file.")
      next
    }
    
    file <- paste(baseurl, year, "synth.hourly.coreplusquality.csv", sep = ".")
    PEcAn.utils::download_file(file, outputfile)
  }
  
  # return list of files downloaded
  return(results)
} # download.FluxnetLaThuile
