# lookup the site based on the site_id
download.Ameriflux.site <- function(site_id) {
  sites <- read.csv(system.file("data/FLUXNET.sitemap.csv",package="PEcAn.data.atmosphere"),
                    stringsAsFactors=FALSE)
  sites$FLUX.id[which(sites$site.id == site_id)]
}

##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param site the site to be downloaded, will be used as prefix as well
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Josh Mantooth, Rob Kooper
download.Ameriflux <- function(sitename, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) {
  # get start/end year code works on whole years only
  
  require(lubridate) #is this necessary?
  require(PEcAn.utils)
  require(data.table)
  
  site = sub(".* \\((.*)\\)", "\\1", sitename)
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  # url where Ameriflux data is stored
  baseurl <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/", site, "/with_gaps/")
  
  # fetch all links
  links <- tryCatch({
    xpathSApply(htmlParse(baseurl), "//a/@href")
  }, error = function(e) {
    logger.severe("Could not get information about", site, ".",
                  "Is this an Ameriflux site?")
  })
  
  # find all links we need based on the years and download them
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        stringsAsFactors = FALSE)
  for(year in start_year:end_year) {
    outputfile <- file.path(outfolder, paste(site, year, "nc", sep="."))
    
    # create array with results
    row <- year - start_year + 1
    results$file[row] <- outputfile
    results$host[row] <- fqdn()
    results$startdate[row] <- paste0(year,"-01-01 00:00:00")
    results$enddate[row] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[row] <- 'application/x-netcdf'
    results$formatname[row] <- 'Ameriflux.level2.h.nc'
    
    # see if file exists
    if (file.exists(outputfile) && !overwrite) {
      logger.debug("File '", outputfile, "' already exists, skipping to next file.")
      next
    }
    
    file <- tail(as.character(links[grep(paste0('_', year, '_.*.nc'), links)]), n=1)
    if (length(file) == 0) {
      logger.severe("Could not download data for", site, "for the year", year)
    }
    download.file(paste0(baseurl, file), outputfile)
  }
  
  # return list of files downloaded
  invisible(results)
}

#site <- download.Ameriflux.site(622)
#print(download.Ameriflux(2001, 2005, site, "/tmp/met/ameriflux"))