##' Download Ameriflux LBL CSV files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param site the FLUXNET ID of the site to be downloaded, used as file name prefix. 
##' The "SITE_ID" field in \href{http://ameriflux.lbl.gov/sites/site-list-and-pages/}{list of Ameriflux sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Ankur Desai, based on download.Ameriflux.R by Josh Mantooth, Rob Kooper

# lookup the site based on the site_id
download.AmerifluxLBL.site <- function(site_id) {
  sites <- read.csv(system.file("data/FLUXNET.sitemap.csv",package="PEcAn.data.atmosphere"),
                    stringsAsFactors=FALSE)
  sites$FLUX.id[which(sites$site.id == site_id)]
}

download.AmerifluxLBL <- function(sitename, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE) {
  # get start/end year code works on whole years only
  
  require(lubridate) 
  require(PEcAn.utils)
  require(data.table)
  require(httr)
  
  site <- sub(".* \\((.*)\\)", "\\1", sitename)
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  #need to query to get full file name #this is Ameriflux version
  url <- "http://wile.lbl.gov:8080/AmeriFlux/DataDownload.svc/datafileURLs"
  json_query <- paste0('{"username":"AnkurDesai","siteList":["',site,'"],"intendedUse":"Research - Land model/Earth system model","description":"testing REST download"}')
  result <- POST(url, body = json_query, encode = "json", add_headers("Content-Type" = "application/json"))
  link <- content(result)
  ftplink <- link$dataURLsList[[1]]$URL
  
  #test to see that we got back a FTP
  if (is.null(ftplink)) {    logger.severe("Could not get information about", site, ".",
                                         "Is this an Fluxnet2015 site?")}


  #copy file name to outputfile
  #set up outputfile stuff
  #download the FTP to outfolder
  #unzip the file
  
  # find all links we need based on the years and download them
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = site,
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
    results$formatname[row] <- 'AmeriFlux.level2.h.nc'
    
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