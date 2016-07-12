##' Download Fluxnet 2015 CSV files
##'
##' @name download.Fluxnet2015
##' @title download.Fluxnet2015
##' @export
##' @param site the FLUXNET ID of the site to be downloaded, used as file name prefix. 
##' The "SITE_ID" field in \href{http://fluxnet.fluxdata.org//sites/site-list-and-pages/}{list of Ameriflux sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Ankur Desai, based on download.Ameriflux.R by Josh Mantooth, Rob Kooper

download.Fluxnet2015 <- function(sitename, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE, ...) {
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
  
  #need to query to get full file name - this is Fluxnet2015 version, TIER1 only
  url <- "http://wile.lbl.gov:8080/AmeriFlux/DataDownload.svc/datafileURLs"
  json_query <- paste0('{"username":"AnkurDesai","siteList":["',site,'"],"intendedUse":"Research - Land model/Earth system model","description":"PEcAn download","dataProduct":"SUBSET","policy":"TIER1"}')  
  result <- POST(url, body = json_query, encode = "json", add_headers("Content-Type" = "application/json"))
  link <- content(result)
  ftplink <- NULL
  if (length(link$dataURLsList) > 0) { ftplink <- link$dataURLsList[[1]]$URL } 
  
  #test to see that we got back a FTP
  if (is.null(ftplink)) {    logger.severe("Could not get information about", site, ".",
                                           "Is this an Fluxnet2015 site?")}
  
  #get start and end year of data from filename
  syear <- as.numeric(substr(ftplink,nchar(ftplink)-16,nchar(ftplink)-13))
  eyear <- as.numeric(substr(ftplink,nchar(ftplink)-11,nchar(ftplink)-8))
  
  if (start_year>eyear) {logger.severe("Start_Year", start_year, "exceeds end of record ",eyear," for ",site)}
  if (end_year<syear) {logger.severe("End_Year", end_year, "precedes start of record ",syear," for ",site)}
  
  #get zip and csv filenames
  outfname <- strsplit(ftplink,'/')
  outfname <- outfname[[1]][length(outfname[[1]])]
  
  endname <- strsplit(outfname,'_')
  endname <- endname[[1]][length(endname[[1]])]
  endname <- substr(endname,1,nchar(endname)-4)
  dbfilename <- paste0(substr(outfname,1,30),'HH_',syear,'-',eyear,'_',endname)
  outcsvname <- paste0(substr(outfname,1,30),'HH_',syear,'-',eyear,'_',endname,'.csv')
  
  output_zip_file <- file.path(outfolder, outfname)
  output_csv_file <- file.path(outfolder, outcsvname)
  
  #if CSV file exists, then skip extraction
  if (!file.exists(output_csv_file) || overwrite) {
    #if zip file downloaded, then skip ftp
    if (!file.exists(output_zip_file) || overwrite) {
      download.file(ftplink,output_zip_file)
    } else {   logger.debug("File '", output_zip_file, "' already exists, skipping download.")
    }
    #if FTP failed to output a file, then error
    if (!file.exists(output_zip_file)) {
      logger.severe("FTP did not download ", output_zip_file, " from ",ftplink)
    } else {
      #extract the half hourly file only
      unzip(output_zip_file,outcsvname,exdir=outfolder)
      #make sure a CSV file output
      if(!file.exists(output_csv_file)) {
        logger.severe("ZIP file ",output_zip_file," did not contain CSV file ",outcsvname)
      }
    }
  } else {   logger.debug("File '", output_csv_file, "' already exists, skipping extraction.")
  }

  rows <- 1  
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = dbfilename,
                        stringsAsFactors = FALSE)

  row <- 1
  results$file[row] <- output_csv_file
  results$host[row] <- fqdn()
  results$startdate[row] <- paste0(syear,"-01-01 00:00:00")
  results$enddate[row] <- paste0(eyear,"-12-31 23:59:59")
  results$mimetype[row] <- 'text/csv'
  results$formatname[row] <- 'FLUXNET2015_SUBSET_HH'

  # return list of files downloaded
  invisible(results)
}