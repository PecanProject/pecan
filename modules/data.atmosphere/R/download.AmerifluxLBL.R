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
  json_query <- paste0('{"username":"AnkurDesai","siteList":["',site,'"],"intendedUse":"Research - Land model/Earth system model","description":"PEcAn download"}')
  result <- POST(url, body = json_query, encode = "json", add_headers("Content-Type" = "application/json"))
  link <- content(result)
  ftplink <- link$dataURLsList[[1]]$URL
  
  #test to see that we got back a FTP
  if (is.null(ftplink)) {    logger.severe("Could not get information about", site, ".",
                                         "Is this an AmerifluxLBL site?")}

   
  #get zip and csv filenames
  outfname <- strsplit(ftplink,'/')
  outfname <- outfname[[1]][length(outfname[[1]])]
  
  endname <- strsplit(outfname,'_')
  endname <- endname[[1]][length(endname[[1]])]
  endname <- substr(endname,1,nchar(endname)-3)
  
  outcsvname <- paste0(substr(outfname,1,15),'_HH_',endname,'csv')
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
      unzip(output_zip_file,outcsvname)
      #make sure a CSV file output
      if(!file.exists(output_csv_file)) {
        logger.severe("ZIP file ",output_zip_file," did not contain CSV file ",outcsvname)
      }
    }
  } else {   logger.debug("File '", output_csv_file, "' already exists, skipping extraction.")
  }
  
  #get start and end year of data from file
  firstline <- system(paste0("head -4 ",output_csv_file),intern=TRUE)
  firstline <- firstline[4]
  lastline <- system(paste0("tail -1 ",output_csv_file),intern=TRUE)
  
  firstdate_st <- paste0(substr(firstline,1,4),"-",substr(firstline,5,6),"-",substr(firstline,7,8)," ",substr(firstline,9,10),":",substr(firstline,11,12))
  firstdate <- as.POSIXlt(firstdate_st)
  lastdate_st <- paste0(substr(lastline,1,4),"-",substr(lastline,5,6),"-",substr(lastline,7,8)," ",substr(lastline,9,10),":",substr(lastline,11,12))
  lastdate <- as.POSIXlt(lastdate_st)
  
  syear <- year(firstdate)
  eyear <- year(lastdate)

  if (start_year>eyear) {logger.severe("Start_Year", start_year, "exceeds end of record ",eyear," for ",site)}
  if (end_year<syear) {logger.severe("End_Year", end_year, "precedes start of record ",syear," for ",site)}
  
  rows <- 1  
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = site,
                        stringsAsFactors = FALSE)
  
  row <- 1
  results$file[row] <- output_csv_file
  results$host[row] <- fqdn()
  results$startdate[row] <- firstdate_st
  results$enddate[row] <- lastdate_st
  results$mimetype[row] <- 'test/csv'
  results$formatname[row] <- 'AMERIFLUX_BASE_HH'
  
  # return list of files downloaded
  invisible(results)

}