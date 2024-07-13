##' Download Fluxnet 2015 CSV files
##'
##' @name download.Fluxnet2015
##' @title download.Fluxnet2015
##' @export
##' @param sitename the FLUXNET ID of the site to be downloaded, used as file name prefix. 
##' The 'SITE_ID' field in \href{https://fluxnet.org/sites/site-list-and-pages/}{list of Ameriflux sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Ankur Desai, based on download.Ameriflux.R by Josh Mantooth, Rob Kooper
download.Fluxnet2015 <- function(sitename, outfolder, start_date, end_date, 
                                 overwrite = FALSE, verbose = FALSE, username = "pecan", ...) {
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
  
  # need to query to get full file name - this is Fluxnet2015 version, TIER1 only
  url <- "http://wile.lbl.gov:8080/AmeriFlux/DataDownload.svc/datafileURLs"
  json_query <- paste0("{\"username\":\"", username, "\",\"siteList\":[\"", site, "\"],\"intendedUse\":\"Research - Land model/Earth system model\",\"description\":\"PEcAn download\",\"dataProduct\":\"SUBSET\",\"policy\":\"TIER1\"}")
  result <- httr::POST(url, body = json_query, encode = "json", httr::add_headers(`Content-Type` = "application/json"))
  link <- httr::content(result)
  ftplink <- NULL

  if(is.null(link) || is.atomic(link)) {
    PEcAn.logger::logger.severe("Could not get information about", site, ".", "Is this an Fluxnet2015 site?")
  } else if (length(link$dataURLsList) > 0) {
    ftplink <- link$dataURLsList[[1]]$URL
  }
  
  # test to see that we got back a FTP
  if (is.null(ftplink)) {
    PEcAn.logger::logger.severe("Could not get information about", site, ".", "Is this an Fluxnet2015 site?")
  }
  
  # get start and end year of data from filename
  syear <- as.numeric(substr(ftplink, nchar(ftplink) - 16, nchar(ftplink) - 13))
  eyear <- as.numeric(substr(ftplink, nchar(ftplink) - 11, nchar(ftplink) - 8))
  if (start_year > eyear) {
    PEcAn.logger::logger.severe("Start_Year", start_year, "exceeds end of record ", eyear, " for ", site)
  }
  if (end_year < syear) {
    PEcAn.logger::logger.severe("End_Year", end_year, "precedes start of record ", syear, " for ", site)
  }
  
  # get zip and csv filenames
  outfname <- strsplit(ftplink, "/")
  outfname <- outfname[[1]][length(outfname[[1]])]
  
  output_zip_file  <- file.path(outfolder, outfname)
  file_timestep_hh <- "HH"
  file_timestep_hr <- "HR"
  file_timestep    <- file_timestep_hh
  endname          <- strsplit(outfname, "_")
  endname          <- endname[[1]][length(endname[[1]])]
  endname          <- substr(endname, 1, nchar(endname) - 4)
  
  outcsvname <- paste0(substr(outfname, 1, 30), 
                       file_timestep_hh, "_", 
                       syear, "-", 
                       eyear, "_", 
                       endname, 
                       ".csv")
  output_csv_file <- file.path(outfolder, outcsvname)
  outcsvname_hr <- paste0(substr(outfname, 1, 30), 
                          file_timestep_hr, "_", 
                          syear, "-", eyear, "_", 
                          endname, 
                          ".csv")
  output_csv_file_hr <- file.path(outfolder, outcsvname_hr)
  
  download_file_flag <- TRUE
  extract_file_flag  <- TRUE
  if (!overwrite && file.exists(output_zip_file)) {
    PEcAn.logger::logger.debug("File '", output_zip_file, "' already exists, skipping download")
    download_file_flag <- FALSE
  }
  if (!overwrite && file.exists(output_csv_file)) {
    PEcAn.logger::logger.debug("File '", output_csv_file, "' already exists, skipping extraction.")
    download_file_flag <- FALSE
    extract_file_flag <- FALSE
    file_timestep <- "HH"
  } else {
    if (!overwrite && file.exists(output_csv_file_hr)) {
      PEcAn.logger::logger.debug("File '", output_csv_file_hr, "' already exists, skipping extraction.")
      download_file_flag <- FALSE
      extract_file_flag <- FALSE
      file_timestep <- "HR"
      outcsvname <- outcsvname_hr
      output_csv_file <- output_csv_file_hr
    }
  }
  
  if (download_file_flag) {
    extract_file_flag <- TRUE
    PEcAn.utils::download_file(ftplink, output_zip_file)
    if (!file.exists(output_zip_file)) {
      PEcAn.logger::logger.severe("FTP did not download ", output_zip_file, " from ", ftplink)
    }
  }
  if (extract_file_flag) {
    avail_file <- utils::unzip(output_zip_file, list = TRUE)
    if (length(grep("HH", avail_file)) > 0) {
      file_timestep <- "HH"
    } else {
      if (length(grep("HR", avail_file)) > 0) {
        file_timestep <- "HR"
        output_csv_file <- output_csv_file_hr
        outcsvname <- outcsvname_hr
      } else {
        PEcAn.logger::logger.severe("Half-hourly or Hourly data file was not found in ", output_zip_file)
      }
    }
    utils::unzip(output_zip_file, outcsvname, exdir = outfolder)
    if (!file.exists(output_csv_file)) {
      PEcAn.logger::logger.severe("ZIP file ", output_zip_file, " did not contain CSV file ", outcsvname)
    }
  }
  
  dbfilename <- paste0(substr(outfname, 1, 30), file_timestep, "_", syear, "-", eyear, "_", endname)
  
  rows <- 1
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows), 
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = dbfilename, 
                        stringsAsFactors = FALSE)
  
  results$file[rows]       <- output_csv_file
  results$host[rows]       <- PEcAn.remote::fqdn()
  results$startdate[rows]  <- paste0(syear, "-01-01 00:00:00")
  results$enddate[rows]    <- paste0(eyear, "-12-31 23:59:59")
  results$mimetype[rows]   <- "text/csv"
  results$formatname[rows] <- "FLUXNET2015_SUBSET_HH"
  
  # return list of files downloaded
  return(results)
} # download.Fluxnet2015
