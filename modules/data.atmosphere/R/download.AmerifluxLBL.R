##' Download Ameriflux LBL CSV files
##'
##' download.AmerifluxLBL. Function uses amf_download_base function from amerifluxr package
##' to download a zip-file of data. The zip-file is extracted to a csv-file that is stored
##' to the given outfolder. Details about amf_download_base function can be found here:
##' https://github.com/chuhousen/amerifluxr/blob/master/R/amf_download_base.R
##'
##' Uses Ameriflux LBL JSON API to download met data from Ameriflux towers in CSV format
##'
##' @export
##' @param sitename the Ameriflux ID of the site to be downloaded, used as file name prefix.
##' The 'SITE_ID' field in \href{http://ameriflux.lbl.gov/sites/site-list-and-pages/}{list of Ameriflux sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param username Ameriflux username
##' @param method Optional. download_file() function option.  Use this to set custom programs such as ncftp
##' @param useremail Used email, should include 'address sign' for code to be functional
##' @param data_product AmeriFlux data product
##' @param data_policy Two possible licenses (based on the site): 'CCBY4.0' or 'LEGACY'
##' @param ... further arguments, currently ignored
##'
##' @examples
##' \dontrun{
##' result <- download.AmerifluxLBL("US-Akn","~/","2011-01-01","2011-12-31",overwrite=TRUE)
##' }
##'
##' @author Ankur Desai, Henri Kajasilta based on download.Ameriflux.R by Josh Mantooth, Rob Kooper, Shawn Serbin


download.AmerifluxLBL <- function(sitename, outfolder, start_date, end_date,
                                  overwrite = FALSE, verbose = FALSE, username = "pecan", method,
                                  useremail = "@", data_product = "BASE-BADM", data_policy = "CCBY4.0", ...) {

  # Initial set-ups for amerifluxr packages
  # get start/end year code works on whole years only
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")

  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)


  site <- sub(".* \\((.*)\\)", "\\1", sitename)


  # make sure output folder exists
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }

  version <- amerifluxr::amf_var_info()
  version <- unique(version[version$Site_ID == site,]$BASE_Version)
  if (length(version) != 1) {
    PEcAn.logger::logger.error("Could not find AmerifluxLBL version info for site", site)
  }
  expected_filename <- paste0("AMF_", site, "_", data_product, "_", version, ".zip")
  expected_fullpath <- file.path(outfolder, expected_filename)

  if (!overwrite && file.exists(expected_fullpath)) {
    PEcAn.logger::logger.debug("File '", expected_filename, "' already exists, skipping download")
    zip_file <- expected_fullpath
  } else {
    repeat {
      tout <- getOption("timeout")
      zip_file <- try(
        amerifluxr::amf_download_base(
          user_id = username,
          user_email = useremail,
          site_id = site,
          data_product = data_product,
          data_policy = data_policy,
          agree_policy = TRUE,
          intended_use = "model",
          intended_use_text = "PEcAn download",
          verbose = verbose,
          out_dir = outfolder)
      )
      if (!inherits(zip_file, "try-error")){
        break
      }else if(tout > 250 ){
        PEcAn.logger::logger.severe("Download takes too long, check your connection.")
        break
      }
      PEcAn.logger::logger.info("Added 100 seconds before the download timeouts")
      options(timeout = tout + 100)
    }
  }


  # Path to created zip-file
  if(!grepl(".zip", zip_file)){
    PEcAn.logger::logger.error("Not able to download a zip-file. Check download.AmerifluxLBL inputs")
  }

  # get zip and csv filenames
  outfname <- basename(zip_file)

  if (outfname != expected_filename) {
    PEcAn.logger::logger.info(
      "Downloaded a file named", sQuote(outfname),
      "but download.AmerifluxLBL was expecting", sQuote(expected_filename), ". This may be a PEcAn bug.")
  }

  file_timestep_hh <- "HH"
  file_timestep_hr <- "HR"
  file_timestep <- file_timestep_hh

  # TODO: Are values of data_product other than "BASE-BADM" actually supported?
  # If not, this whole block reduces to
  #   outcsvname <- sub("-BADM(.*).zip", "_HH\\1.csv", outfname)
  # And: if other values _are_ supported, are the fixed-length substrings below
  #   still correct?
  endname <- strsplit(outfname, "_")
  endname <- endname[[1]][length(endname[[1]])]
  endname <- gsub("\\..*", "", endname)
  outcsvname <- paste0(substr(outfname, 1, 15), "_", file_timestep_hh, "_", endname, ".csv")
  output_csv_file <- file.path(outfolder, outcsvname)
  outcsvname_hr <- paste0(substr(outfname, 1, 15), "_", file_timestep_hr, "_", endname, ".csv")
  output_csv_file_hr <- file.path(outfolder, outcsvname_hr)

  extract_file_flag      <- TRUE
  if (!overwrite && file.exists(output_csv_file)) {
    PEcAn.logger::logger.debug("File '", output_csv_file, "' already exists, skipping extraction.")
    extract_file_flag    <- FALSE
    file_timestep        <- "HH"
  } else {
    if (!overwrite && file.exists(output_csv_file_hr)) {
      PEcAn.logger::logger.debug("File '", output_csv_file_hr, "' already exists, skipping extraction.")
      extract_file_flag  <- FALSE
      file_timestep      <- "HR"
      outcsvname         <- outcsvname_hr
      output_csv_file    <- output_csv_file_hr
    }
  }

  if (extract_file_flag) {
    avail_file <- utils::unzip(zip_file, list = TRUE)
    if (length(grep("HH", avail_file)) > 0) {
      file_timestep <- "HH"
    } else {
      if (length(grep("HR", avail_file)) > 0) {
        file_timestep <- "HR"
        output_csv_file <- output_csv_file_hr
        outcsvname <- outcsvname_hr
      } else {
        PEcAn.logger::logger.severe("Half-hourly or Hourly data file was not found in ", zip_file)
      }
    }
    utils::unzip(zip_file, outcsvname, exdir = outfolder)
    if (!file.exists(output_csv_file)) {
      PEcAn.logger::logger.severe("ZIP file ", zip_file, " did not contain CSV file ", outcsvname)
    }
  }

  dbfilename <- paste0(substr(outfname, 1, 15), "_", file_timestep, "_", endname)

  # get start and end year of data from file
  firstline <- system(paste0("head -4 ", output_csv_file), intern = TRUE)
  firstline <- firstline[4]
  lastline <- system(paste0("tail -1 ", output_csv_file), intern = TRUE)

  firstdate_st <- paste0(substr(firstline, 1, 4), "-",
                         substr(firstline, 5, 6), "-",
                         substr(firstline, 7, 8), " ",
                         substr(firstline, 9, 10), ":",
                         substr(firstline, 11, 12))
  firstdate <- as.POSIXlt(firstdate_st)
  lastdate_st <- paste0(substr(lastline, 1, 4), "-",
                        substr(lastline, 5, 6), "-",
                        substr(lastline, 7, 8), " ",
                        substr(lastline, 9, 10), ":",
                        substr(lastline, 11, 12))
  lastdate <- as.POSIXlt(lastdate_st)

  syear <- lubridate::year(firstdate)
  eyear <- lubridate::year(lastdate)

  if (start_year > eyear) {
    PEcAn.logger::logger.severe("Start_Year", start_year, "exceeds end of record ", eyear, " for ", site)
  }
  if (end_year < syear) {
    PEcAn.logger::logger.severe("End_Year", end_year, "precedes start of record ", syear, " for ", site)
  }

  rows    <- 1
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
  results$startdate[rows]  <- firstdate_st
  results$enddate[rows]    <- lastdate_st
  results$mimetype[rows]   <- "text/csv"
  results$formatname[rows] <- "AMERIFLUX_BASE_HH"

  # return list of files downloaded
  return(results)
} # download.AmerifluxLBL
