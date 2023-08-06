library(testthat)

tmpdir <- tempfile(pattern = "amerifluxData")
dir.create(tmpdir)
on.exit(teardown(unlink(tmpdir, recursive = TRUE)))

sitename <- "US-Akn"
# outfolder <- "./amerifluxData"
outfolder <- tmpdir
start_date <- "2011-01-01"
end_date <- "2011-12-31"
username <- "pecan"
useremail <- "@"
data_product <- "BASE-BADM"
data_policy <- "CCBY4.0"
overwrite <- FALSE
verbose <- FALSE


start_date <- as.POSIXlt(start_date, tz = "UTC")
end_date <- as.POSIXlt(end_date, tz = "UTC")

start_year <- lubridate::year(start_date)
end_year <- lubridate::year(end_date)


site <- sub(".* \\((.*)\\)", "\\1", sitename)

# make sure output folder exists
if (!file.exists(outfolder)) {
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
}

repeat {
  tout <- options("timeout")
  zip_file <- try(amerifluxr::amf_download_base(
    user_id = username,
    user_email = useremail,
    site_id = site,
    data_product = data_product,
    data_policy = data_policy,
    agree_policy = TRUE,
    intended_use = "model",
    intended_use_text = "PEcAn download",
    verbose = verbose,
    out_dir = outfolder
  ))
  if (!inherits(zip_file, "try-error")) {
    break
  } else if (tout$timeout > 250) {
    PEcAn.logger::logger.severe("Download takes too long, check your connection.")
    break
  }
  PEcAn.logger::logger.info("Added 100 seconds before the download timeouts")
  options(timeout = tout$timeout + 100)
}

# Path to created zip-file
ftplink <- zip_file
if (!grepl(".zip", ftplink)) {
  PEcAn.logger::logger.info("Not able to download a zip-file. Check download.AmerifluxLBL inputs")
}

# get zip and csv filenames
outfname <- strsplit(ftplink, "/")
outfname <- outfname[[1]][length(outfname[[1]])]
output_zip_file <- file.path(outfolder, outfname)
file_timestep_hh <- "HH"
file_timestep_hr <- "HR"
file_timestep <- file_timestep_hh

endname <- strsplit(outfname, "_")
endname <- endname[[1]][length(endname[[1]])]
endname <- gsub("\\..*", "", endname)
outcsvname <- paste0(substr(outfname, 1, 15), "_", file_timestep_hh, "_", endname, ".csv")
output_csv_file <- file.path(outfolder, outcsvname)
outcsvname_hr <- paste0(substr(outfname, 1, 15), "_", file_timestep_hr, "_", endname, ".csv")
output_csv_file_hr <- file.path(outfolder, outcsvname_hr)

download_file_flag <- TRUE
extract_file_flag <- TRUE
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
  PEcAn.utils::download_file(ftplink, output_zip_file, method)
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

dbfilename <- paste0(substr(outfname, 1, 15), "_", file_timestep, "_", endname)

# get start and end year of data from file
firstline <- system(paste0("head -4 ", output_csv_file), intern = TRUE)
firstline <- firstline[4]
lastline <- system(paste0("tail -1 ", output_csv_file), intern = TRUE)

firstdate_st <- paste0(
  substr(firstline, 1, 4), "-",
  substr(firstline, 5, 6), "-",
  substr(firstline, 7, 8), " ",
  substr(firstline, 9, 10), ":",
  substr(firstline, 11, 12)
)
firstdate <- as.POSIXlt(firstdate_st)
lastdate_st <- paste0(
  substr(lastline, 1, 4), "-",
  substr(lastline, 5, 6), "-",
  substr(lastline, 7, 8), " ",
  substr(lastline, 9, 10), ":",
  substr(lastline, 11, 12)
)
lastdate <- as.POSIXlt(lastdate_st)

syear <- lubridate::year(firstdate)
eyear <- lubridate::year(lastdate)

if (start_year > eyear) {
  PEcAn.logger::logger.severe("Start_Year", start_year, "exceeds end of record ", eyear, " for ", site)
}
if (end_year < syear) {
  PEcAn.logger::logger.severe("End_Year", end_year, "precedes start of record ", syear, " for ", site)
}

test_that("Downloaded data files are present at the desired location", {
  expect_true(file.exists(paste0(output_csv_file)))
  expect_true(file.exists(paste0(output_zip_file)))
})
