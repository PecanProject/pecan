##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.LINKAGES
##' @title Write LINKAGES met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @return OK if everything was succesful.
##' @export
##' @author Ann Raiho, Betsy Cowdery
##-------------------------------------------------------------------------------------------------#
met2model.LINKAGES <- function(in.path, in.prefix, outfolder, start_date, end_date,
                               overwrite = FALSE, verbose = FALSE, ...) {
  library(PEcAn.utils)

  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  out.file <- file.path(outfolder, "climate.Rdata")
  # out.file <- file.path(outfolder, paste(in.prefix, strptime(start_date, '%Y-%m-%d'),
  # strptime(end_date, '%Y-%m-%d'), 'dat', sep='.'))
  
  # get start/end year since inputs are specified on year basis
  # use years to check if met data contains all of the necessary years 
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  results <- data.frame(file = c(out.file),
                        host = c(PEcAn.remote::fqdn()),
                        mimetype = c("text/plain"),
                        formatname = c("LINKAGES meteorology"),
                        startdate = c(start_date),
                        enddate = c(end_date),
                        dbfile.name = "climate.Rdata",
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)

  if (file.exists(out.file) && !overwrite) {
    
    # get year span for current data file
    load(out.file)
    data_start = min(rownames(temp.mat))
    data_end = max(rownames(temp.mat))
    
    # check to see if needed years fall into the current data year span; if not, rewrite
    if ((data_start <= start_year) & (data_end >= end_year)){
      PEcAn.logger::logger.debug("File '", out.file, "' already exists, skipping to next file.")
      return(invisible(results))
    }
  }

  library(PEcAn.data.atmosphere)

  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  out <- NULL

  year <- sprintf("%04d", seq(start_year, end_year, 1))
  month <- sprintf("%02d", seq(1, 12, 1))

  nyear <- length(year)  # number of years to simulate

  month_matrix_precip <- matrix(NA, nyear, 12)
  
  if(nchar(in.prefix)>0 & substr(in.prefix,nchar(in.prefix),nchar(in.prefix)) != ".") in.prefix = paste0(in.prefix,".")

  for (i in seq_len(nyear)) {
    year_txt <- formatC(year[i], width = 4, format = "d", flag = "0")
    infile <- file.path(in.path, paste0(in.prefix, year_txt, ".nc"))
    ncin <- ncdf4::nc_open(infile)

    ## convert time to seconds
    sec <- ncin$dim$time$vals
    sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(ncin$dim$time$units, " "))[1], "seconds")
    dt <- PEcAn.utils::seconds_in_year(as.numeric(year[i])) / length(sec)
    tstep <- 86400 / dt
    
    # adjust vector depending on the time step of data 
    # assumes evenly-spaced measurements
    DOY_vec_hr <- c(1, c(32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365) * as.integer(tstep))

    ncprecipf <- ncdf4::ncvar_get(ncin, "precipitation_flux")  # units are kg m-2 s-1
    for (m in 1:12) {
      month_matrix_precip[i, m] <- (sum(ncprecipf[DOY_vec_hr[m]:(DOY_vec_hr[m + 1] - 1)]) * dt / 10)
    }
    ncdf4::nc_close(ncin)
    # if(i%%100==0) cat(i,' '); flush.console()
  }

  month_matrix_temp_mean <- matrix(NA, nyear, 12)

  for (i in seq_len(nyear)) {

    year_txt <- formatC(year[i], width = 4, format = "d", flag = "0")

    infile <- file.path(in.path, paste0(in.prefix, year_txt, ".nc"))

    ncin <- ncdf4::nc_open(infile)
    # print(ncin)
    nctemp <- ncdf4::ncvar_get(ncin, "air_temperature")  #units are kg m-2 s-1
    for (m in 1:12) {
      month_matrix_temp_mean[i, m] <- (mean(nctemp[DOY_vec_hr[m]:(DOY_vec_hr[m + 1] - 1)]) -
                                         273.15)  #sub daily to monthly
    }
    ncdf4::nc_close(ncin)
    if (i %% 100 == 0) {
      cat(i, " ")
    }
    flush.console()
  }

  precip.mat <- month_matrix_precip
  temp.mat <- month_matrix_temp_mean
  rownames(temp.mat) <- start_year:end_year
  rownames(precip.mat) <- start_year:end_year
  save(precip.mat, temp.mat, file = out.file)
  return(invisible(results))
} # met2model.LINKAGES
