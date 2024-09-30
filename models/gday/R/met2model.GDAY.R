# R Code to convert NetCDF CF met files into GDAY met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for GDAY
##'
##' @title met2model.GDAY
##' @description
##' Function to convert NetCDF met files in PEcAn-CF format into GDAY met driver files.
##' This function is an R wrapper to the python script "generate_forcing_data.py" 
##' in the inst/ folder. The python script supports arguments to generate sub-daily (30 min) 
##' weather data as well as soil temperature from 6 day running mean. These arguments are 
##' hard-coded in this function to generate daily GDAY files without soil temperature.
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will
##'        only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use
##'        the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param ... additional arguments, currently ignored
##' @return generates GDAY formatted met file as a side affect, returns file metadata
##' that will be inserted into database
##' @author Martin De Kauwe, Tony Gardella
met2model.GDAY <- function(in.path, in.prefix, outfolder, start_date, end_date, 
                           overwrite = FALSE, verbose = FALSE, ...) {
  
  ## GDAY driver format (.csv):
  ##
  ## Daily: year (-), doy (-; NB. leap years), tair (deg C),
  ##        rainfall (mm day-1), tsoil (deg C), tam (deg C), tpm (deg C),
  ##        tmin (deg C), tmax (deg C), tday (deg C), vpd_am (kPa),
  ##        vpd_pm (kPa), co2 (ppm), ndep (t ha-1 day-1), wind (m-2 s-1),
  ##        press (kPa), wind_am (m-2 s-1), wind_pm (m-2 s-1),
  ##        par_am (umol m-2 s-1), par_pm (umol m-2 s-1)
  ## 30min: year (-), doy (-; NB. leap years), hod (-), rainfall (mm 30 min-1),
  ##        par (umol m-2 s-1), tair (deg C), tsoil (deg C), vpd (kPa),
  ##        co2 (ppm), ndep (t ha-1 30 min-1), wind (m-2 s-1), press (kPa)
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  out.file   <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"), 
                      strptime(end_date, "%Y-%m-%d"),
                      sep = ".")
  out.file.full <- file.path(outfolder, out.file)
  
  ## file metadata to be entered into database
  results <- data.frame(file             = out.file.full, 
                        host             = PEcAn.remote::fqdn(), 
                        mimetype         = "text/csv", 
                        formatname       = "GDAY-met", 
                        startdate        = start_date, 
                        enddate          = end_date, 
                        dbfile.name      = out.file, 
                        stringsAsFactors = FALSE)
  
  if (file.exists(out.file.full) && !overwrite) {
    PEcAn.logger::logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }

  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  ## set arguments to generate_forcing_data.py script
  site           <- in.prefix
  fpath          <- in.path
  outfile_tag    <- out.file.full
  sub_daily      <- "false"      # Make 30-min file vs. Day, stick with day for now
  tsoil_run_mean <- "false"  # Generate Tsoil from 7-day running mean or not
  
  command        <- "python3"
  path2script    <- system.file("generate_forcing_data.py", package = "PEcAn.GDAY")
  
  ## construct command line argument
  all_args       <- paste(command, path2script, site, fpath, outfile_tag, sub_daily,
                          tsoil_run_mean)

  ## call conversion script
  system(all_args, ignore.stdout = FALSE, ignore.stderr = TRUE)
  
  return(invisible(results))
} # met2model.GDAY
