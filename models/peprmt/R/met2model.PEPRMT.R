# R Code to convert NetCDF CF met files into PEPRMT met files

## If files already exist in 'Outfolder', the default function is NOT to overwrite them and only
## gives user the notice that file already exists. If user wants to overwrite the existing files,
## just change overwrite statement below to TRUE.

##' met2model for PEPRMT
##'
##' @title met2model.PEPRMT
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @param ... additional arguments, currently ignored
##' @author Abigail Lewis (add names)
met2model.PEPRMT <- function(in.path, in.prefix, outfolder, start_date, end_date,
                            overwrite = FALSE, verbose = FALSE, ...) {

  PEcAn.logger::logger.info("START met2model.PEPRMT")
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  start_date_string <- as.character(strptime(start_date, "%Y-%m-%d"))
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  if (nchar(in.prefix) > 0 && !any(endsWith(in.prefix, c(".", "_")))) {
    in.prefix <- paste0(in.prefix, ".")
  }

  out.file <- paste0(in.prefix, start_date_string,".",
                    strptime(end_date, "%Y-%m-%d"),
                    ".dat")
  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = c(out.file.full),
                        host = c(PEcAn.remote::fqdn()),
                        mimetype = c("text/plain"),
                        formatname = c("PEPRMT meteorology"),
                        startdate = c(start_date),
                        enddate = c(end_date),
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)

  if (file.exists(out.file.full) && !overwrite) {
    PEcAn.logger::logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  out <- NULL

  # Met files are annual. Get start/end year
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)

  ## Loop through and add air temp for each year
  for (Year in start_year:end_year) {
    print(Year)
    
    old.file <- file.path(in.path, paste(in.prefix, Year, ".nc", sep = ""))
    if(!file.exists(old.file)) PEcAn.logger::logger.error("file not found",old.file)
    ## open netcdf
    nc <- ncdf4::nc_open(old.file)

    ## convert time to seconds
    sec <- nc$dim$time$vals
    sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
    timestep.s <- 86400  # seconds in a day
    dt <- PEcAn.utils::seconds_in_year(Year) / length(sec)
    tstep <- round(timestep.s / dt) #4 per day
    dt    <- timestep.s / tstep  #dt is now an integer

    ## extract variables
    lat  <- ncdf4::ncvar_get(nc, "latitude")
    lon  <- ncdf4::ncvar_get(nc, "longitude")
    Tair <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
    SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air") #Shortwave
    ncdf4::nc_close(nc)

    ## build day of year
    diy <- PEcAn.utils::days_in_year(Year)
    doy <- rep(seq_len(diy), each = timestep.s / dt)[seq_along(sec)]

    ## Aggregate variables up to daily
    TA_C            <- PEcAn.utils::ud_convert(tapply(Tair, doy, mean, na.rm = TRUE), "Kelvin", "Celsius")
    #Consider using a different met source for PAR (since this only has SW)
    PAR_umol_m2_day <- tapply(2.114 * SW * dt, doy, sum, na.rm = TRUE) / (24*60*60) 
    #https://rdrr.io/cran/LakeMetabolizer/man/sw.to.par.html
    DOY_disc        <- tapply(doy, doy, mean)

    ## build data matrix
    tmp <- cbind(DOY_disc, PAR_umol_m2_day, TA_C, Year)

    ##filter out days not included in start or end date
    if(Year == start_year){
      start.row <- length(as.Date(paste0(start_year, "-01-01")):as.Date(start_date)) #extra days length includes the start date
      if (start.row > 1){
        PEcAn.logger::logger.info("Subsetting PEPRMT met to match start date ", as.Date(start_date))
        print(start.row)
        print(nrow(tmp))
        tmp <- tmp[start.row:nrow(tmp),]
      }
    }
    if (Year == end_year){
      if(Year == start_year){
        end.row <- length(as.Date(start_date):as.Date(end_date))
        if (end.row < nrow(tmp)){
          PEcAn.logger::logger.info("Subsetting PEPRMT met to match end date")
          tmp <- tmp[1:end.row,]
        }
      } else{
        end.row <- length(as.Date(paste0(end_year, "-01-01")):as.Date(end_date))
        if (end.row < nrow(tmp)){
          PEcAn.logger::logger.info("Subsetting PEPRMT met to match end date")
          tmp <- tmp[1:end.row,]
        }
      }

    }

    if (is.null(out)) {
      out <- tmp
    } else {
      out <- rbind(out, tmp)
    }
  }  ## end loop over years
  
  ## Assuming default values for some variables
  Dates = seq.Date(as.Date(start_date), as.Date(end_date), by = "1 day")
  DOY = as.integer(Dates - as.Date(start_date))+1
  
  #column order matters
  final <- cbind(out, DOY, Year) %>%
    data.frame() %>%
    select(all_of(c("Year", "DOY_disc",  "DOY", "TA_C", "PAR_umol_m2_day")))
  
  utils::write.table(final, out.file.full, quote = FALSE, sep = " ", row.names = FALSE, col.names = TRUE)

  return(invisible(results))

} # met2model.PEPRMT
