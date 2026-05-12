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
  
  ## PEPRMT requires the following inputs: 
  ## Time_2 = d[,1] # day of year (1-infinite # of days)
  #DOY_disc_2=d[,2] #discontinuous day of year that starts over every year (1-365 or 366)
  #Year_2=d[,3] #year 
  #TA_2 = d[,4] #Air temperature (C)
  #WT_2 = d[,5] #water table depth (cm) equals 0 when water table at soil surface
  #PAR_2 <- d[,6] #photosynthetically active radiation (umol m-2 d-1)
  #LAI_2 <- d[,7] #Leaf area index (if not using can be 0s or NaN)
  #GI_2 <- d[,8] #greeness index from Phenocam (GCC) or Landsat EVI etc (unitless)
  #FPAR <- d[,9] #If using LAI data, set FPAR variable to 1's, if using a greenness index set FPAR to 0's
  #LUE<- d[,10] #growing season LUE computed for each site using measured GPP in gC per umol
  #wetland_age_2= d[,11] #Age of wetland in years
  #Sal <- d[,12] #Salinity (ppt)
  #NO3 <- d[,13] #Dissolved NO3 (mg/L)
  #SOM_2 <-d[,14] #Decomposed Organic matter : all the decomposed soil organic matter in top meter of soil informed buy MEM inclusive of current year
  #site_2 <-d[,15] #Site: if running more than 1 site, have 1s in this column for first site, 2s for 2nd site and so on

  start_date <- as.POSIXlt(start_date, tz = "UTC")
  start_date_string <- as.character(strptime(start_date, "%Y-%m-%d"))
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  if(nchar(in.prefix)>0 & substr(in.prefix,nchar(in.prefix),nchar(in.prefix)) != ".") in.prefix = paste0(in.prefix,".")

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
  for (year in start_year:end_year) {
    print(year)
    Year_2=year #year 
    
    old.file <- file.path(in.path, paste(in.prefix, year, ".nc", sep = ""))
    if(!file.exists(old.file)) PEcAn.logger::logger.error("file not found",old.file)
    ## open netcdf
    nc <- ncdf4::nc_open(old.file)

    ## convert time to seconds
    sec <- nc$dim$time$vals
    sec <- PEcAn.utils::ud_convert(sec, unlist(strsplit(nc$dim$time$units, " "))[1], "seconds")
    timestep.s <- 86400  # seconds in a day
    dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
    tstep <- round(timestep.s / dt) #4 per day
    dt    <- timestep.s / tstep  #dt is now an integer

    ## extract variables
    lat  <- ncdf4::ncvar_get(nc, "latitude")
    lon  <- ncdf4::ncvar_get(nc, "longitude")
    Tair <- ncdf4::ncvar_get(nc, "air_temperature")  ## in Kelvin
    SW <- ncdf4::ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air") #Shortwave
    ncdf4::nc_close(nc)

    ## build day of year
    diy <- PEcAn.utils::days_in_year(year)
    doy <- rep(seq_len(diy), each = timestep.s / dt)[seq_along(sec)]

    ## Aggregate variables up to daily
    TA_2         <- PEcAn.utils::ud_convert(tapply(Tair, doy, mean, na.rm = TRUE), "Kelvin", "Celsius")
    #Consider using a different met source for PAR (since this only has SW)
    PAR_2          <- tapply(2.114 * SW * dt, doy, sum, na.rm = TRUE) / (24*60*60) 
    #https://rdrr.io/cran/LakeMetabolizer/man/sw.to.par.html
    Time_2          <- tapply(doy, doy, mean)

    ## build data matrix
    tmp <- cbind(Time_2, PAR_2, TA_2)

    ##filter out days not included in start or end date
    if(year == start_year){
      start.row <- length(as.Date(paste0(start_year, "-01-01")):as.Date(start_date)) #extra days length includes the start date
      if (start.row > 1){
        PEcAn.logger::logger.info("Subsetting PEPRMT met to match start date ", as.Date(start_date))
        print(start.row)
        print(nrow(tmp))
        tmp <- tmp[start.row:nrow(tmp),]
      }
    }
    if (year == end_year){
      if(year == start_year){
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
  Time_2 = as.integer(Dates - as.Date(start_date))+1
  DOY_disc_2 = lubridate::yday(Dates) #discontinuous day of year that starts over every year (1-365 or 366)
  
  # TO DO: currently missing all of the following drivers
  WT_2 = NA
  LAI_2 = NA
  GI_2 = NA
  FPAR = NA
  LUE = NA
  wetland_age_2 = NA
  Sal = NA
  NO3 = NA
  SOM_2 = NA
  site_2 = 1
  
  #column order matters
  final <- cbind(out, DOY_disc_2, Year_2, TA_2, WT_2, LAI_2, GI_2, FPAR, 
                 LUE, wetland_age_2, Sal, NO3, SOM_2, site_2) %>%
    data.frame() %>%
    select(all_of(c("Time_2", "DOY_disc_2", "Year_2", "TA_2", "WT_2", 
                    "PAR_2", "LAI_2", "GI_2", "FPAR", 
                    "LUE", "wetland_age_2", "Sal", "NO3", "SOM_2", "site_2")))
  
  if(sum(is.na(final$WT_2))!=0) warning("Warning: missing water table depth. PEPRMT won't run.")
  if(sum(is.na(final$GI_2))!=0 & 
     sum(is.na(final$LAI_2))!=0) warning("Warning: missing greenness index and LAI both. PEPRMT won't run.")
  if(sum(is.na(final$FPAR))!=0) warning("Warning: missing FPAR. PEPRMT won't run.")
  if(sum(is.na(final$PAR))!=0) warning("Warning: missing PAR. PEPRMT won't run.")
  if(sum(is.na(final$LUE))!=0) warning("Warning: missing LUE. PEPRMT won't run.")
  if(sum(is.na(final$wetland_age_2))!=0) {
    warning("Warning: missing wetland age. Age assumed to be > 2yr.")
    final$wetland_age_2 <- 1000
  }
  if(sum(is.na(final$Sal))!=0) warning("Warning: missing salinity. PEPRMT won't run.")
  if(sum(is.na(final$NO3))!=0) warning("Warning: missing NO3. PEPRMT won't run.")
  if(sum(is.na(final$SOM_2))!=0) warning("Warning: missing SOM. PEPRMT won't run.")
  if(sum(is.na(final$site_2))!=0) warning("Warning: missing site. Site set to 1.")
  
  utils::write.table(final, out.file.full, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)

  return(invisible(results))

} # met2model.PEPRMT
