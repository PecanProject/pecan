##' Download NEON Site Met CSV files
##'
##' download.NEONmet
##' 
##' Uses NEON v0 API to download met data from NEON towers and convert to CF NetCDF
##' 
##' @export
##' @param sitename the NEON ID of the site to be downloaded, used as file name prefix. 
##' The 4-letter SITE code  in \href{https://www.neonscience.org/science-design/field-sites/list}{list of NEON sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year and month of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year and month part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose makes the function output more text
##' @param ... further arguments, currently ignored
##'
##' @examples 
##' \dontrun{
##' result <- download.NEONmet('HARV','~/','2017-01-01','2017-01-31',overwrite=TRUE)
##' }
download.NEONmet <- function(sitename, outfolder, start_date, end_date, 
                                  overwrite = FALSE, verbose = FALSE,  ...) {

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  #Check if site is a NEON site
  site <- sub(".* \\((.*)\\)", "\\1", sitename)
  siteinfo <- nneo::nneo_site(site)
  if (!exists("siteinfo")) {
    PEcAn.logger::logger.error("Could not get information about", sitename, ".", "Is this a NEON site?")
  }
  
  #See what products and dates are available for this site
  availProducts <- siteinfo$dataProducts$dataProductCode #list of data prodcuts by code
  availDates <- siteinfo$dataProducts$availableMonths #lists of availably YYYY-MM per site, use unlist()
  lat <- siteinfo$siteLatitude
  lon <- siteinfo$siteLongitude
  
  #Figure out which months are needed
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_ymd <- lubridate::ymd(start_date)
  end_ymd <- lubridate::ymd(end_date)

  #Subset to max/min available dates if start or end date exceed those bounds
  allDates <- unlist(availDates)
  minDate <- min(allDates)
  maxDate <- max(allDates)
  start_ym <- substr(start_ymd,1,7)
  end_ym <- substr(end_ymd,1,7)
  if (start_ym<minDate) { 
    start_ym <- minDate 
  }
  if (end_ym>maxDate) { 
    end_ym <- maxDate 
  }    
  start_year <- as.numeric(substr(start_ym,1,4))
  end_year <- as.numeric(substr(end_ym,1,4))
  
  #create results data frame
  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows), 
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = paste0("NEONmet.",site), 
                        stringsAsFactors = FALSE)
  
  all_years <- start_year:end_year
  all_files <- file.path(outfolder, paste0("NEONmet.", site, ".", as.character(all_years), ".nc"))
  results$file <- all_files
  results$host <- PEcAn.remote::fqdn()
  results$mimetype   <- "application/x-netcdf"
  results$formatname <- "CF"
  results$startdate  <- paste0(all_years, "-01-01 00:00:00")
  results$enddate    <- paste0(all_years, "-12-31 23:59:59")
  
  for (current_year in all_years) {
    
    #Figure out start and end time for this year
      y_idx <- current_year - start_year + 1
      if (current_year==start_year) 
      { 
        start_m <- substr(start_ym,6,7) 
      } else { 
        start_m <- "01" 
      }
      if (current_year==end_year) { 
        end_m <- substr(end_ym,6,7) 
      } else { 
        end_m <- "12" 
      } 
      days_in_last_month <- as.character(lubridate::days_in_month(lubridate::ymd(paste0(current_year,end_m,"-01"))))
      start_ymd <- (paste0(current_year,"-",start_m,"-01"))
      end_ymd <- (paste0(current_year,"-",end_m,"-",days_in_last_month))
      start_date <- as.POSIXlt(paste0(start_ymd," 00:00:00 UTC"), tz = "UTC")
      end_date <- as.POSIXlt(paste0(end_ymd," 23:30:00 UTC"), tz = "UTC")

  #Warn if no data is available for any months in given year
      monthsNeeded <- substr(seq(as.Date(start_ymd),as.Date(end_ymd),by='month'),1,7)
      if (length(intersect(unlist(availDates),monthsNeeded))==0) {
        PEcAn.logger::logger.warn("No data available in year ",current_year)
        next()
      }
      startMon <- min(monthsNeeded)
      endMon <- max(monthsNeeded)
  
  #Set up netcdf file, dimensions, and sequence of dates
    new.file <- all_files[y_idx]
    if (file.exists(new.file) && !overwrite) {
      PEcAn.logger::logger.debug("File '", new.file, "' already exists, skipping.")
      next()
    }
    
    seqTime <- seq(start_date,end_date,by=1800) 
    datetime <- as.POSIXct(seqTime)
    results$startdate[y_idx] <- as.character(datetime[1])
    results$enddate[y_idx] <- as.character(datetime[length(datetime)])
    
    days_since_1700 <- datetime - lubridate::ymd_hm("1700-01-01 00:00 UTC")
    t <- ncdf4::ncdim_def("time", "days since 1700-01-01", as.numeric(days_since_1700))  #define netCDF dimensions for variables
    timestep <- 1800
  
  ## create lat lon dimensions
    x <- ncdf4::ncdim_def("longitude", "degrees_east", lon)  # define netCDF dimensions for variables
    y <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
    xytdim <- list(x, y, t)
  
  #STEPS: Download all months in startdate to enddate for given variable
  # Read CSV, copy to array, add to NetCDF file

  #NEON.DP1.00002 Air Temp profile or NEON.DP1.00003 Triple-aspirated T (preferred)
    airTempLoc <- grep("DP1\\.00002",availProducts)
    airTemp3Loc <- grep("DP1\\.00003",availProducts)
    if ((length(airTempLoc)==0) && (length(airTemp3Loc)==0)) {
      PEcAn.logger::logger.error("Air temperature DP1.00002 or DP1.00003 not available") 
    }
    airTempDates <- neonmet.getDates(availDates,airTempLoc,startMon,endMon)
    airTemp3Dates <- neonmet.getDates(availDates,airTemp3Loc,startMon,endMon)
    nairTemp <- length(airTempDates)
    nairTemp3 <- length(airTemp3Dates)
    if ((nairTemp==0) && (nairTemp3==0)) {
      PEcAn.logger::logger.error("Air temperature DP1.00002 or DP1.00003 not available in date range ",startMon," ",endMon)
    }
  #define NetCDF variable and create NetCDF file
    airT.var <- ncdf4::ncvar_def(name = "air_temperature", units = "K", dim = xytdim)
    nc <- ncdf4::nc_create(new.file, vars = airT.var)  #create netCDF file
    if (nairTemp3>nairTemp) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON SingleAsp AirTemp")
      }
      ncdata <- neonmet.getVals(dates=airTemp3Dates,product=availProducts[airTemp3Loc[1]],site=site,
                                datetime=datetime,data_col="tempTripleMean",QF=1,
                                units=c("celsius","K"))
    } else {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON TripleAsp AirTemp")
      }
      ncdata <- neonmet.getVals(dates=airTempDates,product=availProducts[airTempLoc[1]],site=site,
                                datetime=datetime,data_col="tempSingleMean",
                                units=c("celsius","K"))
    }
    ncdf4::ncvar_put(nc, varid = airT.var, vals = ncdata)

    # NEON.DP1.00004 Pressure
    pSurfLoc <- grep("DP1\\.00004",availProducts)
    pSurfDates <- neonmet.getDates(availDates,pSurfLoc,startMon,endMon)
    npSurf <- length(pSurfDates)
    if (length(pSurfDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON Pressure")
      }
      Psurf.var <- ncdf4::ncvar_def(name = "air_pressure", units = "Pa", dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = Psurf.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=pSurfDates,product=availProducts[pSurfLoc[1]],site=site,
                                datetime=datetime,data_col="staPresMean",QF_col="staPresFinalQF",
                                units=c("kPa","Pa"))
      ncdf4::ncvar_put(nc, varid = Psurf.var, vals = ncdata)
    } else {
      PEcAn.logger::logger.warn("No NEON Pressure Data")
    }

    # NEON.DP1.00024 PAR
    PARLoc <- grep("DP1\\.00024",availProducts)
    PARDates <- neonmet.getDates(availDates,PARLoc,startMon,endMon)
    if (length(PARDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON PAR")
      }
      PAR.var <- ncdf4::ncvar_def(name = "surface_downwelling_photosynthetic_photon_flux_in_air", 
                                  units = "mol m-2 s-1", 
                                  dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = PAR.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=PARDates,product=availProducts[PARLoc[1]],site=site,
                                datetime=datetime,data_col="PARMean",QF_col="PARFinalQF",
                                units=c("umol m-2 s-1", "mol m-2 s-1"))
      ncdf4::ncvar_put(nc, varid = PAR.var, vals = ncdata)
    } else {
      PEcAn.logger::logger.warn("No NEON PAR DAta")
    }
    
    # NEON.DP1.00006 Precip (missing uncertainty information)
    precipLoc <- grep("DP1\\.00006",availProducts)
    precipDates <- neonmet.getDates(availDates,precipLoc,startMon,endMon)
    if (length(precipDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON Precip")
      }
      precip.var <- ncdf4::ncvar_def(name = "precipitation_flux",
                                     units = "kg m-2 s-1", 
                                     dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = precip.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=precipDates,product=availProducts[precipLoc[1]],site=site,
                                datetime=datetime,data_col="priPrecipBulk",QF_col="priPrecipFinalQF",
                                urlstring = "\\.00000\\.900\\.(.*)30min",
                                units=c("kg m-2 1/1800 s-1", "kg m-2 s-1")) #mm per half hour 
      ncdf4::ncvar_put(nc, varid = precip.var, vals = ncdata)
    } else {
      PEcAn.logger::logger.warn("No NEON Precip")
    }
    
    # NEON.DP1.00098 RH
    RHLoc <- grep("DP1\\.00098",availProducts)
    RHDates <- neonmet.getDates(availDates,RHLoc,startMon,endMon)
    if (length(RHDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON RH")
      }
      RH.var <- ncdf4::ncvar_def(name = "relative_humidity", 
                                  units = "%", 
                                  dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = RH.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=RHDates,product=availProducts[RHLoc[1]],site=site,
                                datetime=datetime,data_col="RHMean",QF_col="RHFinalQF",
                                units=c("%", "%"))
      ncdf4::ncvar_put(nc, varid = RH.var, vals = ncdata)
    } else {
      PEcAn.logger::logger.warn("No NEON RH data")
    }
    
    # DP1.00023 SW/LW or NEON.DP1.00022 SW (Possible future: DP1.00014 for Direct/Diffuse SW)
    SWLoc <- grep("DP1\\.00022",availProducts)
    SWLWLoc <- grep("DP1\\.00023",availProducts)
    SWDates <- neonmet.getDates(availDates,SWLoc,startMon,endMon)
    SWLWDates <- neonmet.getDates(availDates,SWLWLoc,startMon,endMon)
    if (length(SWLWDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON SWLW")
      }
      SW.var <- ncdf4::ncvar_def(name = "surface_downwelling_shortwave_flux_in_air", 
                          units = "W m-2", 
                          dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = SW.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=SWLWDates,product=availProducts[SWLWLoc[1]],site=site,
                                datetime=datetime,data_col="inSWMean",QF_col="inSWFinalQF",
                                units=c("W m-2", "W m-2"))
      ncdf4::ncvar_put(nc, varid = SW.var, vals = ncdata)
      LW.var <- ncdf4::ncvar_def(name = "surface_downwelling_longwave_flux_in_air",
                          units = "W m-2", 
                          dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = LW.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=SWLWDates,product=availProducts[SWLWLoc[1]],site=site,
                                datetime=datetime,data_col="inLWMean",QF_col="inLWFinalQF",
                                units=c("W m-2", "W m-2"))
      ncdf4::ncvar_put(nc, varid = LW.var, vals = ncdata)
    } else {
      if (length(SWDates)>0) {
        if (verbose) {
          PEcAn.logger::logger.info("Reading NEON SW")
        }
        SW.var <- ncdf4::ncvar_def(name = "surface_downwelling_shortwave_flux_in_air", 
                            units = "W m-2", 
                            dim = xytdim)
        nc <- ncdf4::ncvar_add(nc = nc, v = SW.var, verbose = verbose)
        ncdata <- neonmet.getVals(dates=SWDates,product=availProducts[SWLoc[1]],site=site,
                                  datetime=datetime,data_col="shortRadMean",
                                  units=c("W m-2", "W m-2"))
        ncdf4::ncvar_put(nc, varid = SW.var, vals = ncdata)
      } else {
        PEcAn.logger::logger.warn("No NEON SW/LW or SW")
      }
    }
  
    # NEON.DP1.00001 2D wind speed/direction - have to do northward/eastward math
    WSpdLoc = grep("DP1\\.00001",availProducts)
    WSpdDates <- neonmet.getDates(availDates,WSpdLoc,startMon,endMon)
    if (length(WSpdDates)>0) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON Wind Speed/Direction")
      }
      WSpd.var <- ncdf4::ncvar_def(name = "wind_speed", 
                                  units = "m s-1", 
                                  dim = xytdim)
      WDir.var <- ncdf4::ncvar_def(name = "wind_direction", 
                                   units = "degrees", 
                                   dim = xytdim)
      Ewind.var <- ncdf4::ncvar_def(name = "eastward_wind", units = "m s-1", dim = xytdim)
      Nwind.var <- ncdf4::ncvar_def(name = "northward_wind", units = "m s-1", dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = WSpd.var, verbose = verbose)
      nc <- ncdf4::ncvar_add(nc = nc, v = WDir.var, verbose = verbose)
      nc <- ncdf4::ncvar_add(nc = nc, v = Ewind.var, verbose = verbose)
      nc <- ncdf4::ncvar_add(nc = nc, v = Nwind.var, verbose = verbose)
      ncdata_spd <- neonmet.getVals(dates=WSpdDates,product=availProducts[WSpdLoc[1]],site=site,
                                datetime=datetime,data_col="windSpeedMean",QF_col="windSpeedFinalQF",
                                units=c("m s-1", "m s-1"))
      ncdf4::ncvar_put(nc, varid = WSpd.var, vals = ncdata_spd)
      ncdata_dir <- neonmet.getVals(dates=WSpdDates,product=availProducts[WSpdLoc[1]],site=site,
                                datetime=datetime,data_col="windDirMean",QF_col="windDirFinalQF",
                                units=c("degrees", "degrees"))      
      
      ncdf4::ncvar_put(nc, varid = WDir.var, vals = ncdata_dir)     
      wdir_rad <- PEcAn.utils::ud_convert(ncdata_dir,"degrees","radians")
      ncdata_e <- ncdata_spd * cos(wdir_rad)
      ncdata_n <- ncdata_spd * sin(wdir_rad)
      ncdf4::ncvar_put(nc, varid = Ewind.var, vals = ncdata_e)
      ncdf4::ncvar_put(nc, varid = Nwind.var, vals = ncdata_n)
    } else {
      PEcAn.logger::logger.warn("No NEON Wind data")
    }
    
    # NEON.DP1.00041 Soil temp    (take 2cm level which is level 501)
    soilTLoc = grep("DP1\\.00041",availProducts)
    soilTDates <- neonmet.getDates(availDates,soilTLoc,startMon,endMon)
    if (length(soilTDates>0)) {
      if (verbose) {
        PEcAn.logger::logger.info("Reading NEON Soil Temp")
      }
      soilT.var <- ncdf4::ncvar_def(name = "soil_temperature", 
                                  units = "K", 
                                  dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = soilT.var, verbose = verbose)
      ncdata <- neonmet.getVals(dates=soilTDates,product=availProducts[soilTLoc[1]],site=site,
                                datetime=datetime,data_col="soilTempMean",
                                urlstring = "\\.00000\\.001\\.5..\\.(.*)_30_minute",
                                units=c("celsius", "K"),belowground=TRUE)      
      ncdf4::ncvar_put(nc, varid = soilT.var, vals = ncdata)
    } else {
      PEcAn.logger::logger.warn("No NEON Soil Temp")
    }
    
    # NEON.DP1.00034 CO2 at tower top (alt NEON.DP3.00009 CO2 profile) - not yet avail, don't have variable names
  ncdf4::nc_close(nc)
  } #For loop
  return(invisible(results))
} #function

neonmet.getDates <- function(availDates,Loc,startMon,endMon) {
  if (length(Loc)>0) { 
    Dates <- unlist(availDates[Loc[1]])
    GoodDates <- which((Dates >= startMon) & (Dates <= endMon))
    if (length(GoodDates)>0) {
      return(Dates[GoodDates])
    } else {
      return(NULL)
    }
  } else { 
    return(NULL)
  }  
}

neonmet.getVals <- function(dates,product,site,datetime,
                            data_col,QF_col="finalQF", QF=0,
                            urlstring = "\\.00000\\.000\\.(.*)30min",units,FillValue=NA,belowground=FALSE) {
  ncdata <- rep(FillValue,length(datetime))
  for (mon in dates) {
    neonData <- nneo::nneo_data(product_code = product, site_code = site, year_month = mon)
    urls <- neonData$data$files$name
    if (length(urls)>0) {
      #Extract and read 30 minute data from the highest vertical level among files returned
      #If belowground, then take top most level (lowest value)
      if (belowground==TRUE) {
        url30 <- utils::head(sort(urls[grep(urlstring,urls)]),1)
      } else {
        url30 <- utils::tail(sort(urls[grep(urlstring,urls)]),1)
      }
      if (length(url30)!=0) {
        csvData <- nneo::nneo_file(product_code = product, site_code = site, year_month = mon, filename = url30) 
        #Retreive time dimension and figure out where in array to put it
        csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
        arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
        csvVar <- csvData[[data_col]]
        if (length(QF_col)!=0) {
          csvQF <- csvData[[QF_col]]
          csvVar[which(csvQF!=QF)] <- NA 
        }
        if ((length(units)=2)&&(units[1]!=units[2])) {
          csvVar <- PEcAn.utils::ud_convert(csvVar,units[1], units[2])
          #need a correction for precip or rate conversion /1800
        }
        ncdata[arrLoc] <- csvVar
      }
    }
  }
  return(ncdata)
}



