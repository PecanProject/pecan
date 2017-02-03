##' Download NEON Site Met CSV files
##'
##' @name download.NEONmet
##' @title download.NEONmet
##' @export
##' @param site the NEON ID of the site to be downloaded, used as file name prefix. 
##' The 4-letter SITE code  in \href{http://www.neonscience.org/science-design/field-sites/list}{list of NEON sites}
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year and month of the date)
##' @param end_date the end date of the data to be downloaded. Format is YYYY-MM-DD (will only use the year and month part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' 
##' @author Ankur Desai, based on download.AmerifluxLBL.R

download.NEONmet <- function(sitename, outfolder, start_date, end_date, 
                                  overwrite = FALSE, verbose = FALSE, username = "pecan", ...) {

  #devtools::install_github("ropenscilabs/nneo")
  library(nneo)
  library(lubridate)
  library(ncdf4)
  library(udunits2)
  library(PEcAn.utils)
  
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  #Check if site is a NEON site
  site <- sub(".* \\((.*)\\)", "\\1", sitename)
  siteinfo <- nneo::nneo_site(site)
  if (!exists("siteinfo")) {
    PEcAn.utils::logger.error("Could not get information about", sitename, ".", "Is this a NEON site?")
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
  allDates = unlist(availDates)
  minDate <- min(allDates)
  maxDate <- max(allDates)
  start_ym = substr(start_ymd,1,7)
  end_ym = substr(end_ymd,1,7)
  if (start_ym<minDate) { start_ym <- minDate }
  if (end_ym>maxDate) { end_ym <- maxDate }    
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
  results$host <- fqdn()
  results$mimetype   <- "application/x-netcdf"
  results$formatname <- "CF"
  results$startdate  <- paste0(all_years, "-01-01 00:00:00")
  results$enddate    <- paste0(all_years, "-12-31 23:59:59")
  
  for (current_year in all_years) {
    
    #Figure out start and end time for this year
      y_idx <- current_year - start_year + 1
      if (current_year==start_year) { start_m <- substr(start_ym,6,7) } else { start_m <- "01" }
      if (current_year==end_year) { end_m <- substr(end_ym,6,7) } else { end_m <- "12" } 
      days_in_last_month <- as.character(lubridate::days_in_month(lubridate::ymd(paste0(current_year,end_m,"-01"))))
      start_ymd = (paste0(current_year,"-",start_m,"-01"))
      end_ymd = (paste0(current_year,"-",end_m,"-",days_in_last_month))
      start_date <- as.POSIXlt(paste0(start_ymd," 00:00:00 UTC"), tz = "UTC")
      end_date <- as.POSIXlt(paste0(end_ymd," 23:30:00 UTC"), tz = "UTC")

  #Warn if no data is available for any months in given year
      monthsNeeded = substr(seq(as.Date(start_ymd),as.Date(end_ymd),by='month'),1,7)
      if (length(intersect(unlist(availDates),monthsNeeded))==0) {
        logger.warn("No data available in year ",current_year)
        next()
      }
      startMon <- min(monthsNeeded)
      endMon <- max(monthsNeeded)
  
  #Set up netcdf file, dimensions, and sequence of dates
    new.file <- all_files[y_idx]
    if (file.exists(new.file) && !overwrite) {
      logger.debug("File '", new.file, "' already exists, skipping.")
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
    FillValue = NA
  
  #STEPS: Download all months in startdate to enddate for given variable
  # Read CSV, copy to array, add to NetCDF file

  #TEMPERATURE NEON.DP1.00002 Air Temp profile or NEON.DP1.00003 Triple-aspirated T (preferred)
    airTempLoc = grep("DP1.00002",availProducts)
    airTemp3Loc = grep("DP1.00003",availProducts)
    if (length(airTempLoc)!=0) { 
      airTempDates = unlist(availDates[airTempLoc[1]])
      airTempGoodDates = which((airTempDates >= startMon) & (airTempDates <= endMon))
      nairTemp = length(airTempGoodDates)
      } else { nairTemp = 0 }
    if (length(airTemp3Loc)!=0) { 
      airTemp3Dates = unlist(availDates[airTemp3Loc[1]])
      airTemp3GoodDates = which((airTemp3Dates >= startMon) & (airTemp3Dates <= endMon))
      nairTemp3 = length(airTemp3GoodDates)
      } else { nairTemp3 = 0 }
    if ((length(airTempLoc)==0) && (length(airTemp3Loc)==0)) {
      PEcAn.utils::logger.error("Air temperature DP1.00002 or DP1.00003 not available") }
    if ((nairTemp==0) && (nairTemp3==0)) {
      PEcAn.utils::logger.error("Air temperature DP1.00002 or DP1.00003 not available in date range ",startMon," ",endMon)
    }
  
  #define NetCDF variable and create NetCDF file
    airT.var <- ncvar_def(name = "air_temperature", units = "K", dim = xytdim)
    nc <- ncdf4::nc_create(new.file, vars = airT.var)  #create netCDF file
    ncdata <- rep(FillValue,length(datetime))
    if (nairTemp3>nairTemp) {
      for (mon in airTemp3Dates[airTemp3GoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[airTemp3Loc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
          #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$tempTripleMean
          csvQF <- csvData$finalQF
          csvVar[which(csvQF!=1)]=NA  #triple-asp temp always has QF of 1
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"celsius", "K")
        }
      }
    } else {
      for (mon in airTempDates[airTempGoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[airTempLoc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
          #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$tempSingleMean
          csvQF <- csvData$finalQF
          csvVar[which(csvQF!=0)]=NA
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"celsius", "K")
        }
      }
    }
    ncdf4::ncvar_put(nc, varid = airT.var, vals = ncdata)

    # NEON.DP1.00004 Pressure
    pSurfLoc = grep("DP1.00004",availProducts)
    if (length(pSurfLoc)!=0) { 
      pSurfDates = unlist(availDates[pSurfLoc[1]])
      pSurfGoodDates = which((pSurfDates >= startMon) & (pSurfDates <= endMon))
      npSurf = length(pSurfGoodDates)
    } else {npSurf=0}
    if (npSurf>0) {
      Psurf.var <- ncdf4::ncvar_def(name = "air_pressure", units = "Pa", dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = Psurf.var, verbose = verbose)
      ncdata <- rep(FillValue,length(datetime))
      for (mon in pSurfDates[pSurfGoodDates]) {
          neonData <- nneo::nneo_data(product_code = availProducts[pSurfLoc[1]], site_code = site, year_month = mon)
          if (length(neonData$data$urls)>0) {
            #Extract and read 30 minute data from the highest vertical level among files returned
            url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
            csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
            #Retreive time dimension and figure out where in array to put it
            csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
            arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
            csvVar <- csvData$staPresMean
            csvQF <- csvData$staPresFinalQF
            csvVar[which(csvQF!=0)]=NA
            ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"kPa", "Pa")
          }
      }
      ncdf4::ncvar_put(nc, varid = Psurf.var, vals = ncdata)
    }
      
    # NEON.DP1.00024 PAR
    PARLoc = grep("DP1.00024",availProducts)
    if (length(PARLoc)!=0) { 
      PARDates = unlist(availDates[PARLoc[1]])
      PARGoodDates = which((PARDates >= startMon) & (PARDates <= endMon))
      nPAR = length(PARGoodDates)
    } else {nPAR=0}
    if (nPAR>0) {
      PAR.var <- ncdf4::ncvar_def(name = "surface_downwelling_photosynthetic_photon_flux_in_air", 
                                  units = "mol m-2 s-1", 
                                  dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = PAR.var, verbose = verbose)
      ncdata <- rep(FillValue,length(datetime))
      for (mon in PARDates[PARGoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[PARLoc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
          #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$PARMean
          csvQF <- csvData$PARFinalQF
          csvVar[which(csvQF!=0)]=NA
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"umol m-2 s-1", "mol m-2 s-1")
        }
      }
      ncdf4::ncvar_put(nc, varid = PAR.var, vals = ncdata)
    }
    
    # NEON.DP1.00006 Precip (missing uncertainty information)
    precipLoc = grep("DP1.00006",availProducts)
    if (length(precipLoc)!=0) { 
      precipDates = unlist(availDates[precipLoc[1]])
      precipGoodDates = which((precipDates >= startMon) & (precipDates <= endMon))
      nprecip = length(precipGoodDates)
    } else {nprecip=0}
    if (nprecip>0) {
      precip.var <- ncdf4::ncvar_def(name = "precipitation_flux",
                                     units = "kg m-2 s-1", 
                                     dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = precip.var, verbose = verbose)
      ncdata <- rep(FillValue,length(datetime))
      for (mon in precipDates[precipGoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[precipLoc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
          #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$secPrecipBulk #in mm per half hour
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar/1800,"kg m-2 s-1", "kg m-2 s-1")
        }
      }
      ncdf4::ncvar_put(nc, varid = precip.var, vals = ncdata)
    }
    
    # NEON.DP1.00098 RH
    RHLoc = grep("DP1.00098",availProducts)
    if (length(RHLoc)!=0) { 
      RHDates = unlist(availDates[RHLoc[1]])
      RHGoodDates = which((RHDates >= startMon) & (RHDates <= endMon))
      nRH = length(RHGoodDates)
    } else {nRH=0}
    if (nRH>0) {
      RH.var <- ncdf4::ncvar_def(name = "relative_humidity", 
                                  units = "%", 
                                  dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = RH.var, verbose = verbose)
      ncdata <- rep(FillValue,length(datetime))
      for (mon in RHDates[RHGoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[RHLoc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
          #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$RHMean
          csvQF <- csvData$RHFinalQF
          csvVar[which(csvQF!=0)]=NA
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"%", "%")
        }
      }
      ncdf4::ncvar_put(nc, varid = RH.var, vals = ncdata)
    }
    
    # NEON.DP1.00022 SW or DP1.00023 SW/LW (or DP1.00014 for Direct/Diffuse SW)
    SWLoc = grep("DP1.00022",availProducts)
    SWLWLoc = grep("DP1.00023",availProducts)
    if (length(SWLoc)!=0) { 
      SWDates = unlist(availDates[SWLoc[1]])
      SWGoodDates = which((SWDates >= startMon) & (SWDates <= endMon))
      nSW = length(SWGoodDates)
    } else { nSW = 0 }
    if (length(SWLWLoc)!=0) { 
      SWLWDates = unlist(availDates[SWLWLoc[1]])
      SWLWGoodDates = which((SWLWDates >= startMon) & (SWDates <= endMon))
      nSWLW = length(SWLWGoodDates)
    } else { nSWLW = 0 }

    if (nSWLW!=0) {
      SW.var <- ncvar_def(name = "surface_downwelling_shortwave_flux_in_air", 
                          units = "W m-2", 
                          dim = xytdim)
      nc <- ncdf4::ncvar_add(nc = nc, v = SW.var, verbose = verbose)
      LW.var <- ncvar_def(name = "surface_downwelling_longwave_flux_in_air",
                          units = "W m-2", 
                          dim = xytdim)
      nc <- ncvar_add(nc = nc, v = LW.var, verbose = verbose)
      ncdata <- rep(FillValue,length(datetime))
      ncdata2 <- rep(FillValue,length(datetime))
      for (mon in SWLWDates[SWLWGoodDates]) {
        neonData <- nneo::nneo_data(product_code = availProducts[SWLWLoc[1]], site_code = site, year_month = mon)
        if (length(neonData$data$urls)>0) {
           #Extract and read 30 minute data from the highest vertical level among files returned
          url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
          csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
          #Retreive time dimension and figure out where in array to put it
          csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
          arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
          csvVar <- csvData$inSWMean
          csvQF <- csvData$inSWFinalQF
          csvVar[which(csvQF!=0)]=NA  
          ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"W m-2", "W m-2")
          csvVar <- csvData$inLWMean
          csvQF <- csvData$inLWFinalQF
          csvVar[which(csvQF!=0)]=NA  
          ncdata2[arrLoc] <- udunits2::ud.convert(csvVar,"W m-2", "W m-2")
        }
      }
      ncdf4::ncvar_put(nc, varid = SW.var, vals = ncdata)
      ncdf4::ncvar_put(nc, varid = LW.var, vals = ncdata2)
    } else {
      if (nSW!=0) {
        SW.var <- ncvar_def(name = "surface_downwelling_shortwave_flux_in_air", 
                            units = "W m-2", 
                            dim = xytdim)
        nc <- ncdf4::ncvar_add(nc = nc, v = SW.var, verbose = verbose)
        ncdata <- rep(FillValue,length(datetime))
        for (mon in SWDates[SWGoodDates]) {
          neonData <- nneo::nneo_data(product_code = availProducts[SWLoc[1]], site_code = site, year_month = mon)
          if (length(neonData$data$urls)>0) {
            #Extract and read 30 minute data from the highest vertical level among files returned
            url30 <- tail(sort(neonData$data$urls[grep("*.00000.000.*_30min",neonData$data$urls)]),1)
            csvData <- read.csv(url30,stringsAsFactors=FALSE,header=TRUE)
            #Retreive time dimension and figure out where in array to put it
            csvDateTime <- as.POSIXct(gsub("T"," ",csvData$startDateTime),tz="UTC")
            arrLoc <- floor(as.numeric(difftime(csvDateTime,datetime[1],tz="UTC",units="hours"))*2)+1
            csvVar <- csvData$shortRadMean
            csvQF <- csvData$finalQF
            csvVar[which(csvQF!=0)]=NA
            ncdata[arrLoc] <- udunits2::ud.convert(csvVar,"W m-2", "W m-2")
          }
        }
        ncdf4::ncvar_put(nc, varid = SW.var, vals = ncdata)
      }
    }
  
    # NEON.DP1.00001 2D wind speed/direction - have to do northward/eastward math
    
    # NEON.DP1.00041 Soil temp    (take 2cm level which is level 501)
    
    # NEON.DP1.00034 CO2 at tower top (alt NEON.DP3.00009 CO2 profile) - not yet avail
    
  ncdf4::nc_close(nc)
  } #For loop
  return(invisible(results))
} #function

#Description of file name convention
  # http://data.neonscience.org:80/api/v0/data/DP1.00098.001/HEAL/2016-05/NEON.D19.HEAL.DP1.00098.001.00000.003.000.030.RH_30min.csv?package=basic
  #NEON denotes the organizational origin of the data product and identifies the product as operational; data collected as part of a special data collection exercise are designated by a separate, unique alphanumeric code created by the PI
  #DOM is a three-character alphanumeric code, referring to the domain of data acquisition (D01 � D20)
  #SITE is four-character alphanumeric code, referring to the site of data acquisition; all sites are designated by a standardized four-character alphabetic code
  #DPL is a three-character alphanumeric code, referring to data product processing level
  #PRNUM is a five-character numeric code, referring to the data product number (see the Data Product Catalog at http://data.neonscience.org/data-product-catalog)
  #REV is a three-digit designation, referring to the revision number of the data product; it is also used to track �Data Product Maturity�, from Engineering �Grade/Provisional to Science-Grade (001 = initial REV, Engineering-Grade or Provisional; 101 = initial REV, Science-Grade)
  #TERMS is a five-digit designation used in data product numbering to identify a sub-product or discrete vector of metadata. Since each download file typically contains several sub-products, this field is set to 00000 in the file name to maintain consistency with the data product numbering scheme.
  #HOR is the Spatial Index #1 and refers to measurement locations within one horizontal plane. For example, if five surface measurements were taken, one at each of the five soil array plots, the number in the HOR field would range from 001-005.
  #VER is the Spatial Index #2 and refers to measurement locations within one vertical plane. For example, if eight temperature measurements are collected, one at each tower level, the number in the VER field would range from 010-080.
  #TMI is the Temporal Index; it is a three-digit designation and refers to the temporal representation, averaging period, or coverage of the data product (e.g., minute, hour, month, year, sub-hourly, day, lunar month, single instance, seasonal, annual, multi-annual)
  #DESC is an abbreviated description of the data product 
  
  #VER - highest number = highest level
  #Default take HOR 001 VER (max)

  # FUTURE VARIABLES THAT WILL MAKE IT EASIER/ALLOW FOR GAP FILLING
  #   NEON.DP4.00001 summary weather stats (days/months/years)
  #   NEON.DP2.00023 gap-filled air T, NEON.DP2.00007 air T profile gap filled
  #   NEON.DP2.00008 gap=filled CO2 conc
  #   NEON.DP2.00019 gap-filled SW direct/diffuse
  #   NEON.DP2.00009 gap-filled H2O conc
  #   NEON.DP2.00005 gap-filled PAR
  #   NEON.DP2.00021 gap-filled SW 
  #   NEON.DP2.00020 gap-filled LW/SW
  #   NEON.DP2.00006 gap-filled soil T  