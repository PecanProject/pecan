##' @name met2CF.csv
##' @title met2CF.csv
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param format data frame or list with elements as described below
##' format is output from db/R/query.format.vars, and should have:
##'   REQUIRED:
##'   format$lat = latitude of site (unless passed by lat)
##'   format$lon = longitude of site (unless passed by lon)
##'   format$header = number of lines of header
##'   format$vars is a data.frame with lists of information for each variable to read, at least airT is required
##'     format$vars$orig_name = Name in CSV file
##'     format$vars$orig_units = Units in CSV file
##'     format$vars$bety_name = Name in BETY - see https://pecan.gitbooks.io/pecan-documentation/content/developers_guide/Adding-an-Input-Converter.html for allowable ones
##'   OPTIONAL:
##'   format$na.strings = list of missing values to convert to NA, such as -9999
##'   format$skip = lines to skip excluding header
##'   format$vars$column_number = Column number in CSV file (optional, will use header name first)
##' Columns with NA for bety variable name are dropped. 
##' Units for datetime field are the lubridate function that will be used to parse the date (e.g. \code{ymd_hms} or \code{mdy_hm}). 
##' @param nc_verbose logical: run ncvar_add in verbose mode?
##' @export
##' @author Mike Dietze, David LeBauer, Ankur Desai
##' @examples
##' \dontrun{
##' library(PEcAn.DB)
##' library(lubridate)
##' library(RPostgreSQL)
##' bety = list(user="bety", password="bety", host="localhost", dbname="bety", driver="PostgreSQL", write=TRUE)
##' con <- db.open(bety)
##' in.path <- '/home/carya/sites/willow/'
##' in.prefix <- 'FLX_US-WCr_FLUXNET2015_SUBSET_HH_1999-2014_1-1'
##' outfolder <- '~/'
##' input.id <- 5000000005
##' format <- query.format.vars(input.id=input.id,con)
##' start_date <- ymd_hm('200401010000')
##' end_date <- ymd_hm('200412312330')
##' PEcAn.data.atmosphere::met2CF.csv(in.path,in.prefix,outfolder,start_date,end_date,format,overwrite=TRUE)
##' }
met2CF.csv <- function(in.path, in.prefix, outfolder, start_date, end_date, format, lat=NULL, lon=NULL, nc_verbose = FALSE, overwrite=FALSE, ...){
  require(PEcAn.utils)
  require(lubridate)
  require(udunits2)  
  require(ncdf4)
  
  start_year <- year(start_date)
  end_year <- year(end_date)
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  ## set up results output to return
  rows <- end_year - start_year + 1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  
  in.file <- paste0(in.prefix,'.csv')
  files <- dir(in.path, in.file, full.names = TRUE)
  files <- files[grep("*.csv",files)]
  if(length(files)==0){
    return(NULL)
    logger.warn("No met files named ", in.file, "found in ", in.path)
  }
  files <- files[1]
  
  # get lat/lon from format.vars if not passed directly
  if (missing(lat) || is.null(lat)) { lat <- format$lat }
  if (missing(lon) || is.null(lon)) { lon <- format$lon }
  
  #create new filename by swapping .csv with .nc, and adding year strings from start to end year
  #year(start_date)
  all_years <- start_year:end_year
  all_files <- paste0(file.path(outfolder,gsub(".csv","",basename(files))),".",as.character(all_years),".nc")
  
  results$file <- all_files
  results$host <- fqdn()
  results$startdate <- paste0(all_years,"-01-01 00:00:00")
  results$enddate <- paste0(all_years,"-12-31 23:59:59")
  results$mimetype <- 'application/x-netcdf'
  results$formatname <- 'CF'
  
  #If all the files already exist, then skip the conversion unless overwrite=TRUE
  if (!overwrite && all(file.exists(all_files))) {
    logger.debug("File '", all_files, "' already exist, skipping to next file.")
    next
  }
  #If some of the files already exist, skip those, but still need to read file
  if (!overwrite && any(file.exists(all_files))) {
    logger.debug("Files ",all_files[which(file.exists(all_files))]," already exist, skipping those")
    all_years <- all_years[which(!file.exists(all_files))]
    all_files <- all_files[which(!file.exists(all_files))]
  }
  
  ## Read the CSV file
  ## some files have a line under the header that lists variable units
  ## search for NA's after conversion to numeric
  if(is.null(format$header)){
    logger.warn("please specify number of header rows in file")
    alldat <- read.csv(files, skip = format$skip, na.strings = format$na.strings, as.is=TRUE, 
                       check.names = FALSE)
  } else if (format$header == 0 | format$header == 1){
    alldat <- read.csv(files, skip = format$skip, na.strings = format$na.strings, as.is=TRUE,
                       check.names = FALSE, header = as.logical(format$header))
  }else if (format$header > 1) {
    alldat <- read.csv(files, skip = format$skip, na.strings = format$na.strings, as.is=TRUE, 
                       check.names = FALSE, header = TRUE)
    alldat <- alldat[-c(1:header-1),]
  }
  
  ## Get datetime vector - requires one column be connected to bety variable datetime
  ## FUTURE: Make this much more generic to deal with multiple ways datetime can be passed in a CSV such as Year,Month,Day, and so on
  datetime_index <- which(format$vars$bety_name == "datetime")
  if (length(datetime_index)==0) { logger.error("datetime column is not specified in format") }
  datetime_units <- format$vars$orig_units[datetime_index] #lubridate function to call such as ymd_hms
  if (datetime_units=="") { datetime_units <- "ymd_hm" }
  datetime_raw <- alldat[, format$vars$orig_name[datetime_index]]
  alldatetime <- do.call(datetime_units, list(datetime_raw))  #convert to POSIXct convention
  ## and remove datetime from 'dat' dataframe
  ##dat[, datetime_index] <- format$na.strings    
  
  ## Only run if years > start_date < end_date,  if both are provided, clip data to those dates
  ## Otherwise set start/end to first/last datetime of file
  years = year(alldatetime)  
  if (!missing(start_date) && !missing(end_date)) {
    availdat <- which(years >= year(start_date) & years <= year(end_date))
    if (length(availdat)==0) { logger.error("data does not contain output after start_date or before end_date")}
    alldat <- alldat[availdat, ]
    alldatetime <- alldatetime[availdat]
    years <- years[availdat]
  } else {
    start_date = alldatetime[1]
    end_date = alldatetime[length(alldatetime)]
  }
  ## convert data to numeric - not needed and is slow
  #dat <- as.data.frame(datetime = datetime, sapply(dat[,-datetime_index], as.numeric))
  
  ## loop over years that need to be read

  for (i in 1:length(all_years)) {
    
    ## Test that file has data for year being processed, else move on
    this.year <- all_years[i]
    availdat.year <- which(years==this.year)
    if (length(availdat.year)==0) {
      logger.debug("File ",in.file," has no data for year ",this.year)
      next
    }
    new.file <- all_files[i]
    
    ## Extract that year's data from large file
    dat <- alldat[availdat.year,]
    datetime <- alldatetime[availdat.year]
    
    ### create time dimension 
    days_since_1700 <- datetime - ymd("1700-01-01")
    t <- ncdim_def("time", "days since 1700-01-01", as.numeric(days_since_1700)) #define netCDF dimensions for variables
    timestep <- as.numeric(mean(ud.convert(diff(days_since_1700), "d", "s")))
    
    ## create lat lon dimensions
    x <- ncdim_def("longitude", "degrees_east", lon) #define netCDF dimensions for variables
    y <- ncdim_def("latitude", "degrees_north", lat)
    xytdim <- list(x,y,t)
    
    ## airT (celsius) => air_temperature (K) - REQUIRED for all met files
    locs <- which(format$vars$bety_name %in% "airT")
    if (length(locs)>0) {
      k <- locs[1]
      airT.var <- ncvar_def(name="air_temperature",units = "K",dim = xytdim)
      nc <- nc_create(new.file, vars = airT.var) #create netCDF file
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for airT by name or column number")
        }
      }
      ncvar_put(nc, varid = airT.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"celsius","K"))  
    } else { logger.error("No air temperature found in met file") }
    
    ## air_pressure (Pa) => air_pressure (Pa)
    locs <- which(format$vars$bety_name %in% "air_pressure")
    if (length(locs)>0) {
      k <- locs[1]
      Psurf.var <- ncvar_def(name="air_pressure",units = "Pa",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = Psurf.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for air_pressure by name or column number")
        }
      }
      ncvar_put(nc, varid = Psurf.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"Pa","Pa"))  
    }
    
    ## co2atm (umol/mol) => mole_fraction_of_carbon_dioxide_in_air (mol/mol) 
    locs <- which(format$vars$bety_name %in% "co2atm")
    if (length(locs)>0) {
      k <- locs[1]
      CO2.var <- ncvar_def(name="mole_fraction_of_carbon_dioxide_in_air",units = "mol mol-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = CO2.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for co2atm by name or column number")
        }
      }
      ncvar_put(nc, varid = CO2.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"umol mol-1","mol mol-1"))  
    }    
    
    ## soilM (%) => volume_fraction_of_condensed_water_in_soil (%)
    locs <- which(format$vars$bety_name %in% "soilM")
    if (length(locs)>0) {
      k <- locs[1]
      soilM.var <- ncvar_def(name="volume_fraction_of_condensed_water_in_soil",units = "1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = soilM.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for soilM by name or column number")
        }
      }
      ncvar_put(nc, varid = soilM.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"1","1"))  
    }
    
    ## soilT (celsius) => soil_temperature (K)
    locs <- which(format$vars$bety_name %in% "soilT")
    if (length(locs)>0) {
      k <- locs[1]
      soilT.var <- ncvar_def(name="soil_temperature",units = "K",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = soilT.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for soilT by name or column number")
        }
      }
      ncvar_put(nc, varid = soilT.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"celsius","K"))  
    }
    
    ## relative_humidity (%) => relative_humidity (%)
    locs <- which(format$vars$bety_name %in% "relative_humidity")
    if (length(locs)>0) {
      k <- locs[1]
      RH.var <- ncvar_def(name="relative_humidity",units = "%",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = RH.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for relative_humidity by name or column number")
        }
      }
      ncvar_put(nc, varid = RH.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"%","%"))  
    }
    
    ## specific_humidity (g g-1) => specific_humidity (kg kg-1)
    locs <- which(format$vars$bety_name %in% "specific_humidity")
    if (length(locs)>0) {
      k <- locs[1]
      qair.var <- ncvar_def(name="specific_humidity",units = "kg kg-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = qair.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for specific_humidity by name or column number")
        }
      }
      ncvar_put(nc, varid = qair.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"g g-1","kg kg-1"))  
    } else {
      ## file needs to be closed and re-opened to access added variables
      nc_close(nc)
      nc = nc_open(new.file,write=TRUE,readunlim=FALSE)
      if("relative_humidity" %in% names(nc$var) & "air_temperature" %in% names(nc$var)){
        ## Convert RH to SH
        qair = rh2qair(rh=ncvar_get(nc,"relative_humidity")/100,T=ncvar_get(nc,"air_temperature"))
        qair.var = ncvar_def(name="specific_humidity",units="kg kg-1",dim=xytdim)
        nc = ncvar_add(nc = nc, v = qair.var, verbose = nc_verbose) #add variable to existing netCDF file
        ncvar_put(nc, varid = 'specific_humidity',vals=qair)
      }
    }
    
    ## VPD (Pa) => water_vapor_saturation_deficit (Pa)
    locs <- which(format$vars$bety_name %in% "VPD")
    if (length(locs)>0) {
      k <- locs[1]
      VPD.var <- ncvar_def(name="water_vapor_saturation_deficit",units = "Pa",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = VPD.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for VPD by name or column number")
        }
      }
      ncvar_put(nc, varid = VPD.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"Pa","Pa"))  
    }
    
    ## surface_downwelling_longwave_flux_in_air (W m-2) => surface_downwelling_longwave_flux_in_air (W m-2)
    locs <- which(format$vars$bety_name %in% "surface_downwelling_longwave_flux_in_air")
    if (length(locs)>0) {
      k <- locs[1]
      LW.var <- ncvar_def(name="surface_downwelling_longwave_flux_in_air",units = "W m-2",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = LW.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for surface_downwelling_longwave_flux_in_air by name or column number")
        }
      }
      ncvar_put(nc, varid = LW.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"W m-2","W m-2"))  
    }
    
    ## solar_radiation (W m-2) => surface_downwelling_shortwave_flux_in_air (W m-2)
    locs <- which(format$vars$bety_name %in% "solar_radiation")
    if (length(locs)>0) {
      k <- locs[1]
      SW.var <- ncvar_def(name="surface_downwelling_shortwave_flux_in_air",units = "W m-2",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = SW.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for solar_radiation by name or column number")
        }
      }
      ncvar_put(nc, varid = SW.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"W m-2","W m-2"))  
    }
    
    ## PAR (umol m-2 s-1) => surface_downwelling_photosynthetic_photon_flux_in_air (mol m-2 s-1)
    locs <- which(format$vars$bety_name %in% "PAR")
    if (length(locs)>0) {
      k <- locs[1]
      PAR.var <- ncvar_def(name="surface_downwelling_photosynthetic_photon_flux_in_air",units = "mol m-2 s-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = PAR.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for PAR by name or column number")
        }
      }
      ncvar_put(nc, varid = PAR.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"umol m-2 s-1","mol m-2 s-1"))  
    }
    
    ## precipitation_flux (kg m-2 s-1) => precipitation_flux (kg m-2 s-1)
    locs <- which(format$vars$bety_name %in% "precipitation_flux")
    if (length(locs)>0) {
      k <- locs[1]
      precip.var <- ncvar_def(name="precipitation_flux",units = "kg m-2 s-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = precip.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for precipitation_flux by name or column number")
        }
      }
      rain <- dat[,arrloc]
      rain.units <- as.character(format$vars$orig_units[k])
      rain.units <- switch(rain.units,
                           mm = {rain = rain / timestep; "kg m-2 s-1"},
                           m  = {rain = rain / timestep; "Mg m-2 s-1"},
                           'in' = {rain=ud.convert(rain / timestep, "in", "mm"); "kg m-2 s-1"},
                           'mm h-1' = {rain = ud.convert(rain / timestep, "h", "s"); "kg m-2 s-1"})        
      ncvar_put(nc, varid = precip.var,
                vals = met.conv(rain, rain.units, "kg m-2 s-1", "kg m-2 s-1"))  
    }
    
    ## eastward_wind (m s-1) => eastward_wind (m s-1)
    ## northward_wind (m s-1) => northward_wind (m s-1)
    if (("eastward_wind" %in% format$vars$bety_name) & ("northward_wind" %in% format$vars$bety_name)) {
      locs <- which(format$vars$bety_name %in% "northward_wind")
      k <- locs[1]
      Nwind.var <- ncvar_def(name="northward_wind",units = "m s-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = Nwind.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for eastward_wind by name or column number")
        }
      }
      ncvar_put(nc, varid = Nwind.var,
                vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"m s-1","m s-1"))  
      locs <- which(format$vars$bety_name %in% "eastward_wind")
      k <- locs[1]
      Ewind.var <- ncvar_def(name="eastward_wind",units = "m s-1",dim = xytdim)
      nc <- ncvar_add(nc = nc, v = Ewind.var, verbose = nc_verbose)
      arrloc <- as.character(format$vars$orig_name[k])
      if (arrloc=="") {
        if (any(colnames(format$vars)=="column_number")) { 
          arrloc <- format$vars$column_number[k]
        } else {
          logger.error("Cannot find column location for northward_wind by name or column number")
        }
        ncvar_put(nc, varid = Ewind.var,
                  vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"m s-1","m s-1"))  
      }      
    } else {
      locs_wd <- which(format$vars$bety_name %in% "wind_direction")
      locs_ws <- which(format$vars$bety_name %in% "Wspd")
      if (length(locs_wd)>0 & length(locs_ws)>0) {
        #wind speed and direction available, convert to northward and eastward
        k_wd = locs_wd[1]
        arrloc_wd <- as.character(format$vars$orig_name[k_wd])
        if (arrloc_wd=="") {
          if (any(colnames(format$vars)=="column_number")) { 
            arrloc_wd <- format$vars$column_number[k_wd]
          } else {
            logger.error("Cannot find column location for wind_direction by name or column number")
          }
        }
        arrloc_ws <- as.character(format$vars$orig_name[k_ws])
        if (arrloc_ws=="") {
          if (any(colnames(format$vars)=="column_number")) { 
            arrloc_ws <- format$vars$column_number[k_ws]
          } else {
            logger.error("Cannot find column location for wind_speed by name or column number")
          }
        }        
        wind <- met.conv(dat[,arrloc_ws],format$vars$orig_units[k_ws],"m s-1","m s-1")
        wind_direction <- met.conv(dat[,arrloc_wd],format$vars$orig_units[k_wd],"radians","radians")
        uwind <- wind*cos(wind_direction)
        vwind <- wind*sin(wind_direction)
        Ewind.var <- ncvar_def(name="eastward_wind",units = "m s-1",dim = xytdim)
        nc <- ncvar_add(nc = nc, v = Ewind.var, verbose = nc_verbose)
        Nwind.var <- ncvar_def(name="northward_wind",units = "m s-1",dim = xytdim)
        nc <- ncvar_add(nc = nc, v = Nwind.var, verbose = nc_verbose)
        ncvar_put(nc, varid = Ewind.var,vals=uwind)  
        ncvar_put(nc, varid = Nwind.var,vals=vwind)  
      } else {
        #no wind direction is present, just insert wind_speed
        ## Wspd (m s-1) => wind_speed (m s-1)
        locs <- which(format$vars$bety_name %in% "Wspd")
        if (length(locs)>0) {
          k <- locs[1]
          Wspd.var <- ncvar_def(name="wind_speed",units = "m s-1",dim = xytdim)
          nc <- ncvar_add(nc = nc, v = Wspd.var, verbose = nc_verbose)
          arrloc <- as.character(format$vars$orig_name[k])
          if (arrloc=="") {
            if (any(colnames(format$vars)=="column_number")) { 
              arrloc <- format$vars$column_number[k]
            } else {
              logger.error("Cannot find column location for Wspd by name or column number")
            }
          }
          ncvar_put(nc, varid = Wspd.var,
                    vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"m s-1","m s-1"))  
        }
      }
    }  ## end wind 
    
    #       ## wind_direction (degrees) => wind_from_direction (degrees) 
    #       locs <- which(format$vars$bety_name %in% "wind_direction")
    #       if (length(locs)>0) {
    #         k <- locs[1]
    #         Wdir.var <- ncvar_def(name="wind_from_direction",units = "degrees",dim = xytdim)
    #         nc <- ncvar_add(nc = nc, v = Wdir.var, verbose = nc_verbose)
    #         arrloc <- as.character(format$vars$orig_name[k])
    #         if (arrloc=="") {
    #           if (any(colnames(format$vars)=="column_number")) { 
    #             arrloc <- format$vars$column_number[k]
    #           } else {
    #             logger.error("Cannot find column location for wind_direction by name or column number")
    #           }
    #         }
    #         ncvar_put(nc, varid = Wdir.var,
    #                   vals=met.conv(dat[,arrloc],format$vars$orig_units[k],"degrees","degrees"))  
    #       } 
    
    nc_close(nc)
  } ## end loop over years
  invisible(results)
} ## end function met2CF.csv


datetime <- function(list){
  date_string <- sapply(list,as.character)
  datetime = paste(list,"00")
  datetime = ymd_hms(datetime)
  return(datetime)
  
}

met.conv <- function(x,orig,bety,CF){
  orig = as.character(orig)
  bety = as.character(bety)
  CF   = as.character(CF)
  if(nchar(orig) == 0) orig = bety; ## if units not provided, default is that they were the same units as bety
  if(ud.is.parseable(orig)){
    if(ud.are.convertible(orig,bety)){
      return(ud.convert(ud.convert(x,orig,bety),bety,CF))
    } else {
      logger.error(paste("met.conv could not convert",orig,bety,CF))
    }
  } else {
    logger.error(paste("met.conv could not parse units:",orig),"Please check if these units conform to udunits")
  }
}
