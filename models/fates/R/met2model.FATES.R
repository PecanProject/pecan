#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# R Code to convert NetCDF CF met files into NetCDF FATES met files.

##' met2model wrapper for FATES
##' 
##' @title met2model for FATES
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbosefor(year in start_year:end_year)
##' @importFrom ncdf4 ncvar_get ncdim_def ncatt_get ncvar_put
library(ncdf4)
met2model.FATES <- function(in.path,in.prefix,outfolder,start_date,end_date,lst=0,lat, lon, overwrite, verbose) {
#in.path=, in.prefix, outfolder, start_date, end_date, lst = 0, lat, lon, 
                            
  
  # General Structure- FATES Uses Netcdf so we need to rename vars, split files from years into months, and generate the header file
  # Get Met file from inpath.
  # Loop over years (Open nc.file,rename vars,change dimensions as needed,close/save .nc file)
  # close
  # defining temporal dimension needs to be figured out. If we configure FATES to use same tstep then we may not need to change dimensions  
  
  
  insert <- function(ncout, name, unit, data, dim) {
    var  <- ncdf4::ncvar_def(name, unit, dim = dim, missval = as.numeric(1.0e36), verbose = verbose)
    ncout <- ncdf4::ncvar_add(ncout, var)
    ncvar_put(nc = ncout, varid = name, vals = data)
    return(invisible(ncout))
  }
  
  
  ## Create output directory
  if (!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  # Process start and end dates
  start_date <- as.POSIXlt(start_date, tz = "UTC", origin="2016-01-01" ) #origin should be settled down first
  end_date   <- as.POSIXlt(end_date, tz = "UTC", origin="2016-01-01" )
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  sm <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365) + difftime(start_date,"1700-01-01", units="days")  ## day of year thresholds

  ## Build met
  for (year in start_year:end_year) {
    
    in.file <- file.path(in.path, paste(in.prefix, year, "nc", sep = "."))
    
    if (file.exists(in.file)) {
      
      ## Open netcdf file
      nc <- ncdf4::nc_open(in.file)
      
      ## extract variables. These need to be read in and converted to CLM names (all units are correct)
      time      <- ncvar_get(nc, "time")
      LATIXY    <- ncvar_get(nc, "latitude")
      LONGXY    <- ncvar_get(nc, "longitude")
      FLDS      <- ncvar_get(nc, "surface_downwelling_longwave_flux_in_air")  ## W/m2
      FSDS      <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air")  ## W/m2
      PRECTmms  <- ncvar_get(nc, "precipitation_flux")  ## kg/m2/s -> mm/s (same val, diff name)
      PSRF      <- ncvar_get(nc, "air_pressure")  ## Pa
      QBOT      <- ncvar_get(nc, "specific_humidity")  ## g/g -> kg/kg
      TBOT      <- ncvar_get(nc, "air_temperature")  ## K
      WIND      <- sqrt(ncvar_get(nc, "eastward_wind") ^ 2 + ncvar_get(nc, "northward_wind") ^ 2)  ## m/s
      
      ## CREATE MONTHLY FILES
      for (mo in 1:12) {
        # slice
        tsel <- which(time > sm[mo] & time <= sm[mo + 1])
        # define dim
        lat.dim  <- ncdim_def(name = "lat", units = "", vals = 1:1, create_dimvar=FALSE)
        lon.dim  <- ncdim_def(name = "lon", units = "", vals = 1:1, create_dimvar=FALSE)
        time.dim <- ncdim_def(name = "time", units = "", vals = 1:length(time[tsel]),create_dimvar = TRUE, calendar="standard", unlim = FALSE) #left to CTSM automatically transfer
        scalar.dim <- ncdim_def(name="scalar", units = "", vals = 1:1)
        dim      <- list(time.dim, lat.dim, lon.dim)  
        
        # LATITUDE
        var_lat <- ncdf4::ncvar_def(name = "LATIXY", units = "degree_north", 
                         dim = list(lat.dim, lon.dim), missval = as.numeric(-9999))
        # LONGITUDE
        var_long <- ncdf4::ncvar_def(name = "LONGXY", units = "degree_east",
                         dim = list(lat.dim, lon.dim), missval = as.numeric(-9999))
        # time
        var_time <- ncdf4::ncvar_def(name = "time", units = "days since 1700-01-01", prec = "float",
                         dim = list(time.dim), missval = as.numeric(-9999))
        # EDGEE
        var_E <- ncdf4::ncvar_def(name = "EDGEE", units = "degrees_east",
                         dim = list(scalar.dim, lat.dim, lon.dim), missval = as.numeric(-9999))
        # EDGEW edge for resolution , edge-central 0.005, # PEcAn provide range of grid?
        var_W <- ncdf4::ncvar_def(name = "EDGEW", units = "degrees_west",
                         dim = list(scalar.dim, lat.dim, lon.dim), missval = as.numeric(-9999))
        # EDGES
        var_S <- ncdf4::ncvar_def(name = "EDGES", units = "degrees_south",
                         dim = list(scalar.dim, lat.dim, lon.dim), missval = as.numeric(-9999))
        # EDGEN
        var_N <- ncdf4::ncvar_def(name = "EDGEN", units = "degrees_north",
                         dim = list(scalar.dim, lat.dim, lon.dim), missval = as.numeric(-9999))
        
        ## SAPERATELY CREATE FILES
        put_var <- function(ncout){
          ncvar_put(nc = ncout, varid = "LATIXY", vals = LATIXY) #same with FATES
          ncvar_put(nc = ncout, varid = "LONGXY", vals = LONGXY)
          ncvar_put(nc = ncout, varid = "EDGEE", vals = LONGXY+0.005)
          ncvar_put(nc = ncout, varid = "EDGEW", vals = LONGXY-0.005)
          ncvar_put(nc = ncout, varid = "EDGES", vals = LATIXY-0.005)
          ncvar_put(nc = ncout, varid = "EDGEN", vals = LATIXY+0.005)
          #ncvar_put(nc = ncout, varid = "time", vals = time[tsel] )
        }
        # precipitation
        outfile_prec <- file.path(outfolder, paste0("Prec", formatC(year, width = 4, flag = "0"), "-",
                                               formatC(mo, width = 2, flag = "0"), ".nc")) 
        if (file.exists(outfile_prec) & overwrite == FALSE) {
          next
        }
        ncout_prec <- ncdf4::nc_create(outfile_prec, vars = list(var_lat,var_long,var_E,var_W,var_S,var_N), verbose = verbose)
        put_var(ncout_prec)
        ## precipitation_flux
        ncout_prec <- insert(ncout_prec, "PRECTmms", "mm/s", PRECTmms[tsel], dim)
        ncdf4::nc_close(ncout_prec)
        # solar
        outfile_slr <- file.path(outfolder, paste0("Slr", formatC(year, width = 4, flag = "0"), "-",
                                               formatC(mo, width = 2, flag = "0"), ".nc")) 
        if (file.exists(outfile_slr) & overwrite == FALSE) {
          next
        }
        ncout_slr <- ncdf4::nc_create(outfile_slr, vars = list(var_lat,var_long,var_E,var_W,var_S,var_N), verbose = verbose)
        put_var(ncout_slr)
        ## surface_downwelling_shortwave_flux_in_air
        ncout_slr <- insert(ncout_slr, "FSDS", "W m-2", FSDS[tsel], dim)
        ncdf4::nc_close(ncout_slr)
        
        # temper
        outfile_tem <- file.path(outfolder, paste0("Tem", formatC(year, width = 4, flag = "0"), "-",
                                               formatC(mo, width = 2, flag = "0"), ".nc"))
        if (file.exists(outfile_tem) & overwrite == FALSE) {
          next
        }
        ncout_tem <- ncdf4::nc_create(outfile_tem, vars = list(var_lat,var_long,var_E,var_W,var_S,var_N), verbose = verbose)
        put_var(ncout_tem)
        ## surface_downwelling_longwave_flux_in_air
        ncout_tem <- insert(ncout_tem, "FLDS", "W m-2", FLDS[tsel], dim)
        ## air_pressure
        ncout_tem <- insert(ncout_tem, "PSRF", "Pa", PSRF[tsel], dim)
        ## specific_humidity
        ncout_tem <- insert(ncout_tem, "QBOT", "kg/kg", QBOT[tsel], dim)
        ## air_temperature
        ncout_tem <- insert(ncout_tem, "TBOT", "K", TBOT[tsel], dim)
        ## eastward_wind & northward_wind
        ncout_tem <- insert(ncout_tem, "WIND", "m/s", WIND[tsel], dim)
        ncdf4::nc_close(ncout_tem)
        
      }
      ncdf4::nc_close(nc) #PEcAn input file
    }  ## end input file exists
  }  ### end year loop over met files
  
  #PEcAn.logger::logger.info("Done with met2model.FATES")
  
  return(LATIXY)
 # (data.frame(file = paste0(outfolder, "/"), 
 #                   host = c(PEcAn.remote::fqdn()), 
 #                   mimetype = c("application/x-netcdf"), 
 #                  formatname = c("CLM met"), 
 #                   startdate = c(start_date), 
 #                   enddate = c(end_date), 
 #                   dbfile.name = "", 
 #                   stringsAsFactors = FALSE))
} # met2model.FATES

met2model.FATES(in.path='/Users/mac/Documents/pecan/models/fates/R/FI-Hyy',in.prefix='/FLX_FI-Hyy_FLUXNET2015_FULLSET_HH_1996-2018_beta-3',
                            outfolder='/Users/mac/Documents/pecan/models/fates/R/out',start_date=2016/1/1,end_date=2018/12/31,lst=0,lat=61.8474,lon=24.2948,overwrite = FALSE, verbose = FALSE)
