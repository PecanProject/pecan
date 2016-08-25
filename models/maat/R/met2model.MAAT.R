#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#

## R Code to convert NetCDF CF met files into MAAT model met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and 
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change 
##overwrite statement below to TRUE.


## For testing
# "PEcAn.SIPNET::met2model.SIPNET('/data/Model_Output/pecan.output/dbfiles/Ameriflux_CF_gapfill_site_1-146','US-Dix',
# '/data/Model_Output/pecan.output/dbfiles/Ameriflux_SIPNET_site_1-146/','2006/01/01','2006/12/31',lst = '-5',lat = '39.9712',lon = '-74.4346')"
in.path <- '~/Data/Projects/NGEE-Tropics/Met_Drivers/testing/Ameriflux_site_0-676/'
in.prefix <- 'US-WCr'
outfolder <- '~/Data/Projects/NGEE-Tropics/Met_Drivers/testing/Ameriflux_MAAT_site_0-676/'
start_date <- '2001/01/01'
end_date <- '2001/12/31'
year <- 2001

in.path <- '~/Data/Projects/NGEE-Tropics/Met_Drivers/testing/Ameriflux_CF_gapfill_site_1-146/'
in.prefix <- 'US-Dix'
outfolder <- '~/Data/Projects/NGEE-Tropics/Met_Drivers/testing/Ameriflux_MAAT_site_1-146/'
start_date <- '2006/01/01'
end_date <- '2006/12/31'
year <- 2006



##-------------------------------------------------------------------------------------------------#
##' met2model wrapper for MAAT
##'
##' @name met2model.MAAT
##' @title Create MAAT met driver files
##' @param in.path location on disk where inputs (CF met drivers) are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where MAAT met outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##' @export
##' @author Shawn P. Serbin
##'
met2model.MAAT <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){
  if(!require(PEcAn.utils)) print("**Plesae install PEcAn.utils then retry**")
  
  ## MAAT driver format (.csv):
  ## Timestep,  Air Temp (°C), PAR (umols m-2 s-1), Precipitation( ??), Atmospheric CO2 (μmol mol-1) ... # STILL IN DEVELOPMENT
  
  print("START met2model.MAAT")
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")

  out.file <- paste(in.prefix, strptime(start_date, "%Y-%m-%d"),strptime(end_date, "%Y-%m-%d"),"csv", sep=".")
  out.file.full <- file.path(outfolder, out.file)
  
  results <- data.frame(file = out.file.full,
                        host = fqdn(),
                        mimetype ='text/csv',
                        formatname = 'MAAT meteorology' ,
                        startdate = start_date ,
                        enddate = end_date,
                        dbfile.name = out.file,
                        stringsAsFactors = FALSE)
  print("internal results")
  print(results)
  
  if (file.exists(out.file.full) && !overwrite) {
    logger.debug("File '", out.file.full, "' already exists, skipping to next file.")
    return(invisible(results))
  }
  
  require(ncdf4)
  require(lubridate)
  require(PEcAn.data.atmosphere)
  #  require(ncdf)
  
  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  out <- NULL
  
  # get start/end year since inputs are specified on year basis
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  ## loop over files
  # TODO need to filter out the data that is not inside start_date, end_date
  for(year in start_year:end_year) {
    
    skip <- FALSE
    print(year)
    
    ncdf.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))
    
    if(file.exists(ncdf.file)){
      ## open netcdf
      nc <- nc_open(ncdf.file)
      
      ## convert time to seconds
      #if (!is.null(nc$dim$DTIME$vals)){
      #  sec   <- nc$dim$DTIME$vals
        #sec <- udunits2::ud.convert(sec,unlist(strsplit(nc$dim$DTIME$units," "))[1],"seconds")
      #} else {
      #  sec   <- nc$dim$time$vals 
        #sec <- udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
      #}

      sec <- nc$dim$time$vals  
      sec <- udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")

      ifelse(leap_year(year)==TRUE,
             dt <- (366*24*60*60)/length(sec), #leap year
             dt <- (365*24*60*60)/length(sec)) #non-leap year
      tstep = round(86400/dt)
      dt = 86400/tstep
      
      ## extract required MAAT driver variables
      #names(nc$var) # what is in the nc file?
      lat  <- ncvar_get(nc,"latitude")
      lon  <- ncvar_get(nc,"longitude")
      Tair <- ncvar_get(nc,"air_temperature")  ## in Kelvin
      RH_perc <- ncvar_get(nc,"relative_humidity") ## RH Percentage
      
      SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air") ## in W/m2
      PAR  <- try(ncvar_get(nc,"surface_downwelling_photosynthetic_photon_flux_in_air")) ## in mol/m2/s
      if(!is.numeric(PAR)) PAR = SW*0.45 
      
      CO2  <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air"))
      useCO2 = is.numeric(CO2)  
      if(useCO2)  CO2 <- CO2 * 1e6  ## convert from mole fraction (kg/kg) to ppm
      

      
      
      nc_close(nc)
      
    
    
    
} # End of function
##-------------------------------------------------------------------------------------------------#
### EOF