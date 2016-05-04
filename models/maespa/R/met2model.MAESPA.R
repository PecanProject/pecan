./#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#
## R Code to convert NetCDF CF met files into MAESPA met files

##If files already exist in "Outfolder", the default function is NOT to overwrite them and
##only gives user the notice that file already exists. If user wants to overwrite the existing files, just change
##overwrite statement below to TRUE.

##' met2model wrapper for MAESPA
##'
##' @title met2model.MAESPA
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbose
##'
##' @author Tony Gardella

met2model.MAESPA <- function(in.path, in.prefix, outfolder, start_date, end_date, ..., overwrite=FALSE,verbose=FALSE){

  MOL_2_UMOL <- 1E6
  library(PEcAn.utils)
  print("START met2model.MAESPA")
  start.date <- as.POSIXlt(start_date, tz = "GMT")
  end.date<- as.POSIXlt(end_date, tz = "GMT")
  out.file <- paste(in.prefix, strptime(start.date, "%Y-%m-%d"),strptime(end.date, "%Y-%m-%d"),"dat", sep=".")
  out.file.full <- file.path(outfolder, out.file)

  results <- data.frame(file = out.file.full,
                        host = fqdn(),
                        mimetype ='text/plain',
                        formatname = 'maespa.met' ,
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

  ## check to see if the outfolder is defined, if not create directory for output
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }

  out <- NULL

  # get start/end year since inputs are specified on year basis
  start_year <- year(start.date)
  end_year <- year(end.date)

  ## loop over files
  for (year in start_year:end_year) {
    print(year)

    old.file <- file.path(in.path, paste(in.prefix, year, "nc", sep="."))

    if (file.exists(old.file)) {
      ## open netcdf
      nc <- nc_open(old.file)
      ## convert time to seconds
      sec   <- nc$dim$time$vals
      sec = udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")

      ifelse(leap_year(year),
             dt <- (366*24*60*60)/length(sec), #leap year
             dt <- (365*24*60*60)/length(sec)) #non-leap year
      tstep = round(86400/dt)
      dt = 86400/tstep

      #Check wich variabales are available and which are not

      ## extract variables
      lat  <- ncvar_get(nc,"latitude")
      lon  <- ncvar_get(nc,"longitude")
      RAD <-  ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air") #W m-2
      PAR <-   try(ncvar_get(nc,"surface_downwelling_photosynthetic_photon_flux_in_air")) #mol m-2 s-1
      PAR <- PAR * MOL_2_UMOL
      if (!is.numeric(PAR)) {
        SW <- ncvar_get(nc, "surface_downwelling_shortwave_flux_in_air") ##in W/m2
        PAR <- SW * SW_2_PAR
      }
      TAIR <-  ncvar_get(nc,"air_temperature") #K
      `RH%` <- try(ncvar_get(nc,"relative_humidity")) #percentage
      PPT <-  ncvar_get(nc,"precipitation_flux") #kg m-2 s-1
      CA <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air")) #mol/mol
      PRESS <-  ncvar_get(nc,"air_pressure")# Pa

      # TSOIL <-  try(ncvar_get(nc,"soil_temperature")) #K
      # VMFD <-  try(ncvar_get(nc,"vapour_pressure_mole_fraction_deficit"))
      # WIND <- try(ncvar_get(nc,"wind_speed")) #m/s
      # SW <-  try(ncvar_get(nc,"soil_moisture"))# kg m-2
      # SWP <- ncvar_get(nc,"soil_water_potential)# MPa
      # TDEW <- try(ncvar_get(nc, "dew_point_temperature")) #Celsius
      # FBEAM <- ncvar_get try((nc,"fraction_of_surface_downwelling_photosynthetic_photon_flux_in_air") #frction of direct beam
      # RH <- try(ncvar_get(nc,"relative_humidity"))# fraction

      # ºC   air temperature. If nonexistant. Error.
      TAIR <- udunits2::ud.convert(TAIR,"kelvin","celsius")

      ####ppm. atmospheric CO2 concentration. Constant from Enviiron namelist used instead
      if(!is.numeric(CA)) {
        print("Atmospheric CO2 concentration will be set to constant value set in ENVIRON namelist ")
        rm(CA)
        defaultCO2= 400 #400 is estimation of atmospheric CO2 in ppm)
      } else {
        defaultCO2= 400
      } #400 is estimation of atmospheric CO2 in ppm))

      nc_close(nc)
    } else {
      print("Skipping to next year")
      next
    }
    tmp<-rbind(TAIR,PPT,RAD)

    if(is.null(out)) {
      out = tmp
    } else {
      out = cbind(out,tmp)
    }

  }### end loop over years

  out[is.na(out)] <-0
  #Get names
  columnnames =  paste0("'",rownames(out),"'",collapse= " ")
  #Get number of variables
  numbercolumns = nrow(out)
  #turn into matrix
  out<- matrix(out,ncol= numbercolumns)

  #Set day or hour Option(1 or 0)
  if(tstep>=1){dayorhour=1}else{dayorhour=0}
  #Set number of timesteps in a day(timetsep of input data)
  timesteps = tstep
  # Set distribution of diffuse radiation incident from the sky.(0.0) is default.
  difsky= 0.5
  #Change format of date to DD/MM/YY
  startdate = paste0("'",format(as.Date(start_date),"%d/%m/%y"),"'")
  enddate = paste0("'",format(as.Date(end_date),"%d/%m/%y"),"'")
  metdat <- readLines(con=system.file("template.met", package = "PEcAn.MAESPA"), n=-1)


  ## write output
  #metdat<- gsub('@MAESPAMETHEADER@',metheader,metdat)
  metdat<- gsub('@DISTDIFFRADINCIDENCE@',difsky,metdat)
  metdat<- gsub('@DEFCO2@',defaultCO2,metdat)
  #metdat<- gsub('@DEFSWMIN@',defaultSWmin,metdat)
  #metdat<- gsub('@DEFSWMAX@',defaultSWmax,metdat)
  #metdat<- gsub('@DEFATMOSPRESSURE@',deafaultatmospress,metdat)
  metdat<- gsub('@LATITUDE@',lat,metdat)
  metdat<- gsub('@LONGITUDE@',lon,metdat)
  #metdat<- gsub('@TZLONG@',longmeridian,metdat)
  #metdat<- gsub('@LONGHEM@',longhemisphere,metdat)
  #metdat<- gsub('@LATHEM@',lathemisphere,metdat)
  metdat<- gsub('@DAYORHR@',dayorhour,metdat)
  metdat<- gsub('@TSTEPS@',timesteps,metdat)
  metdat<- gsub('@NUMCOLUMNS@',numbercolumns,metdat)
  metdat<- gsub('@STARTDATE@',startdate,metdat)
  metdat<- gsub('@ENDDATE@',enddate,metdat)
  metdat<- gsub('@COLNAMES@',columnnames,metdat)

  writeLines(metdat, con=file.path(out.file.full))
  write(paste(out),file=file.path(out.file.full),append = TRUE,ncol=numbercolumns)

  invisible(results)

}  ### End of function
