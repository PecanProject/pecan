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

# leaf_user_met prefix
PREFIX_XML <- '<?xml version="1.0"?>\n'

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
      sec <- nc$dim$time$vals
      frac.day <- nc$dim$time$vals
      sec <- udunits2::ud.convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")

      ifelse(leap_year(year)==TRUE,
             dt <- (366*24*60*60)/length(sec), #leap year
             dt <- (365*24*60*60)/length(sec)) #non-leap year
      tstep <- round(86400/dt)
      dt <- 86400/tstep
      
      ## extract required MAAT driver variables
      #names(nc$var) # what is in the nc file?
      lat  <- ncvar_get(nc,"latitude")
      lon  <- ncvar_get(nc,"longitude")
      Tair <- ncvar_get(nc,"air_temperature")  ## in Kelvin
      Rain <- ncvar_get(nc,"precipitation_flux") ## "kg/m^2/s"
      
      # get humidity vars (NOTE:later add VPD here!!)
      RH_perc <- ncvar_get(nc,"relative_humidity") ## RH Percentage

      # get radiation
      SW   <- ncvar_get(nc,"surface_downwelling_shortwave_flux_in_air") ## in W/m2
      PAR  <- try(ncvar_get(nc,"surface_downwelling_photosynthetic_photon_flux_in_air")) ## in mol/m2/s
      if(!is.numeric(PAR)) {
        PAR <- SW*0.45
        ##!! UPDATE SO UNITS ARE CONVERTED TO umol/m2/s, converted later to dt
      }
      
      # get CO2 (if exists)
      CO2  <- try(ncvar_get(nc,"mole_fraction_of_carbon_dioxide_in_air"))
      useCO2 = is.numeric(CO2)  
      if(useCO2)  CO2 <- CO2 * 1e6  ## convert from mole fraction (kg/kg) to ppm

      nc_close(nc)
      } else {
        print("Skipping to next year")
        next
      }
    
    ##build time variables (year, month, day of year)
    nyr <- floor(length(sec)/86400/365*dt)
    yr <- NULL
    doy <- NULL
    hr <- NULL
    asec <- sec
    for(y in year+1:nyr-1){
      ytmp <- rep(y,365*86400/dt)
      dtmp <- rep(1:365,each=86400/dt)
      if(y %% 4 == 0){  ## is leap
        ytmp <- rep(y,366*86400/dt)
        dtmp <- rep(1:366,each=86400/dt)
      }
      if(is.null(yr)){
        yr <- ytmp
        doy <- dtmp
        hr <- rep(NA,length(dtmp))
      } else {
        yr <- c(yr,ytmp)
        doy <- c(doy,dtmp)
        hr <- c(hr,rep(NA,length(dtmp)))
      }
      rng <- length(doy) - length(ytmp):1 + 1
      if(!all(rng>=0)){
        skip = TRUE
        logger.warn(paste(year,"is not a complete year and will not be included"))
        break
      }
      asec[rng] <- asec[rng] - asec[rng[1]]
      hr[rng] <- (asec[rng] - (dtmp-1)*86400)/86400*24
    }
    
    # output matrix
    n <- length(Tair)
    tmp <- cbind(YEAR=yr[1:n],DOY=doy[1:n],HOUR=hr[1:n],FRAC_DAY=frac.day[1:n],TIMESTEP=rep(dt/86400,n),
                 
                 #TODO: Add VPD, etc

                 CO2=CO2,
                 Tair_degC=Tair-273.15,  # convert to celcius
                 Prec_mm=Rain*dt, ## converts from mm/s to mm
                 RH_perc=RH_perc,
                 #PAR=PAR*dt #mol/m2/dt
                 PAR_umol_m2_s=PAR*1000000 #umols/m2/s
    )
    
    ## quick error check, sometimes get a NA in the last hr ?? NEEDED?
    hr.na <- which(is.na(tmp[,3]))
    if(length(hr.na)>0) tmp[hr.na,3] <- tmp[hr.na-1,3] + dt/86400*24
    
    if(is.null(out)){
      out <- tmp
    } else {
      out <- rbind(out,tmp)
    }
  } ## end loop over years
  
  if(!is.null(out)){
    
    ## write met csv output 
    #write.table(out,out.file.full,quote = FALSE,sep="\t",row.names=FALSE,col.names=FALSE)
    write.csv(out,out.file.full,row.names=FALSE)
    
    # write out leaf_user_met.xml - example
    #<met_data_translator>
    #<leaf>
      #<env>
        #<par>'PAR'</par>
        #<temp>'AT'</temp>
        #<vpd>'VPD'</vpd>
      #</env>
    #</leaf>
    #</met_data_translator>
    
    # Create leaf_user_met.xml
    # TODO: make this dynamic with names above!
    # TODO: add the additional met variables, make dynamic
    leaf_user_met_list <- list(leaf = list(env = list(temp = "Tair_degC", par = "PAR_umol_m2_s")))
    leaf_user_met_xml <- listToXml(leaf_user_met_list,"met_data_translator")
    
    # output XML file
    saveXML(leaf_user_met_xml, file = file.path(outfolder, "leaf_user_met.xml"), indent=TRUE, prefix = PREFIX_XML)
    
    invisible(results)
    
  } else {
    print("NO MET TO OUTPUT")
    invisible(NULL)
  }
  
} # End of function
##-------------------------------------------------------------------------------------------------#
### EOF
