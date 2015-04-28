##' @name met2CF.csv
##' @title met2CF.csv
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param format data frame or list with elements orig, bety, units for the original variable name, bety variable name, 
##' and original units. Columns with NA for bety variable name are dropped. Units for datetime field are the lubridate function 
##' that will be used to parse the date (e.g. \code{ymd_hms} or \code{mdy_hm}). 
##' @param nc_verbose logical: run ncvar_add in verbose mode?
##' @export
##' @author Mike Dietze, David LeBauer
##' @examples
##' \dontrun{
##'   in.path = "~/Downloads/"
##'   in.file = "WR_E"
##'   outfolder = "/tmp/"
##'   format = list(orig=c("TA","PRECIP","RH","WS","WD","SW","PAR_in"),
##'                 units = c("celsius","mm","%","m/s","degrees","W m-2","umol m-2 s-1"),
##'                 bety=c("airT","precipitation_flux","relative_humidity","Wspd","wind_direction","solar_radiation","PAR"),
##'                 skip=7,
##'                 unit.row=TRUE,
##'                 na.strings=c("-9999","-6999","9999"))  
##'   lat = 40
##'   lon = -80
##'   met2CF.csv(in.path,in.file,outfolder,format,lat,lon)
##' }
met2CF.csv <- function(in.path, in.file, outfolder, format, lat=NULL, lon=NULL, nc_verbose = FALSE){

  files <- dir(in.path, in.file, full.names = TRUE)
  files <- files[grep("*.csv",files)]
  if(length(files)==0){
    return(NULL)
    logger.warn("No met files named ", in.file, "found in ", in.path)
  }
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)  # does nothing if dir exists
  
  for(i in 1:length(files)){
    
    new.file <- file.path(outfolder, gsub(".csv","_CF.nc",basename(files[i])))
  
    dat <- read.csv(files[i], skip = format$skip, na.strings = format$na.strings, as.is=TRUE)

    ## some files have a line under the header that lists variable units
    if(format$unit.row){  
      units <- dat[1,]
      dat <- dat[-1,]
    }
    
    ## Get datetime vector
    datetime_index <- which(format$bety == "datetime")
    datetime_raw <- dat[, datetime_index]
    datetime <- do.call(format$units[datetime_index], list(datetime_raw))
    ## and remove datetime from 'dat' dataframe
    dat[, datetime_index] <- format$na.strings

    ## convert data to numeric

    dat <- as.data.frame(datetime = datetime, sapply(dat[,-datetime_index], as.numeric))
    
    ### create time dimension 
    days_since_1700 <- datetime - ymd("1700-01-01")
    t <- ncdim_def("time", "days since 1700-01-01", as.numeric(days_since_1700)) #define netCDF dimensions for variables
    timestep <- as.numeric(mean(ud.convert(diff(days_since_1700), "d", "s")))


    ## create lat lon dimensions
    x <- ncdim_def("lon", "degrees_east", lon) #define netCDF dimensions for variables
    y <- ncdim_def("lat", "degrees_north", lat)
    
    xytdim <- list(x,y,t)
    ## air_temperature / airT
    if("airT" %in% format$bety){
      k <- which(format$bety=="airT")
      airT.var <- ncvar_def(name="air_temperature",units = "K",dim = xytdim)
      nc <- nc_create(new.file, vars = airT.var) #create netCDF file
      ncvar_put(nc, varid = 'air_temperature',
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"celsius","K"))  
    }
    

    if("CO2" %in% format$bety){
      k <- which(format$bety == "CO2")
      co2.var <- ncvar_def(name = "CO2", units = "ppm", dim = xytdim)
      nc <- ncvar_add(nc = nc, v = co2.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'CO2',
                vals = met.conv(dat[,as.character(format$orig[k])], format$units[k], "Pa", "Pa"))
    }

    ## air_pressure / Psurf
    if("air_pressure" %in% format$bety){
      k = which(format$bety=="air_pressure")

      Psurf.var = ncvar_def(name="air_pressure",units = "Pa",dim = xytdim)
      nc = ncvar_add(nc = nc, v = Psurf.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc,varid='air_pressure',
            vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"Pa","Pa"))
  
    }

    ## precipitation_flux / rain
    if("precipitation_flux" %in% format$bety){
      ## units preprocessing
      k = which(format$bety=="precipitation_flux")
      rain = dat[,as.character(format$orig[k])]
      rain.units = as.character(format$units[k])
      rain.units = switch(rain.units,
             mm = {rain = rain / timestep; "kg/m2/s"},
             m  = {rain = rain / timestep; "Mg/m2/s"},
             'in' = {rain=ud.convert(rain / timestep, "in", "mm"); "kg/m2/s"},
             'mm h-1' = {rain = ud.convert(rain / timestep, "h", "s"); "kg/m2/s"}
      )        
         
      ## insert

      rain.var = ncvar_def(name="precipitation_flux",units="kg/m2/s",dim=xytdim)
      nc = ncvar_add(nc = nc, v = rain.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'precipitation_flux',
                vals = met.conv(rain, rain.units, "kg/m2/s", "kg/m2/s"))  
    }

    ## relative_humidity / RH
    if("relative_humidity" %in% format$bety){
      k = which(format$bety=="relative_humidity")
      RH.var = ncvar_def(name="relative_humidity",units="%",dim=xytdim)
      nc = ncvar_add(nc = nc, v = RH.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc,varid='relative_humidity', 
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"%","%"))
    }

    ## specific_humidity / qair
    if("specific_humidity" %in% format$bety){
      k = which(format$bety=="specific_humidity")
      qair.var = ncvar_def(name="specific_humidity",units="%",dim=xytdim)
      nc = ncvar_add(nc = nc, v = qair.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'specific_humidity',
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"1","1"))
    } else {
      ## file needs to be closed and re-opened to access added variables
      nc_close(nc)
      nc = nc_open(new.file,write=TRUE,readunlim=FALSE)
      if("relative_humidity" %in% names(nc$var) & "air_temperature" %in% names(nc$var)){
        ## Convert RH to SH
        qair = rh2qair(rh=ncvar_get(nc,"relative_humidity")/100,T=ncvar_get(nc,"air_temperature"))
        qair.var = ncvar_def(name="specific_humidity",units="%",dim=xytdim)
        nc = ncvar_add(nc = nc, v = qair.var, verbose = nc_verbose) #add variable to existing netCDF file
        ncvar_put(nc, varid = 'specific_humidity',vals=qair)
      }
    }

    ## wind_speed
    if("eastward_wind" %in% format$bety & "northward_wind" %in% format$bety){
      
      k = which(format$bety=="eastward_wind")
      uwind.var = ncvar_def(name="eastward_wind",units="m/s",dim=xytdim)
      nc = ncvar_add(nc = nc, v = uwind.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'eastward_wind',
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"m/s","m/s"))
 
      k = which(format$bety=="northward_wind")
      uwind.var = ncvar_def(name="northward_wind",units="m/s",dim=xytdim)
      nc = ncvar_add(nc = nc, v = uwind.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'northward_wind',
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"m/s","m/s"))
            
    } else{
      if("Wspd" %in% format$bety){
        
        ## extract & convert wind_speed
        k = which(format$bety=="Wspd")
        wind = met.conv(dat[,as.character(format$orig[k])],format$units[k],"m/s","m/s")
        
        if("wind_direction" %in% format$bety){
          
          k = which(format$bety=="wind_direction")
          wind_direction = met.conv(dat[,as.character(format$orig[k])],format$units[k],"degrees","radians")
          
          ## Convert wind_speed and wind_direction into eastward_wind and northward_wind
          uwind <- wind*cos(wind_direction)
          vwind <- wind*sin(wind_direction)
          
          u.var <- ncvar_def(name='eastward_wind',units='m/s',dim=list(xytdim)) #define netCDF variable, doesn't include longname and comments
          nc = ncvar_add(nc = nc, v = u.var, verbose = nc_verbose) #add variable to existing netCDF file
          ncvar_put(nc, varid = 'eastward_wind',vals=uwind)
          
          v.var <- ncvar_def(name='northward_wind',units='m/s',dim=list(xytdim)) #define netCDF variable, doesn't include longname and comments
          nc = ncvar_add(nc = nc, v = v.var, verbose = nc_verbose) #add variable to existing netCDF file
          ncvar_put(nc, varid = 'northward_wind',vals=vwind)          
        } else {
         
          ## if no direction information is available, just insert wind_speed
          wind.var = ncvar_def(name="wind_speed",units="m/s",dim=xytdim)
          nc = ncvar_add(nc = nc, v = wind.var, verbose = nc_verbose) #add variable to existing netCDF file
          ncvar_put(nc, varid = 'wind_speed',vals=wind)
        }
        
      }
    }  ## end wind
    
    ## surface_downwelling_longwave_flux_in_air / lwdown / dlwrf 
    if("surface_downwelling_longwave_flux_in_air" %in% format$bety){
      k = which(format$bety=="surface_downwelling_longwave_flux_in_air")
      lwdown.var = ncvar_def(name="surface_downwelling_longwave_flux_in_air",units="W m-2",dim=xytdim)
      nc = ncvar_add(nc = nc, v = lwdown.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'surface_downwelling_longwave_flux_in_air',
            vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"W m-2","W m-2"))
    }
    
    ## surface_downwelling_shortwave_flux_in_air / swdown / dswrf / solar radiation
    swdown = NULL
    if("solar_radiation" %in% format$bety){
      k = which(format$bety=="solar_radiation")
      swdown = met.conv(dat[,as.character(format$orig[k])],format$units[k],"W m-2","W m-2")
      swdown.var = ncvar_def(name="surface_downwelling_shortwave_flux_in_air",units="W m-2",dim=xytdim)
      nc = ncvar_add(nc = nc, v = swdown.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'surface_downwelling_shortwave_flux_in_air',
            vals=swdown)
    }

    ## PAR 
    if("PAR" %in% format$bety){
      k = which(format$bety=="PAR")
      PAR = met.conv(dat[,as.character(format$orig[k])],format$units[k],"umol m-2 s-1","mol m-2 s-1")
      PAR.var = ncvar_def(name="surface_downwelling_photosynthetic_photon_flux_in_air",units="mol m-2 s-1",dim=xytdim)
      nc = ncvar_add(nc = nc, v = PAR.var, verbose = nc_verbose) #add variable to existing netCDF file
      ncvar_put(nc, varid = 'surface_downwelling_photosynthetic_photon_flux_in_air', vals=PAR)
    }


    nc_close(nc)
    
  } ## end loop over files
    
}
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
