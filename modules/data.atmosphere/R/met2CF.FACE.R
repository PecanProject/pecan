##' @name met2CF.FACE
##' @title met2CF.FACE
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param convert FACE files to CF files
##' @author Elizabeth Cowdery
##' 

met2CF.FACE <- function(in.path,in.prefix,outfolder,start_year,end_year){

  require(ncdf4)
  require(ncdf4.helpers)
  
  files = dir(in.path,in.prefix)
  file = files[grep(pattern="*.nc",files)]
  if(!(length(file) == 1)) return(NULL)
  f  <- paste0(in.path,file)

  for(treatment in c("a", "e")){

    outfolder <- paste(unlist(strsplit(outfolder, "FACE")), collapse =paste0("FACE_", treatment))
    if(!file.exists(outfolder)) dir.create(outfolder)
    f.cf <- paste0(outfolder,file)
    if(!file.exists(outfolder)){file.copy(f,f.cf)}

    # remove the unwanted treatment 
    if(treatment == "a"){rm.vars <- c("eCO2", "eO3")}
    else if (treatment == "e"){rm.vars <- c("aCO2", "aO3")}
    else{logger.error("Need a CO2 levels treatment")}
  
    paste("ncks -x -v", paste0(rm.vars,collapse = ","), f.cf, f.cf)

    # Change to CF variable names 

    nc <- nc_open(f.cf,write=TRUE)
    nc.vars <-  nc.get.variable.list(nc)
    
    vars <- c("nav_lat", "nav_lon", "Rainf", "Tair", "RH", "VPD", "Qair", 
      "Wind", "SWdown", "PAR", "LWdown", "Psurf", "aCO2", "eCO2", "aO3", 
      "eO3", "SolarElevation")
    
    nvars <- c("latitude", "longitude", "precipitation_flux", "air_temperature", "relative_humidity", 
     "water_vapor_saturation_deficit", "specific_humidity", "wind_speed", 
     "surface_downwelling_shortwave_flux", "surface_downwelling_photosynthetic_radiative_flux_in_air", 
     "surface_downwelling_longwave_flux", "air_pressure", "mass_concentration_of_carbon_dioxide_in_air", 
     "mass_concentration_of_carbon_dioxide_in_air", "mass_concentration_of_ozone_in_air", 
     "mass_concentration_of_ozone_in_air", "solar_elevation_angle")
    
    l <- length(vars)
    for (k in 1:l){   
      if(vars[k] %in% nc.vars){nc <- tncar_rename(nc,vars[k],nvars[k])}
    }
    
    # Split into annual files 
    
    require(lubridate)
    start_date <- as.POSIXlt(start_date, tz = "GMT")
    end_date <- as.POSIXlt(end_date, tz = "GMT")
    start_year <- year(start_date)
    end_year <- year(end_date)
    
    y <- start_year:end_year
    n <- length(y)
    t <- -1 
    for(j in 1:n){
      new.file <- paste0(outfolder,in.prefix,".",y[j],".nc")
      if(!file.exists(new.file)){ 
        s <- t + 1; print(s)
        e <- t + sum(year == y[j]); print(e)      
        if(file.exists(f.cf)==TRUE && file.exists(new.file)==FALSE){
          system(paste0("ncks -d tstep,",s,",",e," ",f.cf," ",new.file))
        } 
      }
      t <- e
    } 

  }

  nc_close(nc) 
  file.remove(paste0(in.path,file))


  }

