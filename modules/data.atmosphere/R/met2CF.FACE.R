met2CF.FACE <- function(in.path,in.prefix,outfolder){
  
  require(ncdf4)
  require(ncdf4.helpers)
  
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) return(NULL)
  if(!file.exists(outfolder)) dir.create(outfolder)
  
  for(i in 1:length(files)){
    
    f  <- paste0(in.path,files[i])
    f.cf <- paste0(outfolder,files[i])
    file.copy(f,f.cf)
    nc.o <- nc_open(f,write=TRUE)
    nc <- nc_open(f.cf,write=TRUE)
    
    # Change to CF variable names 
    
    vars <- c("nav_lat", "nav_lon", "Rainf", "Tair", "RH", "VPD", "Qair", 
              "Wind", "SWdown", "PAR", "LWdown", "Psurf", "aCO2", "eCO2", "aO3", 
              "eO3", "SolarElevation")
    
    nvars <- c("lat", "lon", "precipitation_flux", "air_temperature", "relative_humidity", 
               "water_vapor_saturation_deficit", "specific_humidity", "wind_speed", 
               "surface_downwelling_shortwave_flux", "surface_downwelling_photosynthetic_radiative_flux_in_air", 
               "surface_downwelling_longwave_flux", "air_pressure", "mass_concentration_of_carbon_dioxide_in_air_ambient", 
               "mass_concentration_of_carbon_dioxide_in_air_elevated", "mass_concentration_of_ozone_in_air_ambient", 
               "mass_concentration_of_ozone_in_air_elevated", "solar_elevation_angle")
    
    l <- length(vars)
    for (k in 1:l){
      nc <- ncvar_rename(nc,vars[k],nvars[k])
    }
    
    # Split into annual files 
    
    year <- ncvar_get(nc, 'YEAR')
    y <- year[1]:year[length(year)]
    n <- length(y)
    step <- array(0,n)
    t <- -1 
    for(j in 1:n){
      s <- t + 1; print(s)
      e <- t + sum(year == y[j]); print(e)
      new.prefix <- paste0(unlist(strsplit(files[i],"_"))[1])
      new.file <- paste0(outfolder,new.prefix,"/",new.prefix,".",y[j],".nc")
      if(file.exists(f.cf)==TRUE && file.exists(new.file)==FALSE){
        system(paste0("ncks -d tstep,",s,",",e," ",f.cf," ",new.file))
      }
      
      t <- e
    }
  }
}


