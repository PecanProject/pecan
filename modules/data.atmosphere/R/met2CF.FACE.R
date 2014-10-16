met2CF.FACE <- function(in.path,in.prefix,outfolder){
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  # Check to see if input is already in dbfiles table 
  check <- input.name.check(outname, con, dbparams)
  if(is.null(check)==FALSE){
    logger.error('Input is already in the database.')
    db.close(con)
    return(check) 
  }
  
  require(ncdf4)
  require(ncdf4.helpers)
  
  ################################### 
    
    files = dir(in.path,in.prefix)
    files = files[grep(pattern="*.nc",files)]
    
    if(length(files) == 0) return(NULL)
    if(!file.exists(outfolder)) dir.create(outfolder)
    
    for(i in 1:length(files)){
      
      f  <- paste0(in.path,files[i])
      f.cf <- paste0(outfolder,files[i]) #paste0(outfolder,in.prefix,"_CF",unlist(strsplit(file,in.prefix))[[2]])
      if(!file.exists(f.cf)){file.copy(f,f.cf)}
      
      nc.o <- nc_open(f,write=TRUE)
      nc <- nc_open(f.cf,write=TRUE)
      
      # Change units of PAR
      
      par <- ncvar_get(nc=nc,varid="PAR")
      par.new <- par * 1000000 #umols to mols
      ncvar_put(nc=nc, varid='PAR',vals=par.new)
      ncatt_put(nc=nc,varid='PAR',attname='units',attval='mol/m2/s') 
      
      # Change to CF variable names 
      
      vars <- c("nav_lat", "nav_lon", "Rainf", "Tair", "RH", "VPD", "Qair", 
                "Wind", "SWdown", "PAR", "LWdown", "Psurf", "aCO2", "eCO2", "aO3", 
                "eO3", "SolarElevation")
      
      nvars <- c("lat", "lon", "precipitation_flux", "air_temperature", "relative_humidity", 
                 "water_vapor_saturation_deficit", "specific_humidity", "wind_speed", 
                 "surface_downwelling_shortwave_flux", "surface_downwelling_photosynthetic_radiative_flux_in_air", 
                 "surface_downwelling_longwave_flux", "air_pressure", "mass_concentration_of_carbon_dioxide_in_air", 
                 "mass_concentration_of_carbon_dioxide_in_air", "mass_concentration_of_ozone_in_air", 
                 "mass_concentration_of_ozone_in_air", "solar_elevation_angle")
      
      nc.o.vars <-  nc.get.variable.list(nc)
      l <- length(vars)
      for (k in 1:l){   
        if(vars[k] %in% nc.o.vars){nc <- ncvar_rename(nc,vars[k],nvars[k])}
      }
      
#       # Rename time dimension
#       nc.o.dims <- nc.get.dim.names(nc.o)
#       if ("tstep" %in% nc.o.dims){system(paste("ncrename -d tstep,time ", f.cf))}
      
      
      # Split into annual files 
      
      year <- ncvar_get(nc, 'YEAR')
      y <- year[1]:year[length(year)]
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
      nc_close(nc)
      nc_close(nc.o) 
      file.remove(paste0(outfolder,files[i]))
    } #end loop 
  }
}


