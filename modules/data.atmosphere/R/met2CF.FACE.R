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

met2CF.FACE <- function(in.path,in.prefix,outfolder,start_date,end_date){
  
  require(ncdf4)
  require(ncdf4.helpers)
  require(PEcAn.utils)
  
  files = dir(in.path,in.prefix)
  file = files[grep(pattern="*.nc",files)]
  if(!(length(file) == 1)) return(NULL)
  f  <- file.path(in.path,file)
  
  for(treatment in c("a", "e")){
    
    t.outfolder <- paste(unlist(strsplit(outfolder, "FACE")), collapse =paste0("FACE_", treatment))
    if(!file.exists(t.outfolder)) dir.create(t.outfolder)
    f.cf <- file.path(t.outfolder,file)
    if(!file.exists(f.cf)){ #file.copy(f,f.cf)
      
      # paste("ncks -x -v", paste0(rm.vars,collapse = ","), f.cf, f.cf)
      
      # Change to CF variable names 
      
      nc1 <- nc_open(f,write=TRUE)
      nc.vars <-  nc.get.variable.list(nc1)
      
      lat <- ncdim_def(name='latitude', units='', vals=1:1, create_dimvar=FALSE)
      lon <- ncdim_def(name='longitude', units='', vals=1:1, create_dimvar=FALSE)
      time_units <-paste0("hours/2",unlist(strsplit( nc1$var$TIMEstp$units, "timesteps"))[2] )
      time <- ncdim_def(name='time', units=time_units, vals=nc1$dim$tstep$vals, create_dimvar=TRUE, unlim=TRUE)
      dim=list(lat,lon,time)
      
      
      var <- ncvar_def(name="latitude",
                       units="degree_north",
                       dim=list(lat,lon), missval=as.numeric(-9999))
      
      nc2 <- nc_create(filename=f.cf, vars=var, verbose=TRUE)
      
      ncvar_put(nc=nc2, varid='latitude', vals=ncvar_get(nc1,"nav_lat"))
      
      # copy lon attribute to longitude
      var <- ncvar_def(name="longitude",
                       units="degree_east",
                       dim=list(lat,lon), missval=as.numeric(-9999))
      nc2 <- ncvar_add(nc=nc2, v=var, verbose=TRUE)
      ncvar_put(nc=nc2, varid='longitude', vals=ncvar_get(nc1,"nav_lon"))
      
      
      # convert wind speed and wind direction to eastward_wind and northward_wind
      wd <- 0 # wind direction - not specified so I set to 0???
      ws <- ncvar_get(nc=nc1, varid='Wind') #wind speed
      ew <- ws * cos(wd * (pi/180))
      nw <- ws * sin(wd * (pi/180))
      
      var <- ncvar_def(name='eastward_wind', units='m/s', dim=dim, missval=-6999.0, verbose=FALSE)
      nc2 <- ncvar_add(nc=nc2, v=var, verbose=FALSE)
      ncvar_put(nc=nc2, varid='eastward_wind', vals=ew)
      
      var <- ncvar_def(name='northward_wind', units='m/s', dim=dim, missval=-6999.0, verbose=FALSE)
      nc2 <- ncvar_add(nc=nc2, v=var, verbose=FALSE)
      ncvar_put(nc=nc2, varid='northward_wind', vals=nw)
      
      #       # remove the unwanted treatment 
      #       if(treatment == "a"){rm.vars <- c("eCO2", "eO3")
      #       }else if (treatment == "e"){rm.vars <- c("aCO2", "aO3")
      #       }else{logger.error("Need a CO2 levels treatment")}
      #       vars <- setdiff(vars_all, rm.vars)
      
      
      # convert CO2 to mole_fraction_of_carbon_dioxide_in_air
      copyvals(nc1=nc1, var1= paste0(treatment, 'CO2'),
               nc2=nc2, var2='mole_fraction_of_carbon_dioxide_in_air', units2='mole/mole', dim2=dim, 
               conv=function(x) { x * 1e6 }, verbose=verbose)
      
      # deal with the rest of the variables
      
      vars <- c("Rainf", "Tair", "RH", "VPD", "Qair", "Wind", "SWdown", "PAR", "LWdown", "Psurf", paste0(treatment,"O3"), "SolarElevation")
      
      
      
      nvars <- c("precipitation_flux", "air_temperature", "relative_humidity", 
                 "water_vapor_saturation_deficit", "specific_humidity", "wind_speed", 
                 "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_photosynthetic_radiative_flux_in_air", 
                 "surface_downwelling_longwave_flux_in_air", "air_pressure", 
                 "mass_concentration_of_ozone_in_air", "solar_elevation_angle")
      
      if(!(length(nvars) == length(vars))){logger.error("Variable mismatch")}
      
      l <- length(vars)
      for (k in 1:l){   
        if(vars[k] %in% nc.vars){#nc <- tncar_rename(nc,vars[k],nvars[k])
          
          vals <- ncvar_get(nc1, vars[k])
          
          
          units <- ncatt_get(nc1, varid=vars[k], attname='units', verbose=FALSE)$value
          
          var <- ncvar_def(name=nvars[k], units=units, dim=dim, verbose=FALSE)
          nc2 <- ncvar_add(nc=nc2, v=var, verbose=TRUE)
          ncvar_put(nc=nc2, varid=nvars[k], vals=vals)
          
          att <- ncatt_get(nc1, vars[k], 'long_name')
          if (att$hasatt) {
            val <- att$value
            ncatt_put(nc=nc2, varid=nvars[k], attname='long_name', attval=val)
          }
        }
      }
      
      nc_close(nc2)
      
      # Split into annual files 
      
      year <- ncvar_get(nc1, 'YEAR')
      y <- year[1]:year[length(year)]
      n <- length(y)
      t <- -1 
      for(j in 1:n){
        new.file <- paste0(t.outfolder,in.prefix,".",y[j],".nc")
        if(!file.exists(new.file)){ 
          s <- t + 1; print(s)
          e <- t + sum(year == y[j]); print(e)      
          if(file.exists(f.cf)==TRUE && file.exists(new.file)==FALSE){
            system(paste0("ncks -d time,",s,",",e," ",f.cf," ",new.file))
          } 
        }
        t <- e
      } 
      print(paste("Treatment ", treatment, " done"))
      nc_close(nc2)
      
    }else{print(paste("Treatment ", treatment, " aleady done"))} # end make new file
    
  } # end loop over treatments
  
}

