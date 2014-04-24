##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
<<<<<<< HEAD
<<<<<<< HEAD
##' @title met2CF
=======
##' @title 
>>>>>>> fe95300c67e65ee009f6cbb87485d8e24ffe9431
=======
##' @title met2CF
>>>>>>> d4a680c86b674047e958d780b7047b61a5f31152
##' @export
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' 
##' @author Josh Mantooth, Mike Dietze
met2CF.Ameriflux <- function(in.path,in.prefix,outfolder){
  #---------------- Load libraries. -----------------------------------------------------------------#
#  require(PEcAn.all)
#  require(RPostgreSQL)
  require(ncdf4)
  #--------------------------------------------------------------------------------------------------#  
  
  ## get file names
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    ## send warning
    
    return(NULL)
  }  
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  for(i in 1:length(files)){
    
    new.file =file.path(outfolder,files[i])
    
    ## copy old file to new directory
    system2("cp",paste(file.path(in.path,files[i]),new.file))
    
    ### if reading ameriflux .nc file ###
    nc <- nc_open(new.file,write=TRUE)
    
    #renaming variables and performing unit conversions
    ta <- ncvar_get(nc=nc,varid='TA')
    ta.k <- which(ta > -6999) #select non-missing data
    ta.new <- ta[ta.k] + 273.15 #change units from Celsius to Kelvin
    ta <- replace(x=ta,list=ta.k,values=ta.new) #insert Kelvin values into vector
    
    ncvar_put(nc=nc, varid='TA',vals=ta)
    ncatt_put(nc=nc,varid='TA',attname='units',attval='degrees K') 
    #can do the same for long_name but server that hosts netCDF 
    #CF standard names is down
    nc <- ncvar_rename(nc=nc,'TA','air_temperature')
    
    #convert wind speed and wind direction to U and V
    ws <- ncvar_get(nc=nc,varid='WD') #wind direction
    wd <- ncvar_get(nc=nc,varid='WS') #wind speed
    sub <- which(ws > -6999 & wd > -6999)
    w.miss <- pmin(ws[-sub],wd[-sub])
    wd.sub <- wd[sub] #use wind direction coincident with windspeed
    u = v = rep(NA,length(ws))
    u[sub] <- ws[sub]*cos(wd.sub*(pi/180))
    v[sub] <- ws[sub]*sin(wd.sub*(pi/180))
    u[-sub] = w.miss
    v[-sub] = w.miss
    nc <- ncvar_rename(nc=nc,'WS','wind_speed') #CF name
    nc <- ncvar_rename(nc=nc,'WD','wind_direction') #CF name
    
    #create u and v variables and insert into file
    tdim = nc$dim[["DTIME"]]
    u.var <- ncvar_def(name='eastward_wind',units='m/s',dim=list(tdim)) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=u.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='eastward_wind',vals=u)
    
    v.var <- ncvar_def(name='northward_wind',units='m/s',dim=list(tdim)) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=v.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='northward_wind',vals=v)
   
    #convert air pressure to CF standard
    press <- ncvar_get(nc=nc,varid="PRESS")
    press.pa <- which(press > -6999)
    press.new <- press[press.pa] * 1000 #kilopascals to pascals
    press <- replace(x=press,list=press.pa,values=press.new)
    ncvar_put(nc=nc, varid='PRESS',vals=press)
    ncatt_put(nc=nc,varid='PRESS',attname='units',attval='Pa') 
    nc <- ncvar_rename(nc=nc,'PRESS','air_pressure')
    
    nc <- ncvar_rename(nc=nc,'Rg','surface_downwelling_shortwave_flux')
    nc <- ncvar_rename(nc=nc,'Rgl','surface_downwelling_longwave_flux')
    
    #convert precipitation to CF standard
    dtime <- ncvar_get(nc=nc,varid="DTIME")
    min <- 0.02083/30 #0.02083 DTIME = 30 minutes
    timestep <- round(x=mean(diff(dtime))/min,digits=1) #round to nearest 0.1 minute
    prec <- ncvar_get(nc=nc,varid="PREC")
    prec.sub <- which(prec > -6999)
    prec.new <- prec[prec.sub]/timestep/60 #mm/s = kg/m2/s
    prec <- replace(x=prec,list=prec.sub,values=prec.new)
    ncvar_put(nc=nc, varid='PREC',vals=prec)
    ncatt_put(nc=nc,varid='PREC',attname='units',attval='Kg/m^2/s') 
    nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
    
    
    # convert RH to SH
    rh <- ncvar_get(nc=nc,varid='RH')
    rh.sub <- which(rh > -6999) #select non-missing data
    rh.sh <- as.vector(rh.sub/100) #percent to proportion: needed for conversion
    rh.sh <- replace(x=rh,list=rh.sub,values=rh.sh) #insert proportion values into RH vector
    ta.rh <- ta[rh.sub] # use T coincident with RH
    sh.miss <- rh2rv(rh=rh.sh[rh.sub],T=ta.rh) #conversion, doesn't include missvals
    sh <- replace(x=rh,list=rh.sub,values=sh.miss) #insert Kelvin values into vector
    sh.var <- ncvar_def(name='surface_specific_humidity',units='kg/kg',dim=list(tdim)) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=sh.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='specific_humidity',vals=sh)
    nc <- ncvar_rename(nc=nc,'RH','relative_humidity')
    

    nc_close(nc)
  
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

<<<<<<< HEAD
<<<<<<< HEAD
  
=======
  
>>>>>>> fe95300c67e65ee009f6cbb87485d8e24ffe9431
=======
  
>>>>>>> d4a680c86b674047e958d780b7047b61a5f31152
