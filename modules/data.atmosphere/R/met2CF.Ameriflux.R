##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
##' @title met2CF.Ameriflux
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
    
    #time dimension for adding new variables
    tdim = nc$dim[["DTIME"]]
    
    #renaming variables and performing unit conversions
    ta <- ncvar_get(nc=nc,varid='TA')
    #ta <- ncvar_get(nc=nc,varid='air_temperature')
    ta.k <- which(ta > -6999) #select non-missing data
    ta.new <- ta[ta.k] + 273.15 #change units from Celsius to Kelvin
    ta <- replace(x=ta,list=ta.k,values=ta.new) #insert Kelvin values into vector
    
    ta.var <- ncvar_def(name='air_temperature',units='degrees K',dim=list(tdim),missval=-9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=ta.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='air_temperature',vals=ta)
    
    #convert wind speed and wind direction to U and V
    wd <- ncvar_get(nc=nc,varid='WD') #wind direction
    ws <- ncvar_get(nc=nc,varid='WS') #wind speed
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
    u.var <- ncvar_def(name='eastward_wind',units='m/s',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=u.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='eastward_wind',vals=u)
    
    v.var <- ncvar_def(name='northward_wind',units='m/s',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=v.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='northward_wind',vals=v)
   
    #convert air pressure to CF standard
    press <- ncvar_get(nc=nc,varid="PRESS")
    press.pa <- which(press > -6999)
    press.new <- press[press.pa] * 1000 #kilopascals to pascals
    press <- replace(x=press,list=press.pa,values=press.new)

    p.var <- ncvar_def(name='air_pressure',units='Pa',dim=list(tdim),missval= -9999)
    nc = ncvar_add(nc=nc,v=p.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='air_pressure',vals=press)
    
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
    ncatt_put(nc=nc,varid='PREC',attname='units',attval='kg/m^2/s') 
    nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
    
    
    # convert RH to SH
    rh <- ncvar_get(nc=nc,varid='RH')
    rh.sub <- which(rh > -6999) #select non-missing data
    rh.sh <- as.vector(rh.sub/100) #percent to proportion: needed for conversion
    rh.sh <- replace(x=rh,list=rh.sub,values=rh.sh) #insert proportion values into RH vector
    ta.rh <- ta[rh.sub] # use T coincident with RH
    sh.miss <- rh2qair(rh=rh.sh[rh.sub],T=ta.rh) #conversion, doesn't include missvals. was rh2rv
    sh <- replace(x=rh,list=rh.sub,values=sh.miss) #insert Kelvin values into vector
    sh.var <- ncvar_def(name='specific_humidity',units='kg/kg',dim=list(tdim),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc,v=sh.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='specific_humidity',vals=sh)
    ncatt_put(nc=nc,varid='RH',attname='units',attval='percent') 
    nc <- ncvar_rename(nc=nc,'RH','relative_humidity')
    
    # fixing APARpct
    ncatt_put(nc=nc,varid='APARpct',attname='units',attval='percent') 
    # fixing ZL
    ncatt_put(nc=nc,varid='ZL',attname='units',attval='m/m') 
    
    #get site location attribute
    loc <- ncatt_get(nc=nc,varid=0,attname='site_location')
    lat.value <- rep(as.numeric(substr(loc$value,20,28)),tdim$len)
    lon.value <- rep(as.numeric(substr(loc$value,40,48)),tdim$len)
    
    #create new coordinate dimensions based on site location lat/lon
    #lat <- ncdim_def(name='latitude',units='',vals=1:1,create_dimvar=FALSE)
    #lon <- ncdim_def(name='longitude',units='',vals=1:1,create_dimvar=FALSE)
    #lon <- ncdim_def(name='longitude',units='degree_east',vals=lon.value)
    
    #create site location variables
    lat.var <- ncvar_def(name='latitude',units='degree_north',dim=list(tdim),missval=-9999) #dim=list(lat,tdim)
    nc <- ncvar_add(nc=nc,v=lat.var,verbose=TRUE) #add latitude to existing netCDF file
    ncvar_put(nc,varid='latitude',vals=lat.value)
    
    lon.var <- ncvar_def(name='longitude',units='degree_east',dim=list(tdim),missval=-9999) #dim=list(lat,tdim)
    nc <- ncvar_add(nc=nc,v=lon.var,verbose=TRUE) #add longitude to existing netCDF file
    ncvar_put(nc,varid='longitude',vals=lon.value)

    nc_close(nc)
  
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

