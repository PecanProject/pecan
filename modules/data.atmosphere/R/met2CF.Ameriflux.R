##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
##' @title met2CF.Ameriflux
##' @export
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' 
##' @author Josh Mantooth, Mike Dietze, Elizabeth Cowdery
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
    
    old.file =file.path(in.path,files[i])
    #open raw ameriflux
    nc <- nc_open(old.file,write=TRUE)
    
    #get variables to be copied to new file
    cp.vars <- list()
    vars<-c('TA','WD','WS','PRESS','Rg','Rgl','PREC','RH','PAR','Rn','TS1','TS2','VPD','CO2','DTIME','APARpct','ZL')
    for(j in 1:length(vars)){
      cp.vars[[j]] <- ncvar_get(nc,varid=vars[j])
    }
    
    #get variable attributes
    cp.atts <- list()
    for(j in 1:length(vars)){
    cp.atts[[j]] <- ncatt_get(nc,varid=vars[j])
    }
    
    #get global attributes
    cp.global.atts <- ncatt_get(nc, varid=0)
    
    #get dimension
    tdim = nc$dim[["DTIME"]]
    name <- files[i] #used to create time dimension
    
    #create new coordinate dimensions based on site location lat/lon
    lat <- ncdim_def(name='latitude',units='',vals=1:1,create_dimvar=FALSE)
    lon <- ncdim_def(name='longitude',units='',vals=1:1,create_dimvar=FALSE)
    time <- ncdim_def(name='time',units='',vals=tdim$vals,create_dimvar=TRUE,unlim=TRUE,longname=name)
    
    #define variables to be inserted to new file
    cp.vars.3d <- list()
    for(j in 1:length(vars)){
    cp.vars.3d[[j]] <- ncvar_def(name=vars[[j]],units=cp.atts[[j]]$units,dim=(list(lat,lon,time)),missval=as.numeric(-9999)) 
    }   
    
    #create cf ameriflux with correct name
    name_list1 <- unlist(strsplit(name, "_"))[c(-3,-6)]
    name_list2 <- unlist(strsplit(tail(unlist(strsplit(name, "_")),1),"[.]"))
    date <- unlist(strsplit(name, "_"))[3]
    
    nn <- paste0(c(name_list1,name_list2[1]), collapse ="_")
    new.file <- paste(c(nn,date,name_list2[2]), collapse=".")
    
    new.file.rename =file.path(outfolder,new.file)
    
    #create new file
    nc2<-nc_create(filename = new.file.rename, vars = cp.vars.3d)
    
    for(j in 1:length(vars)){
    ncvar_put(nc=nc2, varid = vars[j], vals = cp.vars[[j]])
    }
    
    #close old file & new file
    nc_close(nc)
    nc_close(nc2) 

    #re-open new file to make write-able
    nc2 <- nc_open(new.file.rename,write=TRUE)

    #insert variable attributes
    for(j in 1:length(vars)){
      for(k in 1:length(cp.atts[[1]])){ #all elements have the same number of attributes
    ncatt_put(nc2, varid = vars[j], attname = names(cp.atts[[j]])[k], attval = as.character(cp.atts[[j]][k])) #attnames
      }
    }
 
   #insert global attributes
   for(j in 1:length(cp.global.atts)){
   ncatt_put(nc2, varid=0, attname = names(cp.global.atts)[j], attval = cp.global.atts[[j]])
   }
   
  
    #renaming variables and performing unit conversions
    ta <- ncvar_get(nc=nc2,varid='TA')
    ta.k <- which(ta > -6999) #select non-missing data
    ta.new <- ta[ta.k] + 273.15 #change units from Celsius to Kelvin
    ta <- replace(x=ta,list=ta.k,values=ta.new) #insert Kelvin values into vector
    nc2 <- ncvar_rename(nc=nc2,'TA','air_temperature')
    ncatt_put(nc=nc2,varid='air_temperature',attname='units',attval='degrees K')
    ncvar_put(nc=nc2,varid='air_temperature',vals=ta)
    
    #convert wind speed and wind direction to U and V
    wd <- ncvar_get(nc=nc2,varid='WD') #wind direction
    ws <- ncvar_get(nc=nc2,varid='WS') #wind speed
    sub <- which(ws > -6999 & wd > -6999)
    w.miss <- pmin(ws[-sub],wd[-sub])
    wd.sub <- wd[sub] #use wind direction coincident with windspeed
    u = v = rep(NA,length(ws))
    u[sub] <- ws[sub]*cos(wd.sub*(pi/180))
    v[sub] <- ws[sub]*sin(wd.sub*(pi/180))
    u[-sub] = w.miss
    v[-sub] = w.miss
    nc <- ncvar_rename(nc=nc2,'WS','wind_speed') #CF name
    nc <- ncvar_rename(nc=nc2,'WD','wind_direction') #CF name
    
    #create u and v variables and insert into file
    u.var <- ncvar_def(name='eastward_wind',units='m/s',dim=list(lat,lon,time),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc2,v=u.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='eastward_wind',vals=u)
    
    v.var <- ncvar_def(name='northward_wind',units='m/s',dim=list(lat,lon,time),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc2,v=v.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='northward_wind',vals=v)
   
    #convert air pressure to CF standard
    press <- ncvar_get(nc=nc2,varid="PRESS")
    press.pa <- which(press > -6999)
    press.new <- press[press.pa] * 1000 #kilopascals to pascals
    press <- replace(x=press,list=press.pa,values=press.new)

    nc2 <- ncvar_rename(nc=nc2,'PRESS','air_pressure')
    ncatt_put(nc=nc2,varid='air_pressure',attname='units',attval='Pa')
    ncvar_put(nc=nc2,varid='air_pressure',vals=press)
   
    #p.var <- ncvar_def(name='air_pressure',units='Pa',dim=list(lat,lon,time),missval= -9999)
    #nc = ncvar_add(nc=nc2,v=p.var,verbose=TRUE) #add variable to existing netCDF file
    #ncvar_put(nc,varid='air_pressure',vals=press)
    
    nc <- ncvar_rename(nc=nc2,'Rg','surface_downwelling_shortwave_flux')
    nc <- ncvar_rename(nc=nc2,'Rgl','surface_downwelling_longwave_flux')
    
    #convert precipitation to CF standard
    t <- ncvar_get(nc=nc2,varid="time")
    min <- 0.02083/30 #0.02083 time = 30 minutes
    timestep <- round(x=mean(diff(t))/min,digits=1) #round to nearest 0.1 minute
    prec <- ncvar_get(nc=nc2,varid="PREC")
    prec.sub <- which(prec > -6999)
    prec.new <- prec[prec.sub]/timestep/60 #mm/s = kg/m2/s
    prec <- replace(x=prec,list=prec.sub,values=prec.new)
    ncvar_put(nc=nc2, varid='PREC',vals=prec)
    ncatt_put(nc=nc2,varid='PREC',attname='units',attval='kg/m^2/s') 
    nc <- ncvar_rename(nc=nc2,'PREC','precipitation_flux')
    
    # convert RH to SH
    rh <- ncvar_get(nc=nc2,varid='RH')
    rh.sub <- which(rh > -6999) #select non-missing data
    rh.sh <- as.vector(rh.sub/100) #percent to proportion: needed for conversion
    rh.sh <- replace(x=rh,list=rh.sub,values=rh.sh) #insert proportion values into RH vector
    ta.rh <- ta[rh.sub] # use T coincident with RH
    sh.miss <- rh2qair(rh=rh.sh[rh.sub],T=ta.rh) #conversion, doesn't include missvals. was rh2rv
    sh <- replace(x=rh,list=rh.sub,values=sh.miss) #insert Kelvin values into vector
    sh.var <- ncvar_def(name='specific_humidity',units='kg/kg',dim=list(lat,lon,time),missval= -9999) #define netCDF variable, doesn't include longname and comments
    nc = ncvar_add(nc=nc2,v=sh.var,verbose=TRUE) #add variable to existing netCDF file
    ncvar_put(nc,varid='specific_humidity',vals=sh)
    ncatt_put(nc=nc2,varid='RH',attname='units',attval='percent') 
    nc <- ncvar_rename(nc=nc2,'RH','relative_humidity')
    
    # fixing APARpct
    ncatt_put(nc=nc2,varid='APARpct',attname='units',attval='percent') 
    # fixing ZL
    ncatt_put(nc=nc2,varid='ZL',attname='units',attval='m/m')
    
    #get site location attribute
    loc <- ncatt_get(nc=nc2,varid=0,attname='site_location')
    lat.value <- rep(as.numeric(substr(loc$value,20,28)),nc2$dim$time$len)
    lon.value <- rep(as.numeric(substr(loc$value,40,48)),nc2$dim$time$len)
    
    #create site location variables
    lat.var <- ncvar_def(name='latitude',units='degree_north',dim=list(lat,lon,time),missval=-9999) 
    nc <- ncvar_add(nc=nc2,v=lat.var,verbose=TRUE) #add latitude to existing netCDF file
    ncvar_put(nc,varid='latitude',vals=lat.value)
    
    lon.var <- ncvar_def(name='longitude',units='degree_east',dim=list(lat,lon,time),missval=-9999) 
    nc <- ncvar_add(nc=nc2,v=lon.var,verbose=TRUE) #add longitude to existing netCDF file
    ncvar_put(nc,varid='longitude',vals=lon.value)
      

    nc_close(nc2)
    
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

