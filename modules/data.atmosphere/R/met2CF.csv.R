##' @name met2CF.csv
##' @title met2CF.csv
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param format data frame or list with elements orig, bety, units for the original variable name, bety variable name, and original units
##' @author Mike Dietze
##' 

if(FALSE){
  in.path = "~/Downloads/"
  in.file = "WR_E"
  outfolder = "/tmp/"
  format = data.frame(orig=c("TA","PRECIP"),units=c("celsius","mm"),bety=c("airT","precipitation_flux"))  
  lat = 40
  lon = -80
}

met2CF.csv <- function(in.path,in.file,outfolder,format,lat=NULL,lon=NULL){
  debug=TRUE
  require(ncdf4)
  require(udunits2)
  require(PEcAn.utils)

  files = dir(in.path,in.file,full.names=TRUE)
  files = files[grep("*.csv",files)]
  if(length(files)==0) return(NULL)
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  
  for(i in 1:length(files)){
    
    new.file =file.path(outfolder,sub(".csv","_CF.nc",basename(files[i])))
  
    ### if reading ameriflux .csv file ###
#    dat <- read.csv(files[i],skip=17,na.strings=c(-9999,-6999)) #example file
    dat <- read.csv(files[i],skip=7,na.strings=c(-9999,-6999,9999),as.is=TRUE) #example file
    units <- dat[1,]
    dat <- as.data.frame(sapply(dat[-1,],as.numeric))
    
    ### time 
## HACK!!!
    tdim = ncdim_def("time","day of year",seq(0,365,length=nrow(dat))) #define netCDF dimensions for variables
    timestep = round(diff(tdim$vals)[1]*86400)


    ## create new netCDF file
    x = ncdim_def("Lon","degreesE",lon) #define netCDF dimensions for variables
    y = ncdim_def("Lat","degreesN",lat)
    co2.var = ncvar_def(name="CO2",units="ppm",dim=list(x,y),verbose=debug)
    nc = nc_create(new.file, vars=co2.var,verbose=debug) #create netCDF file

    ## air_temperature / airT
    if("airT" %in% format$bety){
      k = which(format$bety=="airT")
      airT.var = ncvar_def(name="air_temperature",units="K",dim=tdim,verbose=debug)
      nc = ncvar_add(nc=nc,v=airT.var,verbose=debug) #add variable to existing netCDF file
      ncvar_put(nc,varid='air_temperature',
                vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"celsius","K"))  
    }

    ## air_pressure / Psurf
    if("air_pressure" %in% format$bety){
      k = which(format$bety=="air_pressure")
      Psurf.var = ncvar_def(name="air_pressure",units="Pa",dim=tdim,verbose=debug)
      nc = ncvar_add(nc=nc,v=Psurf.var,verbose=debug) #add variable to existing netCDF file
      ncvar_put(nc,varid='air_pressure',
            vals=met.conv(dat[,as.character(format$orig[k])],format$units[k],"Pa","Pa"))
  
    }

    ## precipitation_flux / rain
    if("precipitation_flux" %in% format$bety){
      ## units preprocessing
      rain = dat[,as.character(format$orig[k])]
      rain.units = as.character(format$units[k])
      rain.units = switch(rain.units,
             mm = {rain=rain/timestep;"kg/m2/s"},
             m  = {rain=rain/timestep;"Mg/m2/s"},
             'in' = {rain=ud.convert(rain/timestep,"in","mm");"kg/m2/s"}
      )        
         
      ## insert
      k = which(format$bety=="precipitation_flux")
      rain.var = ncvar_def(name="precipitation_flux",units="kg/m2/s",dim=tdim,verbose=debug)
      nc = ncvar_add(nc=nc,v=rain.var,verbose=debug) #add variable to existing netCDF file
      ncvar_put(nc,varid='precipitation_flux',
            vals=met.conv(rain,rain.units,"kg/m2/s","kg/m2/s"))  

    }

    uwind  = ncvar_def(name="eastward_wind",units="m s-1",dim) #define netCDF variables
    sh.var <- ncvar_def(name='surface_specific_humidity',units='kg/kg',dim=list(tdim)) #define netCDF variable, doesn't include longname and comments
    
    
    nc_close(nc)
    
  } ## end loop over files
    
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