##' Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
##'
##' @name met2CF.Ameriflux
##' @title 
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
    nc <- ncvar_rename(nc=nc,'WS','wind_speed')
    
    press <- ncvar_get(nc=nc,varid="PRESS")
    press.pa <- which(press > -6999)
    press.new <- press[press.pa] * 1000 #kilopascals to pascals
    press <- replace(x=press,list=press.pa,values=press.new)
    ncvar_put(nc=nc, varid='PRESS',vals=press)
    ncatt_put(nc=nc,varid='PRESS',attname='units',attval='Pa') 
    
    nc <- ncvar_rename(nc=nc,'PRESS','air_pressure')
    nc <- ncvar_rename(nc=nc,'Rg','surface_downwelling_shortwave_flux')
    nc <- ncvar_rename(nc=nc,'Rgl','surface_downwelling_longwave_flux')
    nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
    
    # SH still hasn't been calculated
    # cannot delete variables using ncdf4
    # can build new files, but need to recreate the dimensions, variables and then create file
    # can we just delete what we don't need using nco?
#     nc.new <- nc_create(filename=paste(in.prefix,02004,sep="."),
#               vars=c(ncvar_get(nc=nc,varid='air_temperature'),
#                      ncvar_get(nc=nc,varid='wind_speed'),
#                      ncvar_get(nc=nc,varid='air_pressure'),
#                      ncvar_get(nc=nc,varid='surface_downwelling_shortwave_flux'),
#                      ncvar_get(nc=nc,varid='surface_downwelling_longwave_flux'),
#                      ncvar_get(nc=nc,varid='precipitation_flux'),
#                      ncvar_get(nc=nc,varid='YEAR'),
#                      ncvar_get(nc=nc,varid='GAP'),
#                      ncvar_get(nc=nc,varid='DTIME'),
#                      ncvar_get(nc=nc,varid='DOY'),
#                      ncvar_get(nc=nc,varid='HRMIN'),
#                      ncvar_get(nc=nc,varid='UST'),
#                      ncvar_get(nc=nc,varid='WD'),
#                      ncvar_get(nc=nc,varid='TS1'),
#                      ncvar_get(nc=nc,varid='TSdepth1'),
#                      ncvar_get(nc=nc,varid='TS2'),
#                      ncvar_get(nc=nc,varid='TSdepth2'),
#                      ncvar_get(nc=nc,varid='RH'),
#                      ncvar_get(nc=nc,varid='VPD'),
#                      ncvar_get(nc=nc,varid='SWC1'),
#                      ncvar_get(nc=nc,varid='SWC2'),
#                      ncvar_get(nc=nc,varid='PAR')
#                      ))
  
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

  