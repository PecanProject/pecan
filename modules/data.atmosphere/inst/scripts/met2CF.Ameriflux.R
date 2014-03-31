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
    nc <- nc_open(new.file)
      
    nc <- ncvar_rename(nc=nc,'WS','wind_speed')
    nc <- ncvar_rename(nc=nc,'TA','air_temperature')
    nc <- ncvar_rename(nc=nc,'PRESS','air_pressure')
    nc <- ncvar_rename(nc=nc,'Rg','surface_downwelling_shortwave_flux')
    nc <- ncvar_rename(nc=nc,'Rgl','surface_downwelling_longwave_flux')
    nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
  
    ## rename variable longname
    ## nothing in ncdf4 package seems to do this.
    ## may need to rebuild file instead of only renaming variables

    
  
  }  ## end loop over files
 
 
}   ## end netCDF CF conversion ##

  