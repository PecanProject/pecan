##' @name met2CF.NARR
##' @title met2CF.NARR
##' @export
##' 
##' @param in.path
##' @param in.prefix
##' @param outfolder
##' @param convert NARR files to CF files
##' @author Elizabeth Cowdery, Rob Kooper
##' 
met2CF.NARR <- function(in.path, in.prefix, outfolder) {
  
  require(ncdf4)
  
  system(paste0("mkdir -p ", outfolder))
  
  vars=c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate", "uwnd.10m", "vwnd.10m")
  svars=c("pres", "dswrf", "dlwrf", "air", "shum", "prate", "uwnd", "vwnd")
  cfvars=c("air_pressure", "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", "air_temperature", "specific_humidity", "precipitation_flux", "eastward_wind", "northward_wind" )
  
  years = 1979:2014
  
  for(y in years){
    
    newfile <- paste0(outfolder,"/NARR.",y,".nc")
    
    if (!file.exists(newfile)){
      
      for(i in 1:length(vars)){
        file <- paste0(in.path,"/",vars[i],".",y,".nc")
        newfile <-paste0(outfolder,cfvars[i],".",y,".nc")
        
        if(file.exists(file) & !file.exists(newfile)){
          
          file.copy(file, newfile, overwrite = FALSE)
          nc <- nc_open(newfile, write=TRUE)
          ncvar_rename(nc, svars[i], cfvars[i])
          nc_close(nc)  
        }
      }
    }
    
  }
  
  for(y in years){
    newfile <- paste0(outfolder,"/NARR.",y,".nc")
    
    if (!file.exists(newfile)){
      for(i in 1:length(vars)){
        file <-paste0(outfolder,cfvars[i],".",y,".nc")
        if(file.exists(file)){
          if (!file.exists(newfile)){
            file.copy(file, newfile)
            system(paste("ncks -O --fl_fmt=netcdf4", newfile, newfile))   # netCDF4
            system(paste("ncpdq -O -U" , newfile, newfile)) 
          }else{
            system(paste("ncks -O --fl_fmt=netcdf4", file, file))   # netCDF4
            system(paste("ncpdq -O -U" , file, file)) 
            
            system(paste("ncks -A", file, newfile))
          }
          
          file.remove(file)
        }
        
      }
      nc <- nc_open(newfile, write=TRUE)
      ncvar_rename(nc, "lat", "latitude")
      ncvar_rename(nc, "lon", "longitude")
      nc_close(nc)
    }
  }
}