##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the PEcAn standar
##' 
##' @name model2netcdf.JULES
##' @title Code to convert JULES output into netCDF format
##'
##' @param outdir Location of model output
##' @export
##' @author Michael Dietze
model2netcdf.JULES <- function(outdir){
  library(ncdf4)
  files = dir(outdir,pattern=".nc",full.names=TRUE)
  files = files[-grep(pattern = "dump",files)]
  print(files)
  for(fname in files){
    print(fname)
    nc = nc_open(fname,write = TRUE)
    ## extract variable and long names
    write.table(sapply(nc$var,function(x){x$longname}),file = paste0(fname,".var"),col.names=FALSE,row.names = TRUE,quote=FALSE)
    ## JULES time is in seconds, convert to DOY  
    time = ncvar_get(nc,"time")/86400
    ncvar_put(nc,"time",time)
    nc_close(nc)
  }
}
