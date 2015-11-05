load.netcdf <- function(path,vars,units,newunits){
  
  print(paste("Years: ",start.year," - ",end.year),sep="")
  result <- list()
  for(ncfile in ncfiles) {
    nc <- nc_open(ncfile)
    for(i in i:length(vars)){
      v <- vars[i]
      u1 <- units[i]
      u2 <- newunits[i]
      if(v %in% c(names(nc$var),names(nc$dim))){
        newresult <- ncvar_get(nc, v)
        newresult <- ud.convert(newresult, u1, u2)
        result[[v]] <- abind(result[[v]], newresult)
      } else if (!(v %in% names(nc$var))){
        logger.warn(paste(v, "missing in", ncfile))
      }
    }
    nc_close(nc)
  }
  
  print(paste("----- Mean ", variables, " : ",
              lapply(result, mean, na.rm = TRUE)))
  print(paste("----- Median ", variables, ": ",
              lapply(result, median, na.rm = TRUE)))
  
  
  
}