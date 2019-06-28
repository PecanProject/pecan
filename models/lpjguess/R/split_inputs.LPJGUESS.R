## split LPJ-GUESS ncdf files into smaller time units to use in KF
##' @author Istem Fer
##' 
##' @param settings PEcAn settings object
##' @param start.time start date and time for each SDA ensemble
##' @param stop.time stop date and time for each SDA ensemble
##' @param inputs list of model inputs to use in write.configs.LPJGUESS
##' @param overwrite Default FALSE
##' @param outpath if specified, write output to a new directory. Default NULL writes back to the directory being read
##' @description Splits climate met for LPJGUESS
##' 
##' @return name of the split met file
##' @export
split_inputs.LPJGUESS <- function(settings, start.time, stop.time, inputs, overwrite = FALSE, outpath = NULL){
  
  #### Lubridate start and end times
  start.day  <- lubridate::yday(start.time)
  start.year <- lubridate::year(start.time)
  end.day    <- lubridate::yday(stop.time)
  end.year   <- lubridate::year(stop.time)
  
  # Whole run period
  run.start <- lubridate::year(settings$run$start.date)
  run.end   <- lubridate::year(settings$run$end.date)
  
  #### Get met paths
  met    <- inputs
  path   <- dirname(met)
  prefix <- substr(basename(met), 1, nchar(basename(met))-16) #assuming we'll always have "PREFIX.1920.2010.tmp"
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath)
  
  var.names  <- c("tmp", "pre", "cld")
  long.names <- c("air_temperature",
                  "precipitation_flux",
                  "surface_downwelling_shortwave_flux_in_air")
  
  # !!! always full years with LPJ-GUESS !!!
  files.in  <- file.path(outpath, paste0(prefix, run.start,  ".", run.end,  ".", var.names, ".nc"))
  files.out <- file.path(outpath, paste0(prefix, start.year, ".", end.year, ".", var.names, ".nc"))
  
  if(file.exists(files.out[1]) & !overwrite){
    return(files.out[1])
  }
  
  ## open netcdf files
  fnc.tmp <- ncdf4::nc_open(files.in[1])
  fnc.pre <- ncdf4::nc_open(files.in[2])
  fnc.cld <- ncdf4::nc_open(files.in[3])
  
  ## read climate data
  nc.tmp <- ncdf4::ncvar_get(fnc.tmp, var.names[1])
  nc.pre <- ncdf4::ncvar_get(fnc.pre, var.names[2])
  nc.cld <- ncdf4::ncvar_get(fnc.cld, var.names[3])

  # cut where
  if(start.year == run.start){
    years <- start.year:end.year
    inds  <- 1:sum(days_in_year(years))
  }else{
    ### come back
  }

  # split
  nc.tmp <- nc.tmp[1,1,inds]
  nc.pre <- nc.pre[1,1,inds]
  nc.cld <- nc.cld[1,1,inds]

  var.list <- list(nc.tmp, nc.pre, nc.cld)
  
  # not that these will be different than "K", "kg m-2 s-1", "W m-2"
  var.units <- c(fnc.tmp$var$tmp$units,
                 fnc.pre$var$pre$units,
                 fnc.cld$var$cld$units)
    
  # get other stuff to be written to ncdf
  
  ## retrieve lat/lon
  lon <- ncdf4::ncvar_get(fnc.tmp, "lon")
  lat <- ncdf4::ncvar_get(fnc.tmp, "lat")

  # write back
  ## write climate data define dimensions
  
  latdim  <- ncdf4::ncdim_def(name = "lat", "degrees_north", as.double(lat))
  londim  <- ncdf4::ncdim_def(name = "lon", "degrees_east", as.double(lon))
  timedim <- ncdf4::ncdim_def("time", paste0("days since ", start.year - 1, "-12-31", sep = ""), as.double(c(1:length(nc.tmp))))
  
  fillvalue <- 9.96920996838687e+36
  
  for (n in seq_along(var.names)) {
    # define variable
    var.def <- ncdf4::ncvar_def(name = var.names[n],
                                units = var.units[n],
                                dim = (list(londim, latdim, timedim)),
                                fillvalue, long.names[n],
                                verbose = FALSE,
                                prec = "float")
    
    # create netCD file for LPJ-GUESS
    ncfile <- ncdf4::nc_create(files.out[[n]], vars = var.def, force_v4 = TRUE)
    

    # put variable, rep(...,each=4) is a hack to write the same data for all grids (which all are the
    # same)
    ncdf4::ncvar_put(ncfile, var.def, rep(var.list[[n]], each = 4))

    
    # additional attributes for LPJ-GUESS
    ncdf4::ncatt_put(nc = ncfile, varid = var.names[n], attname = "standard_name", long.names[n])
    
    ncdf4::ncatt_put(nc = ncfile, varid = "lon", attname = "axis", "X")
    ncdf4::ncatt_put(nc = ncfile, varid = "lon", attname = "standard_name", "longitude")
    
    ncdf4::ncatt_put(nc = ncfile, varid = "lat", attname = "axis", "Y")
    ncdf4::ncatt_put(nc = ncfile, varid = "lat", attname = "standard_name", "latitude")
    
    ncdf4::ncatt_put(nc = ncfile, varid = "time", attname = "calendar", "gregorian")
    
    ncdf4::nc_close(ncfile)
  }
  
  # close nc
  ncdf4::nc_close(fnc.tmp)
  ncdf4::nc_close(fnc.pre)
  ncdf4::nc_close(fnc.cld)
  
  return(files.out[1])
} # split_inputs.LPJGUESS