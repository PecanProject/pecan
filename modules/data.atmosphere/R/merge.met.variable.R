#' Title
#'
#' @param in.path 
#' @param in.prefix 
#' @param outfolder 
#' @param start_date 
#' @param end_date 
#' @param merge.file 
#' @param overwrite 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' in.path    <- "~/paleon/PalEONregional_CF_site_1-24047/"
#' in.prefix  <- ""
#' merge.file <- "~/paleon/paleon_monthly_co2.nc"
#' start_date <- "0850-01-01"
#' end_date   <- "2010-12-31"
#' overwrite  <- FALSE
#' verbose    <- TRUE
merge.met.variable <- function(in.path,in.prefix,start_date, end_date, merge.file,
                               overwrite = FALSE, verbose = FALSE, ...){
  
  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  if(nchar(in.prefix)>0) in.prefix <- paste0(in.prefix,".")
  
  ## open and parse file to be merged in
  merge.nc <- ncdf4::nc_open(merge.file)
  merge.vars <- names(merge.nc$var)
  merge.attr <- ncdf4::ncatt_get(merge.nc,varid = merge.vars[1])
  merge.time <- ncdf4::ncvar_get(merge.nc,"time")
  merge.time.attr <- ncdf4::ncatt_get(merge.nc,"time")
  merge.data <- ncdf4::ncvar_get(merge.nc,varid = merge.vars[1])
  
  udunits2::ud.is.parseable(merge.time.attr$units)
  origin <- "1970-01-01 00:00:00 UTC"
  merge.time.std <- udunits2::ud.convert(merge.time,
                                         merge.time.attr$units,
                                         paste0("seconds since ",origin))
  
  merge.time.std <- as.POSIXct(merge.time.std,tz = "UTC",origin=origin) 
  merge.years <- unique(lubridate::year(merge.time.std))
  
  # check dates
  if(lubridate::year(merge.time.std[1]) > start_year){
    PEcAn.utils::logger.error("merge.time > start_year", merge.time.std[1],start_date)
    ncdf4::nc_close(merge.nc)
    return(NULL)
  }
  if(lubridate::year(tail(merge.time.std,1)) < end_year){
    PEcAn.utils::logger.error("merge.time < end_year", tail(merge.time.std,1),end_date)
    ncdf4::nc_close(merge.nc)
    return(NULL)
  }
  
  # check lat/lon
  merge.dims <- names(merge.nc$dim)
  byLatLon <- FALSE
  if(length(grep("^lat",merge.dims))>0 & length(grep("^lon",merge.dims))>0){
    byLatLon <- TRUE
  }  
  
  ## close merge file
  ncdf4::nc_close(merge.nc)
  
  ## prep data structure for results
  rows <- end_year - start_year + 1
  results <- data.frame(file = character(rows),
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows),
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = in.prefix, 
                        stringsAsFactors = FALSE)
  
  for (year in start_year:end_year) {
    old.file <- file.path(in.path, paste0(in.prefix, year, ".nc"))
#    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep = "."))
    
    ## subset merged data
    merge.sel <- which(lubridate::year(merge.time.std) == year)
    merge.sub <- data.frame(time=merge.time.std[merge.sel],data = merge.data[merge.sel])
    
    ## open target file
    nc <- ncdf4::nc_open(old.file,write = TRUE)
    
    ##extract target time
    target.time <- ncdf4::ncvar_get(nc,"time")
    target.time.attr <- ncdf4::ncatt_get(nc,"time")
    target.time.std <- udunits2::ud.convert(target.time,
                                           target.time.attr$units,
                                           paste0("seconds since ",origin))
    target.time.std <- as.POSIXct(target.time.std,tz = "UTC",origin=origin) 
    
    
    ## interpolate merged data to target time
    merge.interp <- approx(merge.sub$time,merge.sub$data, xout = target.time.std, 
                           rule = 2, method = "linear", ties = mean)
    
    ## insert new variable
    var.merge <- ncdf4::ncvar_def(name = merge.vars[1], units = merge.attr$units, dim = nc$dim$time, 
                     missval = merge.attr$`_FillValue`, verbose = verbose)
    nc <- ncdf4::ncvar_add(nc = nc, v = var.merge, verbose = verbose)
    ncdf4::ncvar_put(nc = nc, varid = merge.vars[1], vals = merge.interp$y)
    
    ## close file
    ncdf4::nc_close(nc)
    
  } ## end loop over year
  
  
  
}