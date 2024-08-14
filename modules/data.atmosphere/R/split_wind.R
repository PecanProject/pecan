#' Split wind_speed into eastward_wind and northward_wind
#'
#' Currently modifies the files IN PLACE rather than creating a new copy of the files an a new DB record. 
#'
#' @param in.path     path to original data
#' @param in.prefix   prefix of original data
#' @param start_date,end_date date (or character in a standard date format). Only year component is used.
#' @param overwrite logical: replace output file if it already exists? 
#' @param verbose logical: should \code{\link[ncdf4:ncdf4-package]{ncdf4}} functions print debugging information as they run? 
#' @param ... other arguments, currently ignored
#'
#' @return nothing. TODO: Return data frame summarizing results
#' @export
#'
#'
#' @examples
#' \dontrun{
#' in.path    <- "~/paleon/PalEONregional_CF_site_1-24047/"
#' in.prefix  <- ""
#' outfolder  <- "~/paleon/metTest/"
#' start_date <- "0850-01-01"
#' end_date   <- "2010-12-31"
#' overwrite  <- FALSE
#' verbose    <- TRUE
#' 
#' split_wind(in.path, in.prefix, start_date, end_date, merge.file, overwrite, verbose)
#' }
split_wind <- function(in.path, in.prefix, start_date, end_date,
                               overwrite = FALSE, verbose = FALSE, ...){
  
  # get start/end year code works on whole years only
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  if(nchar(in.prefix)>0) in.prefix <- paste0(in.prefix, ".")
  
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
    year_txt <- formatC(year, width = 4, format = "d", flag = "0")
    old.file <- file.path(in.path, paste0(in.prefix, year_txt, ".nc"))
#    new.file <- file.path(outfolder, paste(in.prefix, year, "nc", sep = "."))
    
    ## open target file
    nc <- ncdf4::nc_open(old.file, write = TRUE)
    
    if("eastward_wind" %in% names(nc$var)) {
      PEcAn.logger::logger.info("eastward_wind already exists", year_txt)
      ncdf4::nc_close(nc)
      next
    }
    if(!("wind_speed" %in% names(nc$var))) {
      PEcAn.logger::logger.error("wind_speed does not exist", year_txt)
      ncdf4::nc_close(nc)
      next
    }

    
    ##extract wind_speed & direction
    wind_speed <- ncdf4::ncvar_get(nc, "wind_speed")
    wind_speed.attr <- ncdf4::ncatt_get(nc, "wind_speed")
    WD <- "wind_direction" %in% names(nc$var)
    if(WD){
      wind_dir <- pi/2 - PEcAn.utils::ud_convert(ncdf4::ncvar_get(nc, "wind_direction"), wind_dir$units, "radians")
      wind_dir.attr <- ncdf4::ncatt_get(nc, "wind_direction")
      east <- wind_speed*cos(wind_dir)
      north <- wind_speed*sin(wind_dir)
    } else {
      east <- wind_speed
      north <- 0*wind_speed
    }
    
    ## get dimensions [latitude, longitude, time]
    dims <- list(nc$dim$latitude, nc$dim$longitude, nc$dim$time)
    
    ## insert east
    eastward <- ncdf4::ncvar_def(name = "eastward_wind", units = wind_speed.attr$units, dim = dims, 
                     missval = wind_speed.attr$`_FillValue`, verbose = verbose)
    nc <- ncdf4::ncvar_add(nc = nc, v = eastward, verbose = verbose)
    ncdf4::ncvar_put(nc = nc, varid = "eastward_wind", vals = east)
    
    ## insert north
    northward <- ncdf4::ncvar_def(name = "northward_wind", units = wind_speed.attr$units, dim = dims, 
                                 missval = wind_speed.attr$`_FillValue`, verbose = verbose)
    nc <- ncdf4::ncvar_add(nc = nc, v = northward, verbose = verbose)
    ncdf4::ncvar_put(nc = nc, varid = "northward_wind", vals = north)
    
    ## close file
    ncdf4::nc_close(nc)
    
  } ## end loop over year
  
}
