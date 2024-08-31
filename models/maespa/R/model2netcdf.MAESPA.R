##' Convert MAESPA output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.MAESPA
##' @title Code to convert MAESPA's output into netCDF format
##'
##' @param outdir Location of MAESPA output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param stem_density Number of trees/plotsize. Values in trees.dat
##' @export
##'
##' @author Tony Gardella
model2netcdf.MAESPA <- function(outdir, sitelat, sitelon, start_date, end_date, stem_density) {
  

  ### Read in model output using Maeswrap. Dayflx.dat, watbalday.dat
  dayflx.dataframe    <- Maeswrap::readdayflux(filename = "Dayflx.dat")
  watbalday.dataframe <- Maeswrap::readwatbal(filename = "watbalday.dat")
  
  # moles of Carbon to kilograms
  mole2kg_C <- 0.0120107
  # Seconds in a day
  secINday <- 60 * 60 * 24
  
  ### Determine number of years and output timestep
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  years <- start_year:end_year
  
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Set up outputs for netCDF file in appropriate units
    output <- list()
    output[[1]] <- (dayflx.dataframe$totPs) * mole2kg_C * stem_density  # (GPP) gross photosynthesis. mol tree-1 d-1 -> kg C m-2 s-1
    output[[2]] <- (dayflx.dataframe$netPs) * mole2kg_C * stem_density  # (NPP) photosynthesis net of foliar resp mol tree-1 d-1 -> kg C m-2 s-1
    output[[3]] <- (watbalday.dataframe$et)/secINday  # (Tveg) modeled canopy transpiration   mm -> kg m-2 s-1
    output[[4]] <- (watbalday.dataframe$qh) * 1e+06  # (Qh) Sensible heat flux MJ m-2 day-1 -> W m-2
    output[[5]] <- (watbalday.dataframe$qe) * 1e+06  # (Qle)latent Heat flux MJ m-2 day-1 -> W m-2
    
    # ******************** Declare netCDF variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                   units = paste0("days since ", y, "-01-01 00:00:00"), 
                   vals = (dayflx.dataframe$DOY), 
                   calendar = "standard", 
                   unlim = TRUE)
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var      <- list()
    nc_var[[1]] <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[2]] <- PEcAn.utils::to_ncvar("NPP",dims)
    nc_var[[3]] <- PEcAn.utils::to_ncvar("TVeg", dims)
    nc_var[[4]] <- PEcAn.utils::to_ncvar("Qh", dims)
    nc_var[[5]] <- PEcAn.utils::to_ncvar("Qle", dims)
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  }  ### End of year loop
  
}  ### End of function  
