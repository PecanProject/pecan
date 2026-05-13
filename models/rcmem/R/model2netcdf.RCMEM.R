##' Convert PEPRMT output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.PEPRMT
##' @title Function to convert PEPRMT model output to standard netCDF format
##' @param outdir Location of PEPRMT model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param delete_raw logical: remove out.csv after converting?
##' @export
##' @author Abigail Lewis

model2netcdf.PEPRMT <- function(outdir, sitelat, sitelon, start_date, end_date, delete_raw = FALSE) {
  runid <- basename(outdir)
  raw_output <- file.path(outdir, "out.csv")
  
  ### Read in model output in PEPRMT format
  PEPRMT.output      <- utils::read.csv(raw_output)
  PEPRMT.output.dims <- dim(PEPRMT.output)
  
  years <- unique(PEPRMT.output$Year)
  
  ### Loop over years in PEPRMT output to create separate netCDF outputs
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  #turn on for debugging
    
    ## Subset data for processing
    sub.PEPRMT.output <- dplyr::filter(PEPRMT.output, .data$Year == y)
    sub.PEPRMT.output.dims <- dim(sub.PEPRMT.output)

    # ******************** Declare netCDF variables ********************#
    start.day <- 1
    if (y == lubridate::year(start_date)){
      start.day <- lubridate::yday(start_date)
    } 
    tvals <- (start.day:sub.PEPRMT.output.dims[1])-1
    bounds <- array(data=NA, dim=c(length(tvals),2))
    bounds[,1] <- tvals
    bounds[,2] <- bounds[,1]+1
    t   <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", y, "-01-01 00:00:00"), 
                     vals = tvals, calendar = "standard", unlim = TRUE)
    ## ***** Need to dynamically update the UTC offset here *****
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    dims <- list(lon = lon, lat = lat, time = t)
    time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                      longname="history time interval endpoint dimensions",
                                      vals = 1:2, units="")
    
    ## Output names
    # SOM_total (g C  m^-3) - 1m depth boundary
    # SOM_labile (g C  m^-3)
    # GPP_mod (gC m-2 day-1)
    # Reco_mod (gC m-2 day-1)
    # NEE_mod (gC m-2 day-1)
    # CH4_mod (gC m-2 day-1)
    
    fluxes <- c("CH4_mod", "GPP_mod", "Reco_mod", "NEE_mod")
    pools <- c("S1", "S2")
    sub.PEPRMT.output <- sub.PEPRMT.output[c(fluxes, pools)]
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## Fluxes
    output[[1]] <- (sub.PEPRMT.output[, "CH4_mod"] * 0.001)   # CH4 emission in kgC/m2/s
    output[[2]] <- (sub.PEPRMT.output[, "GPP_mod"] * 0.001)   # GPP in kgC/m2/s
    output[[3]] <- (sub.PEPRMT.output[, "Reco_mod"] * 0.001)  # Reco in kgC/m2/s
    output[[4]] <- (sub.PEPRMT.output[, "NEE_mod"] * 0.001)   # NEE in kgC/m2/s
    
    ## Pools
    output[[5]]  <- (sub.PEPRMT.output[, fluxes[1]] * 0.001)  # Soil Carbon, kgC/m2
    output[[6]]  <- (sub.PEPRMT.output[, fluxes[2]] * 0.001)  # Soil Carbon, kgC/m2
    
    ## time_bounds
    output[[length(fluxes) +
              length(pools) + 
              1]] <- c(rbind(bounds[,1], bounds[,2]))
    
    ## missing value handling
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(t$vals))
    }
    
    ## setup nc file
    # ******************** Declar netCDF variables ********************#
    nc_var <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("CH4_flux", dims)
    nc_var[[2]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[3]]  <- PEcAn.utils::to_ncvar("TotalResp", dims)
    nc_var[[4]]  <- PEcAn.utils::to_ncvar("NEE", dims)
    
    nc_var[[5]]  <- PEcAn.utils::to_ncvar("slow_soil_pool_carbon_content", dims)
    nc_var[[6]]  <- PEcAn.utils::to_ncvar("fast_soil_pool_carbon_content", dims)
    
    nc_var[[7]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                     longname = "history time interval endpoints", dim=list(time_interval,time = t), 
                                     prec = "double")
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
    for (i in seq_along(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
    }
    ncdf4::nc_close(nc)

  }  ### End of year loop

  if (delete_raw) {
    file.remove(raw_output)
  }
} # model2netcdf.PEPRMT
# ==================================================================================================#
## EOF
