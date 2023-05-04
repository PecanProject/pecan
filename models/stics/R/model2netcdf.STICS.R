#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert STICS output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.STICS
##' @title Code to convert STICS' output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param overwrite Whether or not to overwrite existing output files 
##' @export
##'
##' @author Istem Fer
model2netcdf.STICS <- function(outdir, sitelat, sitelon, start_date, end_date, overwrite = FALSE) {


  ### Read in model output in STICS format
  out_files <- list.files(outdir)
  
  stics_out_file <- file.path(outdir, out_files[grepl("mod_s.*", out_files)])
  stics_output   <- utils::read.table(stics_out_file, header = T, sep = ";")
  
  simulation_years <- unique(stics_output$ian)
  
  # get all years that we want data from
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  # check that specified years and output years match
  if (!all(year_seq %in% simulation_years)) {
    PEcAn.logger::logger.severe("Years selected for model run and STICS output years do not match ")
  }
  
  # determine time step?
  
  for (y in simulation_years) {
    
    if (file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & overwrite == FALSE) {
      next
    }
    
    thisyear <- stics_output[ , "ian"] == y
    
    outlist <- list()
    outlist[[1]] <- stics_output[thisyear, "lai.n."]  # LAI in (m2 m-2)
    
    # ******************** Declare netCDF dimensions and variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                          units = paste0("days since ", y, "-01-01 00:00:00"), 
                          stics_output[stics_output[,1] == y, 4], # allow partial years, this info is already in matrix_weather
                          calendar = "standard", 
                          unlim = TRUE)
    
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",  vals = as.numeric(sitelon), longname = "station_longitude")
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var <- list()
    nc_var[[1]] <- PEcAn.utils::to_ncvar("LAI", dims)
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], outlist[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  } ### End of year loop

  
} # model2netcdf.STICS
