#-------------------------------------------------------------------------------
# Copyright (c) 2016 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' @name model2netcdf.FATES
##' @title Code to convert FATES netcdf output into into CF standard
##'
##' @param outdir Location of FATES model output (e.g. a path to a single ensemble output)
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param pfts a named vector of PFT numbers where the names are PFT names
##' @param settings pecan settings object
##' @param process_partial should failed runs be processed? Defaults to `FALSE`.
##'   `TRUE` will generate .nc files for runs that have generated some, but not
##'   all, of the expected outputs
##'
##' 
##' @examples  
##' \dontrun{
##' example.output <- system.file("case.clm2.h0.2004-01-01-00000.nc",package="PEcAn.FATES")
##' model2netcdf.FATES(outdir="~/")
##' }
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper, Toni Viskari, Istem Fer
## modified M. Dietze 07/08/12 modified S. Serbin 05/06/13
## refactored by Istem Fer on 03/2018
## further modified by S. Serbin 09/2018
##' 
##' @export

library(ncdf4.helpers) # dims names
model2netcdf.FATES <- function(outdir, sitelat, sitelon, pfts) {
  ## matched_var could be updated further to take in only oldnames users need.
  matched_var <- list(c("FATES_GPP_PF","GPP","kgC m-2 s-1","Gross Primary Productivity"), 
                      c("ER","TotalResp","kgC m-2 s-1","Total Respiration"),
                      c("NEE","NEE","kgC m-2 s-1", "Net Ecosystem Exchange of carbon, includes fire and hrv_xsmrpool"),
                      c("AR","AutoResp","kgC m-2 s-1","Autotrophic respiration (MR + GR)"),
                      c("HR","HeteroResp","kgC m-2 s-1","Total heterotrophic respiration"),
                      c("SR","SoilResp","kgC m-2 s-1","Total soil respiration (HR + root resp)"),
                      c("TLAI","LAI","m2 m-2","Total projected leaf area index"),
                      c("Qle","Evap","Evap","kgC m-2 s-1","Total evaporation"),
                      c("QVEGT","Transp","kg m-2 s-1","Canopy transpiration")) 

  var_update <- function(out,oldname,newname,newunits=NULL,long_name=NULL){
    if (oldname %in% nc_month_names) {
      ## define units of variables
      oldunits <- ncdf4::ncatt_get(nc_month,oldname,"units")$value
      if (oldunits=="gC/m^2/s") oldunits <- "gC m-2 s-1"
      if (oldname=="TLAI") oldunits <- "m2 m-2" # delete old unit ='none'
      if (is.null(newunits)) newunits = oldunits
      ## check pft dimensions
      if (any(grepl('pft',nc.get.dim.names(nc_month, oldname)))){
        dimension <- xypt # include fates_levpft
      }else{ 
        dimension <- xyt 
      } # only xyt

      ## transpose dimensions into (,t)
      if (nc.get.dim.names(nc_month,oldname)[length(nc.get.dim.names(nc_month,oldname))]=='time'){
            dat_0 <- ncdf4::ncvar_get(nc_month,oldname) # time at the tail of dims
            # dat.new <- PEcAn.utils::misc.convert(dat,oldunits,newunits) # convert data units
          }
      newvar <- ncdf4::ncvar_def(name = newname, units = newunits, longname=long_name, dim = dimension)
      ## Adding target variables into out 
      if(is.null(out)) {
        out <- list(var <- list(),dat <- list(), dimm<-list())
        out$var[[1]] <- newvar
        out$dat[[1]] <- dat_0 # dat.new
        out$dimm[[1]]<- length(dimension)
        # show(out$dat)
      } else {
        i <- length(out$var) + 1
        out$var[[i]] <- newvar
        out$dat[[i]] <- dat_0 # dat.new
        out$dimm[[i]]<- length(dimension)
        show(out$dat[[i]])
      }
    } 
     # PEcAn.logger::logger.info(paste0("HLM-FATES variable: ", oldname," not present. Skipping conversion"))
    return(out)
  }
    
  ## Get files and years
  files      <- dir(outdir, "*clm2.h0.*.nc", full.names = TRUE)  # currently specific to clm2.h0 files
  file.dates <- lubridate::parse_date_time(sub(".nc", "", sub(".*clm2.h0.", "", files)),"%y-%m", tz='UTC')
  years      <- lubridate::year(file.dates)
  init_year  <- years[1]

  ## Loop over years
  for (year in unique(years)) {
      ysel  <- which(year == years)
      fname <- files[ysel[1]] # FATES filename
      oname <- file.path(dirname(fname), paste0(year, ".nc")) # Pecan filename
     # PEcAn.logger::logger.info(paste("model2netcdf.FATES - Converting to:", oname))
      time_points_year <-c()
      out <- NULL
      for (month in ysel){
        month_file <- files[ysel[month]] 
        nc_month <- ncdf4::nc_open(month_file) # read monthly output file of FATES model
        nc_month_names <- names(nc_month$var)
        ## Create time bounds to populate time_bounds variable iteratively
        var_bound <- ncdf4::ncvar_get(nc_month, "time_bounds") # start,end day of month
        ## define dimensions 
        t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", init_year, "-01-01 00:00:00"),
                    vals = as.double(1.0:1.0), calendar = "noleap", unlim = TRUE)
        time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                    longname = "history time interval endpoint dimensions",vals = 1:2, units = "")
        lat  <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.double(1.0:1.0), longname = "coordinate_latitude")
        lon  <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.double(1.0:1.0), longname = "coordinate_longitude")
        pft  <- ncdf4::ncdim_def('pft', '', vals=1:12, longname = "FATES pft number")
        xyt  <- list(lon, lat, t)
        xypt <- list(lon, lat, pft, t)
        ## write monthly files with start(1,1,i)
        for (name_param in matched_var){
          out  <- var_update(out,name_param[1],name_param[2],name_param[3],name_param[4]) # convert monthly fates output into one variable
        }
        out$var[[length(out$var) + 1]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                          longname = "history time interval endpoints", dim=list(time_interval,t), prec = "double")
        out$dat[[length(out$dat) + 1]] <- c(rbind(var_bound[1], var_bound[2])) #start, end days of the year
        out$dimm[[length(out$dimm) + 1]] <- 2
        if(month==ysel[1]){
          ncout <- ncdf4::nc_create(oname,out$var) # create yearly nc file
          # HYC: define var time, lon, lat, and put var lon, lat
          time_var <- ncdf4::ncvar_def(name = "time", units = paste0("days since ", init_year, "-01-01 00:00:00"),longname = "time", dim = list(t), prec = "double")
          lat_var  <- ncdf4::ncvar_def(name = "lat", units = "degrees_north", longname = "coordinate_latitude", dim=list(lat), prec = "double")
          lon_var  <- ncdf4::ncvar_def(name = "lon", units = "degrees_east", longname = "coordinate_longitude", dim=list(lon), prec = "double")
          ncdf4::ncvar_put(ncout, lat_var, sitelat, start=c(1))
          ncdf4::ncvar_put(ncout, lon_var, sitelon, start=c(1))
        }
        # put time
        ncdf4::ncvar_put(ncout, time_var, mean(var_bound), start=c(month), count=c(1))

        for (i in seq_along(out$var)) {
          if(out$dimm[[i]]==4){ # xypt
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,1,1,month), count=c(1,1,12,1))
          }else if (out$dimm[[i]]==3) { # xyt
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,1,month))
          }else{ # time_bounds
            ncdf4::ncvar_put(ncout, out$var[[i]], out$dat[[i]], start=c(1,month))
          }
          
        } # write monthly converted variable into PEcAn output
        
      }
    ## extract variable and long names of the yearly file to VAR file for PEcAn vis
  #  utils::write.table(sapply(ncout$var, function(x) { x$longname }), 
  #              file = paste0(oname, ".var"), 
  #              col.names = FALSE, 
  #              row.names = TRUE, 
  #              quote = FALSE)
    try(ncdf4::nc_close(ncout))

    } # end of year for loop
} # model2netcdf.FATES
model2netcdf.FATES('/Users/mac/Documents/noresm-land-sites-platform/resources/cases/test_oneyear/archive/lnd/hist', 67.3623, 26.6385, 12)
