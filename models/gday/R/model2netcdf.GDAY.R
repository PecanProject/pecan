#-------------------------------------------------------------------------------
# Copyright (c) 2012 NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------#
##' Convert GDAY output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.GDAY
##' @title Function to convert GDAY model output to standard netCDF format
##' @param outdir Location of GDAY model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##' @author Martin De Kauwe
model2netcdf.GDAY <- function(outdir, sitelat, sitelon, start_date, end_date) {

require(PEcAn.utils)
require(ncdf4)

  G_2_KG <- 0.001
  TONNES_PER_HA_TO_G_M2 <- 100.0
  THA_2_KG_M2 <- TONNES_PER_HA_TO_G_M2 * 0.001

  ### Read in model output in GDAY format
  GDAY.output <- read.csv(file.path(outdir, "out.txt"), header=TRUE, sep=',',
                          skip=1)
  GDAY.output.dims <- dim(GDAY.output)

  ### Determine number of years and output timestep
  days = as.Date(start_date):as.Date(end_date)
  year = strftime(as.Date(days, origin="1970-01-01"), "%Y")
  num.years <- length(unique(year))
  years <- unique(year)
  timestep.s <- 86400

  ### Loop over years in GDAY output to create separate netCDF outputs
  for (y in years){
    if (file.exists(file.path(outdir, paste(y, "nc", sep=".")))) {
      next
    }

    ## Subset data for processing
    sub.GDAY.output <- subset(GDAY.output, year == y)
    sub.GDAY.output.dims <- dim(sub.GDAY.output)

    ## Setup outputs for netCDF file in appropriate units
    output <- list()

    ## standard variables: Fluxes
    output[[1]] <- (sub.GDAY.output[,"auto_resp"] * THA_2_KG_M2) / timestep.s
    output[[2]] <- (sub.GDAY.output[,"hetero_resp"] * THA_2_KG_M2) / timestep.s
    output[[3]] <- (sub.GDAY.output[,"gpp"] * THA_2_KG_M2) / timestep.s
    output[[4]] <- (sub.GDAY.output[,"nep"] * -1 * THA_2_KG_M2) / timestep.s
    output[[5]] <- (sub.GDAY.output[,"npp"] * THA_2_KG_M2) / timestep.s


    #******************** Declare netCDF variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:nrow(sub.GDAY.output),
                   calendar = "standard", unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude")
    lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")

   ## ***** Need to dynamically update the UTC offset here *****

    for(i in 1:length(output)){
      if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
    }

    var <- list()
    var[[1]]  <- mstmipvar("AutoResp", lat, lon, t, NA)
    var[[2]]  <- mstmipvar("HeteroResp", lat, lon, t, NA)
    var[[3]]  <- mstmipvar("GPP", lat, lon, t, NA)
    var[[4]]  <- mstmipvar("NEE", lat, lon, t, NA)
    var[[5]]  <- mstmipvar("NPP", lat, lon, t, NA)

    #var[[6]]  <- ncvar_def("LeafLitter", "kgC/m2/s", list(lon,lat,t), -999)
    #var[[7]]  <- ncvar_def("WoodyLitter", "kgC/m2/s", list(lon,lat,t), -999)
    #var[[8]]  <- ncvar_def("RootLitter", "kgC/m2/s", list(lon,lat,t), -999)
    #var[[9]]  <- ncvar_def("LeafBiomass", "kgC/m2", list(lon,lat,t), -999)
    #var[[10]]  <- ncvar_def("WoodBiomass", "kgC/m2", list(lon,lat,t), -999)
    #var[[11]]  <- ncvar_def("RootBiomass", "kgC/m2", list(lon,lat,t), -999)
    #var[[12]]  <- ncvar_def("LitterBiomass", "kgC/m2", list(lon,lat,t), -999)
    #var[[13]]  <- ncvar_def("SoilC", "kgC/m2", list(lon,lat,t), -999)

    #var[[14]]  <- mstmipvar("TotalResp", lat, lon, t, NA)
    #var[[15]]  <- mstmipvar("TotLivBiom", lat, lon, t, NA)
    #var[[16]]  <- mstmipvar("TotSoilCarb", lat, lon, t, NA)

    #******************** Declar netCDF variables ********************#


    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(y,"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep=".")), "w")
    for(i in 1:length(var)) {
      ncvar_put(nc, var[[i]], output[[i]])
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)

  } ### End of year loop
} ### End of function
#==============================================================================#


################################################################################
### EOF.  End of R script file.
################################################################################
