#-------------------------------------------------------------------------------
# Copyright (c) 2015 Boston University, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# NCSA Open Source License
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
##' @importFrom ncdf4 ncvar_def ncdim_def nc_create ncvar_put nc_close

model2netcdf.GDAY <- function(outdir, sitelat, sitelon, start_date, end_date) {
  G_2_KG <- 0.001
  TONNES_PER_HA_TO_G_M2 <- 100
  THA_2_KG_M2 <- TONNES_PER_HA_TO_G_M2 * 0.001

  ### Read in model output in GDAY format
  GDAY.output <- read.csv(file.path(outdir, "gday_out.csv"), header = TRUE, sep = ",", skip = 1)
  GDAY.output.dims <- dim(GDAY.output)

  ### Determine number of years and output timestep
  days <- as.Date(start_date):as.Date(end_date)
  year <- strftime(as.Date(days, origin = "1970-01-01"), "%Y")
  num.years <- length(unique(year))
  years <- unique(year)
  timestep.s <- 86400

  ### Loop over years in GDAY output to create separate netCDF outputs
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }

    ## Subset data for processing
    sub.GDAY.output <- subset(GDAY.output, year == y)
    sub.GDAY.output.dims <- dim(sub.GDAY.output)

    ## Setup outputs for netCDF file in appropriate units
    output <- list()

    ## standard variables: C-Fluxes
    output[[1]] <- (sub.GDAY.output[, "auto_resp"] * THA_2_KG_M2) / timestep.s
    output[[2]] <- (sub.GDAY.output[, "hetero_resp"] * THA_2_KG_M2) / timestep.s
    output[[3]] <- (sub.GDAY.output[, "auto_resp"] + sub.GDAY.output[, "hetero_resp"] *
      THA_2_KG_M2) / timestep.s
    output[[4]] <- (sub.GDAY.output[, "gpp"] * THA_2_KG_M2) / timestep.s
    output[[5]] <- (sub.GDAY.output[, "nep"] * -1 * THA_2_KG_M2) / timestep.s
    output[[6]] <- (sub.GDAY.output[, "npp"] * THA_2_KG_M2) / timestep.s

    ## standard variables: C-State
    output[[7]] <- (sub.GDAY.output[, "stem"] + sub.GDAY.output[, "branch"] * THA_2_KG_M2) / timestep.s
    output[[8]] <- (sub.GDAY.output[, "soilc"] * THA_2_KG_M2) / timestep.s
    output[[9]] <- (sub.GDAY.output[, "lai"])

    ## standard variables: water fluxes
    output[[10]] <- (sub.GDAY.output[, "et"]) / timestep.s
    output[[11]] <- (sub.GDAY.output[, "transpiration"]) / timestep.s

    # ******************** Declare netCDF variables ********************#
    t <- ncdim_def(
      name = "time", units = paste0("days since ", y, "-01-01 00:00:00"),
      vals = 1:nrow(sub.GDAY.output),
      calendar = "standard", unlim = TRUE
    )
    lat <- ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")

    ## ***** Need to dynamically update the UTC offset here *****

    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) {
        output[[i]] <- rep(-999, length(t$vals))
      }
    }

    dims <- list(lon = lon, lat = lat, time = t)

    var <- list()
    ## C-Fluxes
    var[[1]] <- PEcAn.utils::to_ncvar("AutoResp", dims)
    var[[2]] <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    var[[3]] <- PEcAn.utils::to_ncvar("TotalResp", dims)
    var[[4]] <- PEcAn.utils::to_ncvar("GPP", dims)
    var[[5]] <- PEcAn.utils::to_ncvar("NEE", dims)
    var[[6]] <- PEcAn.utils::to_ncvar("NPP", dims)

    ## C-State
    var[[7]] <- PEcAn.utils::to_ncvar("AbvGrndWood", dims)
    var[[8]] <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    var[[9]] <- PEcAn.utils::to_ncvar("LAI", dims)

    ## Water fluxes
    var[[10]] <- PEcAn.utils::to_ncvar("Evap", dims)
    var[[11]] <- PEcAn.utils::to_ncvar("TVeg", dims)

    # var[[6]]  <- PEcAn.utils::to_ncvar("LeafLitter", "kgC/m2/s", list(lon,lat,t), -999 )
    # var[[7]]  <- PEcAn.utils::to_ncvar("WoodyLitter", "kgC/m2/s", list(lon,lat,t), -999)
    # var[[8]]  <- PEcAn.utils::to_ncvar("RootLitter", "kgC/m2/s", list(lon,lat,t), -999)
    # var[[9]]  <- PEcAn.utils::to_ncvar("LeafBiomass", "kgC/m2", list(lon,lat,t), -999)
    # var[[10]]  <- PEcAn.utils::to_ncvar("WoodBiomass", "kgC/m2", list(lon,lat,t), -999)
    # var[[11]]  <- PEcAn.utils::to_ncvar("RootBiomass", "kgC/m2", list(lon,lat,t), -999)
    # var[[12]]  <- PEcAn.utils::to_ncvar("LitterBiomass", "kgC/m2", list(lon,lat,t), -999)
    # var[[13]]  <- PEcAn.utils::to_ncvar("SoilC", "kgC/m2", list(lon,lat,t), -999)

    # ******************** Declare netCDF variables ********************#

    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(y, "nc", sep = ".")), var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(var)) {
      ncvar_put(nc, var[[i]], output[[i]])
      cat(paste(var[[i]]$name, var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    nc_close(nc)
  } ### End of year loop
} # model2netcdf.GDAY
