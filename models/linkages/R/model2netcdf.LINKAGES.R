#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
##'
##' @name model2netcdf.LINKAGES
##' @title Code to convert LINKAGES's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Ann Raiho
model2netcdf.LINKAGES <- function(outdir, sitelat, sitelon, start_date=NULL, end_date=NULL,force=FALSE){ #, PFTs) {
#  logger.severe("NOT IMPLEMENTED")

  ##QUESTIONS:How should I name the PFTs? What should I do about more than one plot?

  ### Read in model output in linkages format
  load(file.path(outdir,"linkages.out.Rdata"))
  #linkages.output.dims <- dim(linkages.output)

  ### Determine number of years and output timestep
  num.years <- nrow(ag.biomass)
  years <- unique(year)

  ### Loop over years in linkages output to create separate netCDF outputs
  for (y in years){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging

    ## Subset data for processing
    #sub.linkages.output <- subset(linkages.output, year == y)
    #sub.linkages.output.dims <- dim(sub.linkages.output)

    #sub.linkages.pft <- subset(linkages.pft, year == y)
    #sub.linkages.pft.dims <- dim(sub.linkages.pft)

    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables: Carbon Pools
    output[[1]] <- ag.biomass[y,] # Above Ground Biomass in kgC/m2
    output[[2]] <- ag.biomass[y,]  # Total Live Biomass in kgC/m2 (no distinction from AGB in linkages)
    output[[3]] <- total.soil.carbon[y,] # TotSoilCarb in kgC/m2
    output[[4]] <- c(ag.biomass[y,],total.soil.carbon[y,],
                     leaf.litter[y,]) #Carb Pools in kgC/m2
    output[[5]] <- c("AGB","Soil Organic Matter","Leaf Litter") #poolname
    output[[6]] <- ag.npp[y,] # GWBI = NPP in linkages
    output[[7]] <- hetero.resp[y,] # HeteroResp in kgC/m^2/s
    output[[8]] <- ag.npp[y,] # NPP = GWBI in linkages
    output[[9]] <- nee[y,] # NEE #possibly questionable
    output[[10]] <- et[y,] # Evap in kg/m^2/s

    output[[11]] <- agb.pft[,y,]

    output[[12]] <- f.comp[,y,]

    #******************** Declare netCDF variables ********************#
    dim.t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = as.numeric(years[y]),
                   calendar = "standard", unlim = TRUE)
    dim.lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude")
    dim.lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")
    dim.string <- ncdim_def("names", "", 1:24, create_dimvar=FALSE)
    dim.cpools <- ncdim_def("cpools", "",
                        vals = 1:3,
                        longname = "Carbon Pools")
    dim.cpools1 <- ncdim_def("cpools", "",
                         vals = 1:3,
                         longname = "Carbon Pools", create_dimvar=FALSE)
    dim.pfts <- ncdim_def("pfts", "",
                             vals = 1:nrow(agb.pft),
                             longname = "PFTs", create_dimvar=FALSE)

    for(i in 1:length(output)){
      if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
    }

    var <- list()
    var[[1]]  <- ncvar_def("AGB", "kgC/m2",list(dim.lat, dim.lon, dim.t),-999)
    var[[2]]  <- ncvar_def("TotLivBiomass", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[3]]  <- ncvar_def("TotSoilCarb", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[4]]  <- ncvar_def("CarbPools", "kgC/m2", list(dim.cpools, dim.lat, dim.lon, dim.t),-999)
    var[[5]]  <- ncvar_def("poolnames", units="", dim=list(dim.string, dim.cpools1), longname="Carbon Pool Names", prec="char")
    var[[6]]  <- ncvar_def("GWBI", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[7]]  <- ncvar_def("HeteroResp", "kgC/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    var[[8]]  <- ncvar_def("NPP", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[9]]  <- ncvar_def("NEE", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[10]]  <- ncvar_def("Evap", "kg/m2/s", list(dim.lat, dim.lon, dim.t), -999)

    var[[11]]  <- ncvar_def("AGB.pft", "kgC/m2",list(dim.pfts, dim.lat, dim.lon, dim.t),-999)
    var[[12]]  <- ncvar_def("Fcomp", "kgC/kgC",list(dim.pfts, dim.lat, dim.lon, dim.t),-999)

    #******************** Declar netCDF variables ********************#


    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(sprintf("%04d",years[y]),"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(years[y], "nc", "var", sep=".")), "w")
    for(i in 1:length(var)){
      print(i)
      ncvar_put(nc,var[[i]],output[[i]])
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)

  } ### End of year loop



}

